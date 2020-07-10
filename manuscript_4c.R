# File for BHF 4C initiative
# Process existing data submitted

# Loading packages =================================================
library(forcats)
library(plyr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(zoo)
library(readxl)
library(openxlsx)
library(here)
library(data.table)
library(epitools)
library(knitr)
library(officer)


# Identify all existing excel files in the directory

# existing_files <- list.files(pattern = "*.xlsx")
existing_files <- list.files(pattern="\\.xlsx$")

# Defining function to extract data
extraction = function(x) {
  location <- gsub("\\.xlsx$","", x) # Identifier
  df_new <- read.xlsx(x, sheet="Weekly counts 2019-2020") # Latest data
  df_old <- read.xlsx(x, sheet="Weekly counts 2018-2019") # Previous data
  start <- which(df_new[,2] == "Total A&E visits") # Identifying row when data starts
  end <- which(df_new[,2] == "Aortic aneurysm repair") # Identifying row when data ends
  df1 <- df_new[c(start:end) , c(2, 5:32)] # Extracting latest data
  df2 <- df_old[c(start:end) , c(2, 5:32)] #Extracting previous data
  df2[df2=="Total ED visits"] <- "Total A&E visits" # Correcting name of row
  df_list <- list(df1, df2)
  df_list <- lapply(df_list, function(x){
    x[x=="*"] <- 2.5
    x[x=="< 5"] <- 2.5
    replacing_na <- x[rowSums(is.na(x)) < 28,]
    replacing_na[is.na(replacing_na)] <- 0 # replace NA with 0 if the whole row is not NA 
    x[rowSums(is.na(x)) < 28,] <- replacing_na 
    x <- pivot_longer(x, cols=-'X2', names_to = 'date', values_to = 'number', values_drop_na = FALSE) # pivoting table
    x$date <- substring(x$date, 2, end)
    x$date <- as.Date("2019-10-31") + 7 *(as.numeric(x$date)-5) # Converting time periods to mid-week date
    names(x) <- c("type", "date", "number")
    x$type <- factor(x$type, ordered = FALSE)
    x$number <- as.numeric(x$number)
    x$location <- rep(location, nrow(x))
    x <- as.data.frame(x)
    x
  })
  df1 <- df_list[[1]]
  names(df1) <- c("type", "date", "current", "location")
  df2 <- df_list[[2]]
  names(df2) <- c("type", "date", "past", "location")
  df <- left_join(df1, df2, by=c("type", "date", "location")) # Joining two dataframes into one
  df
}

# Extracting data from each file with dataframe name as file name
# Creating a list with all data

datalist = lapply(existing_files, extraction)

# Combining all dataframes in list to one dataframe
data <- bind_rows(datalist)

# Generating averages of each type of data ==================================================================
# Ignoring missing data
data_aggregate <- data %>% group_by(type, date) %>% summarise(current = mean(current, na.rm=T), 
                                                              past = mean(past, na.rm=T),
                                                              .groups = 'drop')

data_aggregate <- pivot_longer(data_aggregate, cols=c('current', 'past'), 
                               names_to = 'period', values_to = 'number', values_drop_na = FALSE) # pivoting table

data <- pivot_longer(data, cols=c('current', 'past'), 
                     names_to = 'period', values_to = 'number', values_drop_na = FALSE) # pivoting table

# Generate 4-week rolling average ==============================================================
# Aggregate dataframe
data_aggregate <- arrange(data_aggregate, period, type, date) # Sorting by date

data_aggregate_rolling <- data_aggregate %>% group_by(type, period) %>% 
  mutate(avg = rollmean(number, 4, na.pad = TRUE, align = "right"))

# Individual hospital dataframe
data <- arrange(data, period, type, date)

data_rolling <- data %>% group_by(location, type, period) %>% 
  mutate(avg = rollmean(number, 4, na.pad = TRUE, align = "right"))


# Organising dataframe for aggregate data =======================================================
graph_data <- pivot_longer(data_aggregate_rolling, cols = c('number', 'avg'), names_to = 'rolling', values_to = 'number', values_drop_na = FALSE)
graph_data$rolling  <- factor(graph_data$rolling)
levels(graph_data$rolling)[levels(graph_data$rolling) == 'number'] <- 'FALSE'
levels(graph_data$rolling)[levels(graph_data$rolling) == 'avg'] <- 'TRUE'
graph_data <- arrange(graph_data, period, rolling, type, date) # Sorting data
graph_data$submission <- rep("pooled", nrow(graph_data))

# Organising dataframe of hospitals ============================================================
hospital_data <- pivot_longer(data_rolling, cols = c('number', 'avg'), names_to = 'rolling', values_to = 'number', values_drop_na = FALSE)
hospital_data$rolling  <- factor(hospital_data$rolling)
levels(hospital_data$rolling)[levels(hospital_data$rolling) == 'number'] <- 'FALSE'
levels(hospital_data$rolling)[levels(hospital_data$rolling) == 'avg'] <- 'TRUE'
hospital_data <- arrange(hospital_data, period, rolling, type, date) # Sorting data
hospital_data$submission <- rep("submitted", nrow(hospital_data))

# Renaming groups
graph_data$period[graph_data$period=="current"] <- "2019-2020"
graph_data$period[graph_data$period=="past"] <- "2018-2019"
graph_data$period <- factor(graph_data$period,
                            levels = c("2019-2020", "2018-2019"),
                            ordered = TRUE)
hospital_data$period[hospital_data$period=="current"] <- "2019-2020"
hospital_data$period[hospital_data$period=="past"] <- "2018-2019"
hospital_data$period <- factor(hospital_data$period,
                               levels = c("2019-2020", "2018-2019"),
                               ordered = TRUE)

# Function to rename activity data
rename_activity = function(x) {
  y <- x
  # #######################################################
  # Collapsing cardiac resync and pacemaker ###############
  # Collapsing thrombolysis and thrombectomy ##############
  # #######################################################
  y$type <- revalue(y$type,
                    c("Total admissions" = "Total hospital admissions",
                      "Cardiac conditions" = "A&E attendance with cardiac conditions",
                      "Acute coronary syndromes" = "Admission with ACS",
                      "Heart failure" = "Admission with heart failure",
                      "Peripheral arterial disease" = "Admission with peripheral arterial disease",
                      "Percutaneous coronary intervention" = "PCI performed",
                      "Cardiac pacemaker procedures" = "Cardiac pacemaker and resynchronisation performed", # *
                      "Cardiac resynchronisation therapy" = "Cardiac pacemaker and resynchronisation performed", # *
                      "CABG" = "CABG performed",
                      "Cerebrovascular conditions" = "A&E attendance with cerebrovascular conditions",
                      "Acute stroke/TIA" = "Admission with acute stroke/TIA",
                      "Intravenous thrombolysis for acute ischaemic stroke" = "Stroke thrombolysis and thrombectomy performed", # *
                      "Mechanical thrombectomy for acute ischaemic stroke" = "Stroke thrombolysis and thrombectomy performed", # *
                      "Cerebral aneurysm coiling procedures" = "Cerebral aneurysm coiling procedures performed",
                      "Other vascular conditions" = "A&E attendance with  other vascular conditions",
                      "Aortic aneurysms" = "Admission with aortic aneurysms",
                      "DVT or PE" = "Admission with DVT or PE",
                      "Carotid endarterectomy / stenting" = "Carotid endarterectomy / stenting performed",
                      "Limb revascularisation, bypass or amputation" = "Limb revascularisation, bypass or amputation performed",
                      "Aortic aneurysm repair" = "Aortic aneurysm repair performed",
                      "Peripheral angioplasty" = "Peripheral angioplasty performed"
                    ))
  y <- aggregate(y$number, by=list(type=y$type, date=y$date, period=y$period, submission=y$submission, rolling=y$rolling), FUN=sum)
  x <- y
  names(x)[names(x)=="x"] <- "number"
  x
}

graph_data <- rename_activity(graph_data)


rename_activity_hosp = function(x) {
  y <- x
  # #######################################################
  # Collapsing cardiac resync and pacemaker ###############
  # Collapsing thrombolysis and thrombectomy ##############
  # #######################################################
  y$type <- revalue(y$type,
                    c("Total admissions" = "Total hospital admissions",
                      "Cardiac conditions" = "A&E attendance with cardiac conditions",
                      "Acute coronary syndromes" = "Admission with ACS",
                      "Heart failure" = "Admission with heart failure",
                      "Peripheral arterial disease" = "Admission with peripheral arterial disease",
                      "Percutaneous coronary intervention" = "PCI performed",
                      "Cardiac pacemaker procedures" = "Cardiac pacemaker and resyncrhonisation performed", # *
                      "Cardiac resynchronisation therapy" = "Cardiac pacemaker and resyncrhonisation performed", # *
                      "CABG" = "CABG performed",
                      "Cerebrovascular conditions" = "A&E attendance with cerebrovascular conditions",
                      "Acute stroke/TIA" = "Admission with acute stroke/TIA",
                      "Intravenous thrombolysis for acute ischaemic stroke" = "Stroke thrombolysis and thrombectomy performed", # *
                      "Mechanical thrombectomy for acute ischaemic stroke" = "Stroke thrombolysis and thrombectomy performed", # *
                      "Cerebral aneurysm coiling procedures" = "Cerebral aneurysm coiling procedures performed",
                      "Other vascular conditions" = "A&E attendance with  other vascular conditions",
                      "Aortic aneurysms" = "Admission with aortic aneurysms",
                      "DVT or PE" = "Admission with DVT or PE",
                      "Carotid endarterectomy / stenting" = "Carotid endarterectomy / stenting performed",
                      "Limb revascularisation, bypass or amputation" = "Limb revascularisation, bypass or amputation performed",
                      "Aortic aneurysm repair" = "Aortic aneurysm repair performed",
                      "Peripheral angioplasty" = "Peripheral angioplasty performed"
                    ))
  y <- aggregate(y$number, by=list(type=y$type, date=y$date, period=y$period, submission=y$submission, rolling=y$rolling, location=y$location), FUN=sum)
  x <- y
  names(x)[names(x)=="x"] <- "number"
  x
}

hospital_data <- rename_activity_hosp(hospital_data)


# Function for calculating % difference
calculate_pct_difference <- function(x) {
  a <- x %>% filter(period=="2019-2020") %>% as.data.frame()
  b <- x %>% filter(period=="2018-2019") %>% as.data.frame()
  c <- left_join(a, b, by=c("type", "date", "rolling"))
  c <- select(c, -contains("period")) %>% select(-contains("submission"))
  c$difference <- (c$number.x - c$number.y) / c$number.y * 100
  x <- c
  x
}

calculate_pct_difference_hosp <- function(x) {
  test <- x %>% filter(period=="2019-2020") %>% as.data.frame()
  test1<- x %>% filter(period=="2018-2019") %>% as.data.frame()
  test2 <- left_join(test, test1, by=c("type", "date", "rolling", "location"))
  test2 <- select(test2, -contains("period")) %>% select(-contains("submission"))
  test2$difference <- (test2$number.x - test2$number.y) / test2$number.y * 100
  x <- test2
  x
}



# Calculating % difference for existing dataframes=======================================
graph_data_difference <- calculate_pct_difference(graph_data)
graph_data_difference[sapply(graph_data_difference, is.infinite)] <- NA
graph_data_difference$difference[graph_data_difference$type=="Total COVID admissions"] <- NA
names(graph_data_difference)[names(graph_data_difference)=="difference"] <- "pct"
names(graph_data_difference)[names(graph_data_difference)=="number.x"] <- "current"
names(graph_data_difference)[names(graph_data_difference)=="number.y"] <- "past"

graph_data_difference <- graph_data_difference %>% filter(is.na(current)==FALSE & is.na(past)==FALSE)
graph_data_difference$current <- as.integer(graph_data_difference$current)
graph_data_difference$past <- as.integer(graph_data_difference$past)
graph_data_difference <- data.table(graph_data_difference)

graph_data_difference$lb_current <- apply(graph_data_difference[ , c(4)], 1, function(x){
                                        (poisson.test(x)$conf.int)[1]
                                      })
graph_data_difference$ub_current <- apply(graph_data_difference[ , c(4)], 1, function(x){
                                        (poisson.test(x)$conf.int)[2]
                                      })
graph_data_difference$lb_past <- apply(graph_data_difference[ , c(5)], 1, function(x){
                                        (poisson.test(x)$conf.int)[1]
                                      })
graph_data_difference$ub_past <- apply(graph_data_difference[ , c(5)], 1, function(x){
                                        (poisson.test(x)$conf.int)[2]
                                      })

hospital_data_difference <- calculate_pct_difference_hosp(hospital_data)
hospital_data_difference[sapply(hospital_data_difference, is.infinite)] <- NA
hospital_data_difference$difference[hospital_data_difference$type=="Total COVID admissions"] <- NA
names(hospital_data_difference)[names(hospital_data_difference)=="difference"] <- "pct"
names(hospital_data_difference)[names(hospital_data_difference)=="number.x"] <- "current"
names(hospital_data_difference)[names(hospital_data_difference)=="number.y"] <- "past"

# RENAME graph
graph <- as.data.frame(graph_data_difference)

hospital_data_difference <- hospital_data_difference %>% filter(is.na(current)==FALSE & is.na(past)==FALSE) %>% filter(type!="Total COVID admissions")
hospital_data_difference$current <- as.integer(hospital_data_difference$current)
hospital_data_difference$past <- as.integer(hospital_data_difference$past)
hospital_data_difference <- data.table(hospital_data_difference)

hospital_data_difference$lb_current <- apply(hospital_data_difference[ , c(5)], 1, function(x){
  (poisson.test(x)$conf.int)[1]
})
hospital_data_difference$ub_current <- apply(hospital_data_difference[ , c(5)], 1, function(x){
  (poisson.test(x)$conf.int)[2]
})
hospital_data_difference$lb_past <- apply(hospital_data_difference[ , c(6)], 1, function(x){
  (poisson.test(x)$conf.int)[1]
})
hospital_data_difference$ub_past <- apply(hospital_data_difference[ , c(6)], 1, function(x){
  (poisson.test(x)$conf.int)[2]
})
# RENAME hospitals
hospitals <- hospital_data_difference

# Generating number of excess
hospitals$difference <- as.integer(hospitals$current - hospitals$past)

# Generating pct 95% CI
hospitals$proportion <- binom.wilson(abs(hospitals$difference), hospitals$past)$proportion * 100
hospitals$pct_lb <- binom.wilson(abs(hospitals$difference), hospitals$past)$lower * 100
hospitals$pct_ub <- binom.wilson(abs(hospitals$difference), hospitals$past)$upper * 100

# Order of hospital statistics types
graph$type <- factor(graph$type,
                     levels = c("Total A&E visits",
                                "Total hospital admissions",
                                "Total COVID admissions",
                                "A&E attendance with cardiac conditions",
                                "Admission with ACS",
                                "Admission with heart failure",
                                "PCI performed",
                                "Cardiac pacemaker and resynchronisation performed",
                                "CABG performed",
                                "A&E attendance with cerebrovascular conditions",
                                "Admission with acute stroke/TIA",
                                "Stroke thrombolysis and thrombectomy performed",
                                "Carotid endarterectomy / stenting performed",
                                "Cerebral aneurysm coiling procedures performed",
                                "A&E attendance with  other vascular conditions",
                                "Admission with aortic aneurysms",
                                "Admission with peripheral arterial disease",
                                "Admission with DVT or PE",
                                "Limb revascularisation, bypass or amputation performed",
                                "Aortic aneurysm repair performed",
                                "Peripheral angioplasty performed"))


# Manuscript numbers ============================================================
# All type of activities numbers
hospitals %>% filter(rolling==FALSE) %>% group_by(type) %>% summarise(total = sum(current),
                                                                      tota_past = sum(past))

#Covid admission numbers
hospital_data %>% filter(type=="Total COVID admissions" & rolling==FALSE & period == "2019-2020") %>% summarise(covid = sum(number, na.rm=T))
covid <- hospital_data %>% filter(type=="Total COVID admissions" & rolling==FALSE & period == "2019-2020")
covid <- mutate(covid,
                interval = case_when(date < as.Date("2020-01-31") ~ "Before 1st case",
                                     date >= as.Date("2020-01-31") & date < as.Date("2020-03-23") ~ "Between 1st case and lockdown",
                                     date >= as.Date("2020-03-23") ~ "After lockdown"))
covid %>% group_by(interval) %>% summarise(covid = sum(number, na.rm=T))

# Calculating relative reduction during Christmas/New Year period
# Reduction occurs between 19 Dec and 09 Jan (4 weeks)
# Comparison with 4 weeks prior bewteen 21 Nov and 12 Dec
# Calculate using current data only
dip_before <- graph %>% filter(rolling==FALSE) %>% 
                filter(type=="Total hospital admissions" | type == "Total A&E visits") %>%
                filter(date > as.Date("2019-11-20") & date < as.Date("2019-12-13")) %>%
                select(type, date, current) %>% group_by(type) %>% 
                summarise(before = sum(current))
dip_after <- graph %>% filter(rolling==FALSE) %>% 
                filter(type=="Total hospital admissions" | type == "Total A&E visits") %>%
                filter(date > as.Date("2019-12-18") & date < as.Date("2020-01-10")) %>%
                select(type, date, current) %>% group_by(type) %>% 
                summarise(after = sum(current))
dip <- left_join(dip_before, dip_after, by="type")
dip <- mutate(dip,
              difference = before - after)
dip <- mutate(dip,
              pct = binom.wilson(difference, before)$proportion * 100,
              lb = binom.wilson(difference, before)$lower * 100,
              ub = binom.wilson(difference, before)$upper * 100)

dip_before <- graph %>% filter(rolling==FALSE) %>% 
  filter(type=="Total hospital admissions" | type == "Total A&E visits") %>%
  filter(date > as.Date("2019-11-20") & date < as.Date("2019-12-13")) %>%
  select(type, date, past) %>% group_by(type) %>% 
  summarise(before = sum(past))
dip_after <- graph %>% filter(rolling==FALSE) %>% 
  filter(type=="Total hospital admissions" | type == "Total A&E visits") %>%
  filter(date > as.Date("2019-12-18") & date < as.Date("2020-01-10")) %>%
  select(type, date, past) %>% group_by(type) %>% 
  summarise(after = sum(past))
dip <- left_join(dip_before, dip_after, by="type")
dip <- mutate(dip,
              difference = before - after)
dip <- mutate(dip,
              pct = binom.wilson(difference, before)$proportion * 100,
              lb = binom.wilson(difference, before)$lower * 100,
              ub = binom.wilson(difference, before)$upper * 100)

dip_before <- graph %>% filter(rolling==FALSE) %>% 
  filter(type=="Total hospital admissions" | type == "Total A&E visits") %>%
  filter(date > as.Date("2019-11-20") & date < as.Date("2019-12-13")) %>%
  select(type, date, current, past) %>% group_by(type) %>% 
  summarise(before = sum(past) + sum(current))
dip_after <- graph %>% filter(rolling==FALSE) %>% 
  filter(type=="Total hospital admissions" | type == "Total A&E visits") %>%
  filter(date > as.Date("2019-12-18") & date < as.Date("2020-01-10")) %>%
  select(type, date, current, past) %>% group_by(type) %>% 
  summarise(after = sum(past) + sum(current))
dip <- left_join(dip_before, dip_after, by="type")
dip <- mutate(dip,
              difference = before - after)
dip <- mutate(dip,
              pct = binom.wilson(difference, before)$proportion * 100,
              lb = binom.wilson(difference, before)$lower * 100,
              ub = binom.wilson(difference, before)$upper * 100)

# Generting RRs for manuscript
# These are RR for ED attendances and total hospital admissions
# Relative reduction compared with before 1st case of COVID
rr_overall <- rr_combine %>% filter(type=="Total A&E visits")

rr_overall <- rbind(as.matrix(rr_overall[1,c(5:7)]), as.matrix(rr_overall[1,c(2:4)])) %>% t() %>% as.data.frame()
rr_overall <- as.matrix(apply(rr_overall, 2, as.numeric))
riskratio(rr_overall)$measure %>% as.data.frame() # ED attendances

rr_overall <- rr_combine %>% filter(type=="Total hospital admissions")

rr_overall <- rbind(as.matrix(rr_overall[1,c(5:7)]), as.matrix(rr_overall[1,c(2:4)])) %>% t() %>% as.data.frame()
rr_overall <- as.matrix(apply(rr_overall, 2, as.numeric))
riskratio(rr_overall)$measure %>% as.data.frame() # Total hospital admissions


# Table 1 ======================================================================================
table1 <- graph %>% filter(rolling==FALSE)
table1 <- mutate(table1,
             interval = case_when(date < as.Date("2020-01-31") ~ "Before 1st case",
                                  date >= as.Date("2020-01-31") & date < as.Date("2020-03-23") ~ "Between 1st case and lockdown",
                                  date >= as.Date("2020-03-23") ~ "After lockdown"))
table1$interval <- factor(table1$interval,
                          levels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                          labels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                          ordered = TRUE)
table1 <- table1 %>% select(type, interval, current, past)
table1 <- table1 %>% group_by(type, interval) %>% summarise(current = sum(current, na.rm=T), 
                                                            past = sum(past, na.rm=T))
table1$difference <- table1$current - table1$past
table1$proportion <- binom.wilson(abs(table1$current-table1$past), table1$past)$proportion * 100
table1$pct_lb <- binom.wilson(abs(table1$current-table1$past), table1$past)$lower * 100
table1$pct_ub <- binom.wilson(abs(table1$current-table1$past), table1$past)$upper * 100

table1 <- table1 %>% mutate(
                      proportion = case_when(difference<0 & is.na(proportion)==FALSE ~ proportion * (-1),
                                             TRUE ~ proportion),
                      pct_lb = case_when(difference<0 & is.na(pct_lb)==FALSE ~ pct_lb * (-1),
                                         TRUE ~ pct_lb),
                      pct_ub = case_when(difference<0 & is.na(pct_ub)==FALSE ~ pct_ub * (-1),
                                         TRUE ~ pct_ub),
                    )
table1 <- table1 %>% select(type, interval, proportion, pct_lb, pct_ub)

table1 <- pivot_wider(table1, names_from = c("interval"), values_from = c("proportion", "pct_lb", "pct_ub"))
names(table1) <- c("type", "b4_prop", "between_prop", "after_prop", "b4_lb", "between_lb", "after_lb", "b4_ub", "between_ub", "after_ub")
table1 <- table1[, c(1,2,5,8,3,6,9,4,7,10)]
table1[, 2:10] <- apply(table1[ , 2:10], 2, function (x) round(x, 1))


kable(table1, "rst")

doc <- read_docx() %>%
  body_add_table(value = table1, style = "table_template") %>%
  body_end_section_landscape() 
print(doc, target = "table1.docx")


# # Collapsing into diagnosis, admission, and treatment
# No longer in use
# rr_services <- rr_combine %>% filter(type!="Total COVID admissions")
# rr_services$type <- revalue(rr_services$type,
#                             c("Admission with ACS" = "Cardiac admissions",
#                               "A&E attendance with cardiac conditions" = "Cardiac A&E attendances",
#                               "A&E attendance with cerebrovascular conditions" = "Cerebrovascular A&E attendances",
#                               "A&E attendance with  other vascular conditions" = "Other vascular A&E attendances",
#                               "Admission with acute stroke/TIA" = "Cerebrovascular admissions",
#                               "Admission with heart failure" = "Cardiac admissions",
#                               "Admission with peripheral arterial disease" = "Other vascular admissions",
#                               "PCI performed" = "Cardiac procedures/treatments",
#                               "Cardiac pacemaker and resynchronisation performed" = "Cardiac procedures/treatments", 
#                               "CABG performed" = "Cardiac procedures/treatments",
#                               "Stroke thrombolysis and thrombectomy performed" = "Cerebrovascular procedures/treatments", 
#                               "Cerebral aneurysm coiling procedures performed" = "Cerebrovascular procedures/treatments",
#                               "Admission with aortic aneurysms" = "Other vascular admissions",
#                               "Admission with DVT or PE" = "Other vascular admissions",
#                               "Carotid endarterectomy / stenting performed" = "Cerebrovascular procedures/treatments",
#                               "Limb revascularisation, bypass or amputation performed" = "Other vascular procedures/treatments",
#                               "Aortic aneurysm repair performed" = "Other vascular procedures/treatments",
#                               "Peripheral angioplasty performed" = "Other vascular procedures/treatments"
#                             ))
# rr_services <- rr_services %>% group_by(type) %>%
#                   summarise(now1 = sum(`Before 1st case.x`),
#                             now2 = sum(`Between 1st case and lockdown.x`),
#                             now3 = sum(`After lockdown.x`),
#                             past1 = sum(`Before 1st case.y`),
#                             past2 = sum(`Between 1st case and lockdown.y`),
#                             past3 = sum(`After lockdown.y`))
# 
# rr_services_result <- data.frame(b1 = as.numeric(), b1_lb = as.numeric(), b1_ub = as.numeric(), 
#                         b2 = as.numeric(), b2_lb = as.numeric(), b2_ub = as.numeric())
# 
# for (i in c(1:11)) {
#   test <- rbind(as.matrix(rr_services[i,c(5:7)]), as.matrix(rr_services[i,c(2:4)])) %>% t() %>% as.data.frame()
#   test <- as.matrix(sapply(test, as.numeric))
#   result <- riskratio(test)$measure %>% as.data.frame()
#   result <- cbind(result[2,], result[3,])
#   names(result) <- c("b1", "b1_lb", "b1_ub", "b2", "b2_lb", "b2_ub")
#   rr_services_result <- rbind(rr_services_result, result)
# }
# 
# rr_services_result <- cbind(rr_services$type, rr_services_result)
# names(rr_services_result)[names(rr_services_result)=="rr_services$type"] <- "type"
# 
# rr_services_result_rr <- rr_services_result %>% select(type, b1, b2) %>%
#   pivot_longer(cols = c("b1", "b2"), names_to = "period", values_to = "RR")
# 
# rr_services_result_lb <- rr_services_result %>% select(type, b1_lb, b2_lb) %>% 
#   filter(type!="Total COVID admissions") %>%
#   pivot_longer(cols = c("b1_lb", "b2_lb"), names_to = "period", values_to = "RR_lb")
# rr_services_result_lb$period[rr_services_result_lb$period=="b1_lb"] <- "b1"
# rr_services_result_lb$period[rr_services_result_lb$period=="b2_lb"] <- "b2"
# 
# 
# rr_services_result_ub <- rr_services_result %>% select(type, b1_ub, b2_ub) %>% 
#   filter(type!="Total COVID admissions") %>%
#   pivot_longer(cols = c("b1_ub", "b2_ub"), names_to = "period", values_to = "RR_ub")
# rr_services_result_ub$period[rr_services_result_ub$period=="b1_ub"] <- "b1"
# rr_services_result_ub$period[rr_services_result_ub$period=="b2_ub"] <- "b2"
# 
# rr_services_result_rr <- left_join(rr_services_result_rr, rr_services_result_lb, by=c("type", "period"))
# rr_services_result_rr <- left_join(rr_services_result_rr, rr_services_result_ub, by=c("type", "period"))
# rr_services_result <- rr_services_result_rr
# 
# rr_services_result$period[rr_services_result$period=="b1"] <- "Between 1st case and lockdown"
# rr_services_result$period[rr_services_result$period=="b2"] <- "After lockdown"
# rr_services_result$period <- factor(rr_services_result$period,
#                           levels=c("Between 1st case and lockdown",
#                                    "After lockdown"
#                           ),
#                           ordered = TRUE)
# 
# rr_services_result$type <- fct_rev(rr_services_result$type)
# rr_services_result$RR_lb <- paste0("(", as.character(format(round(rr_services_result$RR_lb, 2), nsmall = 2)), " -")
# rr_services_result$RR_ub <- paste0(as.character(format(round(rr_services_result$RR_ub, 2), nsmall = 2)), ")")
# 
# rr_services_heatmap <- ggplot(rr_services_result, aes(period, type)) + 
#   geom_tile(aes(fill=RR)) + 
#   geom_text(aes(label = format(round(RR, 2), nsmall = 2)),
#             position = position_nudge(x = -0.16)) + 
#   geom_text(aes(label = RR_lb),
#             position = position_nudge(x = 0)) +
#   geom_text(aes(label = RR_ub),
#             position = position_nudge(x = 0.16)) +
#   scale_fill_gradient(low="#eb0d05", high="#fcfce8") +
#   scale_x_discrete(expand=c(0,0), position = 'top') +
#   scale_y_discrete(expand=c(0,0)) +
#   theme(axis.text.x=element_text(face="bold", size=10),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.ticks.y=element_blank())
# 
# rr_services_heatmap


# Supplementary table 1

rr_result_table <- rr_result
rr_result_table[ ,c(2:7)] <- round(rr_result_table[ ,c(2:7)],digit=2)


doc <- read_docx() %>%
  body_add_table(value = rr_result_table, style = "table_template") %>%
  body_end_section_landscape() 
print(doc, target = "table2.docx")

# heatmap for all hospitals ===============================================================
hosp_heatmap_data <- hospital_data %>% filter(type!="Total COVID admissions") %>% filter(rolling==FALSE)


hosp_heatmap_data <- spread(hosp_heatmap_data, key = period, value = number) %>% as.data.frame()

hosp_heatmap_data <- mutate(hosp_heatmap_data,
             interval = case_when(date < as.Date("2020-01-31") ~ "Before 1st case",
                                  date >= as.Date("2020-01-31") & date < as.Date("2020-03-23") ~ "Between 1st case and lockdown",
                                  date >= as.Date("2020-03-23") ~ "After lockdown"))
hosp_heatmap_data$interval <- factor(hosp_heatmap_data$interval,
                      levels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                      labels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                      ordered = TRUE)
hosp_heatmap_data <- select(hosp_heatmap_data, type, location, `2019-2020`, `2018-2019`, interval)

hosp_heatmap_data$type <- revalue(hosp_heatmap_data$type,
                            c("Admission with ACS" = "Cardiac admissions",
                              "A&E attendance with cardiac conditions" = "Cardiac A&E attendances",
                              "A&E attendance with cerebrovascular conditions" = "Cerebrovascular A&E attendances",
                              "A&E attendance with  other vascular conditions" = "Other vascular A&E attendances",
                              "Admission with acute stroke/TIA" = "Cerebrovascular admissions",
                              "Admission with heart failure" = "Cardiac admissions",
                              "Admission with peripheral arterial disease" = "Other vascular admissions",
                              "PCI performed" = "Cardiac procedures/treatments",
                              "Cardiac pacemaker and resyncrhonisation performed" = "Cardiac procedures/treatments", 
                              "CABG performed" = "Cardiac procedures/treatments",
                              "Stroke thrombolysis and thrombectomy performed" = "Cerebrovascular procedures/treatments", 
                              "Cerebral aneurysm coiling procedures performed" = "Cerebrovascular procedures/treatments",
                              "Admission with aortic aneurysms" = "Other vascular admissions",
                              "Admission with DVT or PE" = "Other vascular admissions",
                              "Carotid endarterectomy / stenting performed" = "Cerebrovascular procedures/treatments",
                              "Limb revascularisation, bypass or amputation performed" = "Other vascular procedures/treatments",
                              "Aortic aneurysm repair performed" = "Other vascular procedures/treatments",
                              "Peripheral angioplasty performed" = "Other vascular procedures/treatments"
                            ))

hosp_heatmap_data <- hosp_heatmap_data %>% group_by(location, type, interval) %>%
  summarise(current = sum(`2019-2020`, na.rm =T),
            past = sum(`2018-2019`, na.rm =T))

hosp_heatmap_data$type <- factor(hosp_heatmap_data$type,
                                  levels=c("Total A&E visits",
                                           "Total hospital admissions",
                                           "Cardiac A&E attendances",
                                           "Cardiac admissions",
                                           "Cardiac procedures/treatments",
                                           "Cerebrovascular A&E attendances",
                                           "Cerebrovascular admissions",
                                           "Cerebrovascular procedures/treatments",
                                           "Other vascular A&E attendances",
                                           "Other vascular admissions",
                                           "Other vascular procedures/treatments"),
                                 ordered = TRUE)



hosp_heatmap_data_current <- hosp_heatmap_data %>% select(type, location, interval, current) 
hosp_heatmap_data_current <- spread(hosp_heatmap_data_current, key = interval, value = current) %>% as.data.frame()

hosp_heatmap_data_past <- hosp_heatmap_data %>% select(type, location, interval, past) 
hosp_heatmap_data_past <- spread(hosp_heatmap_data_past, key = interval, value = past) %>% as.data.frame()


hosp_heatmap_data_combine <- left_join(hosp_heatmap_data_current, hosp_heatmap_data_past, by=c("type", "location"))



################
##### Filtering non-NA ==========
###############
hosp_heatmap_data_filtered <- hosp_heatmap_data_combine %>% filter(is.na(`Before 1st case.x`)==FALSE) %>%
  filter(`Before 1st case.x`!=0)

hosp_rr_result <- data.frame(b1 = as.numeric(), b1_lb = as.numeric(), b1_ub = as.numeric(), 
                        b2 = as.numeric(), b2_lb = as.numeric(), b2_ub = as.numeric())

for (i in c(1:68)) {
  test <- rbind(as.matrix(hosp_heatmap_data_filtered[i,c(6:8)]), as.matrix(hosp_heatmap_data_filtered[i,c(3:5)])) %>% t() %>% as.data.frame()
  test <- as.matrix(sapply(test, as.numeric))
  result <- riskratio(test)$measure %>% as.data.frame()
  result <- cbind(result[2,], result[3,])
  names(result) <- c("b1", "b1_lb", "b1_ub", "b2", "b2_lb", "b2_ub")
  hosp_rr_result <- rbind(hosp_rr_result, result)
}

hosp_inter <- cbind(hosp_heatmap_data_filtered[ , c(1,2)], hosp_rr_result)

hosp_heatmap <- left_join(hosp_heatmap_data_combine, hosp_inter, by=c("type", "location"))

hosp_heatmap <- hosp_heatmap %>% mutate(b2 = ifelse(`After lockdown.x`==0, NA, b2),
                        b2_lb = ifelse(`After lockdown.x`==0, NA, b2_lb),
                        b2_ub = ifelse(`After lockdown.x`==0, NA, b2_ub))

# rr_services_result$type <- fct_rev(rr_services_result$type)
# rr_services_result$RR_lb <- paste0("(", as.character(format(round(rr_services_result$RR_lb, 2), nsmall = 2)), " -")
# rr_services_result$RR_ub <- paste0(as.character(format(round(rr_services_result$RR_ub, 2), nsmall = 2)), ")")

hosp_heatmap_b1 <- hosp_heatmap %>% select(type, location, b1)
hosp_heatmap_b2 <- hosp_heatmap %>% select(type, location, b2)
hosp_heatmap_b1 <- spread(hosp_heatmap_b1, key = location, value = b1, sep="_") %>% as.data.frame()
hosp_heatmap_b2 <- spread(hosp_heatmap_b2, key = location, value = b2, sep="_") %>% as.data.frame()

hosp_heatmap <- left_join(hosp_heatmap_b1, hosp_heatmap_b2, by="type")

hosp_heatmap <- pivot_longer(hosp_heatmap, cols=(-type), names_to = 'Hospital', values_to = 'RR', values_drop_na = FALSE)
hosp_heatmap$Hospital <- factor(hosp_heatmap$Hospital)
hosp_heatmap$Hospital <- revalue(hosp_heatmap$Hospital,
                              c("location_A.x" = "A2",
                                "location_A.y" = "A3",
                                "location_B.x" = "B2",
                                "location_B.y" = "B3",
                                "location_C.x" = "C2",
                                "location_C.y" = "C3",
                                "location_D.x" = "D2",
                                "location_D.y" = "D3",
                                "location_E.x" = "E2",
                                "location_E.y" = "E3",
                                "location_F.x" = "F2",
                                "location_F.y" = "F3",
                                "location_G.x" = "G2",
                                "location_G.y" = "G3",
                                "location_H.x" = "H2",
                                "location_H.y" = "H3",
                                "location_I.x" = "I2",
                                "location_I.y" = "I3"
                              ))


hosp_heatmap$RR_text <- as.character(format(round(hosp_heatmap$RR, 2), nsmall = 2))
hosp_heatmap$RR_text[hosp_heatmap$RR_text=="  NA"] <- "-"
hosp_heatmap$type <- fct_rev(hosp_heatmap$type)

hosp_services_heatmap <- ggplot(hosp_heatmap, aes(Hospital, type)) + 
  geom_tile(aes(fill=RR)) + 
  geom_text(aes(label = RR_text)) + 
  # geom_text(aes(label = RR_lb),
  #           position = position_nudge(x = 0)) +
  # geom_text(aes(label = RR_ub),
  #           position = position_nudge(x = 0.16)) +
  scale_fill_gradient(low="#c70700", high="#fffc99",
                      na.value = "#f2f2f2") +
  scale_x_discrete(expand=c(0,0), position = 'top') +
  scale_y_discrete(expand=c(0,0)) +
  theme(axis.text.x=element_text(face="bold", size=10),
        axis.title.x=element_text(face="bold", size=12,
                                  margin = margin(t = 3, r = 3, b = 3, l = 3)),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

hosp_services_heatmap



# Figure 1 ===========================================================================
figure1 <- graph %>% filter(rolling==FALSE) %>% filter(type=="Total hospital admissions" | type=="Total A&E visits" | type=="Total COVID admissions")
figure1.1 <- pivot_longer(figure1, cols=c('current', 'past'), 
                                names_to = 'period', values_to = 'number', values_drop_na = FALSE) # pivoting table 

figure1.2 <- pivot_longer(figure1, cols=c('lb_current', 'lb_past'), 
                        names_to = 'period', values_to = 'lb', values_drop_na = FALSE) # pivoting table
figure1.2$period[figure1.2$period=="lb_current"] <- "current"
figure1.2$period[figure1.2$period=="lb_past"] <- "past"
figure1.2 <- figure1.2 %>% select(type, date, period, lb)


figure1.3 <- pivot_longer(figure1, cols=c('ub_current', 'ub_past'), 
                        names_to = 'period', values_to = 'ub', values_drop_na = FALSE) # pivoting table
figure1.3$period[figure1.3$period=="ub_current"] <- "current"
figure1.3$period[figure1.3$period=="ub_past"] <- "past"
figure1.3 <- figure1.3 %>% select(type, date, period, ub)

figure1.4 <- left_join(figure1.1, figure1.2, by=c("type", "date", "period"))
figure1.4 <- left_join(figure1.4, figure1.3, by=c("type", "date", "period"))
figure1 <- figure1.4 %>% select(type, date, period, number, lb, ub)
figure1$period[figure1$period=="current"] <- "2019-2020"
figure1$period[figure1$period=="past"] <- "2018-2019"
figure1$period <- factor(figure1$period,
                            levels = c("2019-2020", "2018-2019"),
                            ordered = TRUE)
figure1$type <- revalue(figure1$type,
                        c("Total A&E visits" = "Total ED attendances"))

axis_name_size = 12
axis_label_size = 11
plot_title_size = 16

ggplot(figure1) +
  geom_vline(aes(xintercept=as.Date("2020-03-23")),
             linetype = "dashed", colour="#4f009e", size=1) +
  geom_vline(aes(xintercept=as.Date("2020-01-31")),
             linetype = "dashed", colour="#047d24", size=1) +
  geom_ribbon(aes(x=date, ymin=lb, ymax=ub, fill=type, linetype=period),
               alpha=0.1) +
  geom_line(aes(date, number, colour=type, linetype=period), size=1) +
  annotate("text", x=as.Date("2020-03-19"), 
           y=300,
           label="Lockdown",
           angle=90,
           size=4, hjust=0) +
  annotate("text", x=as.Date("2020-01-27"), 
           y=300,
           label="1st case",
           angle=90,
           size=4, hjust=0) +
  scale_y_continuous(name="Mean estimate",
                     minor_breaks = NULL) +
  scale_x_date(name="Date",
               breaks = "months",
               labels = date_format("%b"),
               minor_breaks=NULL) +
  scale_color_discrete(guide=guide_legend(nrow=2)) +
  scale_linetype_discrete(guide=guide_legend(nrow=2)) +
  labs(title="") +
  theme(
    panel.background = element_rect(fill="transparent", colour = NA),
    plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
    axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
    axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size=axis_label_size),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.key.size = unit(1, "cm"),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.text = element_text(size = axis_name_size),
    legend.position="bottom",
    legend.title = element_blank()
  )

# Figure 2 ===================================================================
figure2 <- hospitals %>% as.data.frame() %>% filter(rolling==FALSE) 

figure2 <- figure2 %>% filter(type == "Total hospital admissions" | type=="Total A&E visits") 

figure2 <- figure2 %>% group_by(location, type, date) %>% 
  summarise(current = sum(current, na.rm =T),
            past = sum(past, na.rm =T),
            .groups = 'drop')
figure2 <- figure2 %>% mutate(difference = current - past)

figure2$pct <- binom.wilson(abs(figure2$difference), figure2$past)$proportion * 100
figure2$pct_lb <- binom.wilson(abs(figure2$difference), figure2$past)$lower * 100
figure2$pct_ub <- binom.wilson(abs(figure2$difference), figure2$past)$upper * 100
figure2 <- as.data.frame(figure2)

figure2 <- figure2 %>% mutate(
  pct = case_when(difference<0 & is.na(pct)==FALSE ~ pct * (-1),
                  TRUE ~ pct),
  pct_lb = case_when(difference<0 & is.na(pct_lb)==FALSE ~ pct_lb * (-1),
                     TRUE ~ pct_lb),
  pct_ub = case_when(difference<0 & is.na(pct_ub)==FALSE ~ pct_ub * (-1),
                     TRUE ~ pct_ub),
)

figure2$type <- factor(figure2$type, 
                       levels = c("Total hospital admissions",
                                  "Total A&E visits"),
                       ordered = TRUE)

figure2$type <- revalue(figure2$type,
                        c("Total A&E visits" = "Total ED attendances"))

axis_name_size = 10
axis_label_size = 9
plot_title_size = 14

ggplot(figure2) +
  geom_hline(aes(yintercept=0),
             linetype = "solid", colour="#910a00", size=0.5) +
  geom_vline(aes(xintercept=as.Date("2020-01-31")),
             linetype = "dashed", colour="#047d24", size=1) +
  geom_ribbon(aes(x=date, ymin=pct_lb, ymax=pct_ub, fill=type),
              alpha=0.2) +
  geom_line(aes(date, pct, colour=type), size=1) +
  geom_vline(aes(xintercept=as.Date("2020-03-23")),
             linetype = "dashed", colour="#4f009e", size=1) +
  facet_wrap(~location, ncol = 4) +
  scale_y_continuous(minor_breaks = NULL) +
  ylab(expression(atop("Percentage change from", paste("2018-2019")))) +
  scale_x_date(name="Date",
               breaks = "months",
               labels = date_format("%b %y"),
               minor_breaks=NULL) +
  scale_color_discrete(aesthetics = c("colour", "fill"),
                       guide=guide_legend(nrow=1)) +
  labs(title="") +
  theme(
    panel.background = element_rect(fill="transparent", colour = NA),
    plot.margin = unit(c(0.5, 1, 0.1, 0.7), "cm"),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
    axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
    axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
    axis.text.x = element_text(angle = 90, hjust=0.85,vjust=0.4),
    axis.text = element_text(size=axis_label_size),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.key.size = unit(1, "cm"),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.text = element_text(size = axis_name_size),
    legend.position="bottom",
    legend.title = element_blank(),
    strip.background = element_rect(
      color="#2c3e50", fill="#2c3e50", size=1.5, linetype="solid"
    ),
    strip.text.x = element_text(
      size = 10, color = "white"
    )
  )


# Figure 3 ===================================================================
figure3 <- graph %>% filter(rolling==FALSE) 

figure3 <- figure3 %>% filter(type != "Total hospital admissions") %>%
  filter(type!= "Total COVID admissions") %>%
  filter(type!="Total A&E visits")

graph %>% group_by(type) %>% summarise(nrow = length(type))
figure3 %>% group_by(type) %>% summarise(nrow = length(type))

figure3$type <- revalue(figure3$type,
                  c("Admission with ACS" = "Cardiac admissions",
                    "A&E attendance with cardiac conditions" = "Cardiac ED attendances",
                    "A&E attendance with cerebrovascular conditions" = "Cerebrovascular ED attendances",
                    "A&E attendance with  other vascular conditions" = "Other vascular ED attendances",
                    "Admission with acute stroke/TIA" = "Cerebrovascular admissions",
                    "Admission with heart failure" = "Cardiac admissions",
                    "Admission with peripheral arterial disease" = "Other vascular admissions",
                    "PCI performed" = "Cardiac procedures/treatments",
                    "Cardiac pacemaker and resynchronisation performed" = "Cardiac procedures/treatments", 
                    "CABG performed" = "Cardiac procedures/treatments",
                    "Stroke thrombolysis and thrombectomy performed" = "Cerebrovascular procedures/treatments", 
                    "Cerebral aneurysm coiling procedures performed" = "Cerebrovascular procedures/treatments",
                    "Admission with aortic aneurysms" = "Other vascular admissions",
                    "Admission with DVT or PE" = "Other vascular admissions",
                    "Carotid endarterectomy / stenting performed" = "Cerebrovascular procedures/treatments",
                    "Limb revascularisation, bypass or amputation performed" = "Other vascular procedures/treatments",
                    "Aortic aneurysm repair performed" = "Other vascular procedures/treatments",
                    "Peripheral angioplasty performed" = "Other vascular procedures/treatments"
                  ))



figure3 <- figure3 %>% group_by(type, date) %>% 
                summarise(current = sum(current),
                         past = sum(past),
                         .groups = 'drop')
figure3 <- figure3 %>% mutate(difference = current - past)

figure3$pct <- binom.wilson(abs(figure3$difference), figure3$past)$proportion * 100
figure3$pct_lb <- binom.wilson(abs(figure3$difference), figure3$past)$lower * 100
figure3$pct_ub <- binom.wilson(abs(figure3$difference), figure3$past)$upper * 100
figure3 <- as.data.frame(figure3)

figure3 <- figure3 %>% mutate(
  pct = case_when(difference<0 & is.na(pct)==FALSE ~ pct * (-1),
                         TRUE ~ pct),
  pct_lb = case_when(difference<0 & is.na(pct_lb)==FALSE ~ pct_lb * (-1),
                     TRUE ~ pct_lb),
  pct_ub = case_when(difference<0 & is.na(pct_ub)==FALSE ~ pct_ub * (-1),
                     TRUE ~ pct_ub),
)




figure3$type <- factor(figure3$type, 
                       levels = c("Cardiac ED attendances",
                                  "Cardiac admissions",
                                  "Cardiac procedures/treatments",
                                  "Cerebrovascular ED attendances",
                                  "Cerebrovascular admissions",
                                  "Cerebrovascular procedures/treatments",
                                  "Other vascular ED attendances",
                                  "Other vascular admissions",
                                  "Other vascular procedures/treatments"),
                       ordered = TRUE)


axis_name_size = 10
axis_label_size = 9
plot_title_size = 14

ggplot(figure3) +
  geom_hline(aes(yintercept=0),
             linetype = "solid", colour="#910a00", size=0.5) +
  geom_vline(aes(xintercept=as.Date("2020-01-31")),
             linetype = "dashed", colour="#047d24", size=1) +
  geom_ribbon(aes(x=date, ymin=pct_lb, ymax=pct_ub, fill=type),
              alpha=0.2) +
  geom_line(aes(date, pct, colour=type), size=1) +
  geom_vline(aes(xintercept=as.Date("2020-03-23")),
             linetype = "dashed", colour="#4f009e", size=1) +
  facet_wrap(~type, ncol = 3) +
  scale_y_continuous(minor_breaks = NULL) +
  ylab(expression(atop("Percentage change from", paste("2018-2019")))) +
  scale_x_date(name="Date",
               breaks = "months",
               labels = date_format("%b %y"),
               minor_breaks=NULL) +
  scale_color_discrete(aesthetics = c("colour", "fill"),
                       guide=guide_legend(nrow=3)) +
  labs(title="") +
  theme(
    panel.background = element_rect(fill="transparent", colour = NA),
    plot.margin = unit(c(0.5, 1, 0.5, 0.7), "cm"),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
    axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
    axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size=axis_label_size),
    legend.position = 'none',
    # legend.key = element_rect(fill = "transparent", color = NA),
    # legend.key.size = unit(1, "cm"),
    # legend.background = element_rect(fill = "transparent", colour = NA),
    # legend.text = element_text(size = axis_name_size),
    # legend.position="bottom",
    # legend.title = element_blank(),
    strip.background = element_rect(
      color="#2c3e50", fill="#2c3e50", size=1.5, linetype="solid"
    ),
    strip.text.x = element_text(
      size = 10, color = "white"
    )
  )

# Figure 4 ===============================================================================
rr <- graph %>% filter(rolling==FALSE)
rr <- mutate(rr,
             interval = case_when(date < as.Date("2020-01-31") ~ "Before 1st case",
                                  date >= as.Date("2020-01-31") & date < as.Date("2020-03-23") ~ "Between 1st case and lockdown",
                                  date >= as.Date("2020-03-23") ~ "After lockdown"))
rr$interval <- factor(rr$interval,
                      levels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                      labels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                      ordered = TRUE)
rr <- rr %>% select(type, interval, current, past)
rr_current <- rr %>% group_by(type, interval) %>% summarise(current = sum(current, na.rm=T))
rr_current <- spread(rr_current, key = interval, value = current) %>% as.data.frame()
rr_past <- rr %>% group_by(type, interval) %>% summarise(past = sum(past, na.rm=T))
rr_past <- spread(rr_past, key = interval, value = past) %>% as.data.frame()

rr_combine <- left_join(rr_current, rr_past, by=c("type"))

rr_result <- data.frame(b1 = as.numeric(), b1_lb = as.numeric(), b1_ub = as.numeric(), 
                        b2 = as.numeric(), b2_lb = as.numeric(), b2_ub = as.numeric())

for (i in c(1:21)) {
  test <- rbind(as.matrix(rr_combine[i,c(5:7)]), as.matrix(rr_combine[i,c(2:4)])) %>% t() %>% as.data.frame()
  test <- as.matrix(sapply(test, as.numeric))
  result <- riskratio(test)$measure %>% as.data.frame()
  result <- cbind(result[2,], result[3,])
  names(result) <- c("b1", "b1_lb", "b1_ub", "b2", "b2_lb", "b2_ub")
  rr_result <- rbind(rr_result, result)
}


rr_result$type <- rr_combine$type
rr_result <- rr_result[, c(7, 1:6)]

rr_graph <- rr_result %>% select(type, b1, b2) %>% 
  filter(type!="Total COVID admissions") %>%
  pivot_longer(cols = c("b1", "b2"), names_to = "period", values_to = "RR")

rr_graph_lb <- rr_result %>% select(type, b1_lb, b2_lb) %>% 
  filter(type!="Total COVID admissions") %>%
  pivot_longer(cols = c("b1_lb", "b2_lb"), names_to = "period", values_to = "RR_lb")
rr_graph_lb$period[rr_graph_lb$period=="b1_lb"] <- "b1"
rr_graph_lb$period[rr_graph_lb$period=="b2_lb"] <- "b2"


rr_graph_ub <- rr_result %>% select(type, b1_ub, b2_ub) %>% 
  filter(type!="Total COVID admissions") %>%
  pivot_longer(cols = c("b1_ub", "b2_ub"), names_to = "period", values_to = "RR_ub")
rr_graph_ub$period[rr_graph_ub$period=="b1_ub"] <- "b1"
rr_graph_ub$period[rr_graph_ub$period=="b2_ub"] <- "b2"

rr_graph <- left_join(rr_graph, rr_graph_lb, by=c("type", "period"))
rr_graph <- left_join(rr_graph, rr_graph_ub, by=c("type", "period"))

rr_graph$period[rr_graph$period=="b1"] <- "Between 1st case and lockdown"
rr_graph$period[rr_graph$period=="b2"] <- "After lockdown"
rr_graph$period <- factor(rr_graph$period,
                          levels=c("Between 1st case and lockdown",
                                   "After lockdown"
                          ),
                          ordered = TRUE)

rr_heatmap_data <- rr_graph
rr_heatmap_data$type <- fct_rev(rr_heatmap_data$type)
rr_heatmap_data$RR_lb <- paste0("(", as.character(format(round(rr_heatmap_data$RR_lb, 2), nsmall = 2)), " -")
rr_heatmap_data$RR_ub <- paste0(as.character(format(round(rr_heatmap_data$RR_ub, 2), nsmall = 2)), ")")

rr_heatmap_data$type <- revalue(rr_heatmap_data$type,
                                c("Total A&E visits" = "Total ED visits",
                                  "A&E attendance with cardiac conditions" = "ED attendance with cardiac conditions",
                                  "A&E attendance with cerebrovascular conditions" = "ED attendance with cerebrovascular conditions",
                                  "A&E attendance with  other vascular conditions" = "ED attendance with  other vascular conditions"))

rr_heatmap <- ggplot(rr_heatmap_data, aes(period, type)) + 
  geom_tile(aes(fill=RR)) + 
  geom_text(aes(label = format(round(RR, 2), nsmall = 2)),
            position = position_nudge(x = -0.16)) + 
  geom_text(aes(label = RR_lb),
            position = position_nudge(x = 0)) +
  geom_text(aes(label = RR_ub),
            position = position_nudge(x = 0.16)) +
  scale_fill_gradient(low="#9513bd", high="#fcfce8") +
  scale_x_discrete(expand=c(0,0), position = 'top') +
  scale_y_discrete(expand=c(0,0)) +
  theme(axis.text.x=element_text(face="bold", size=10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())


rr_heatmap # Figure 4

