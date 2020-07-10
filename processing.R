# File for BHF 4C initiative
# Process existing data submitted

# Loading packages =================================================
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(shinydashboardPlus)
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
library(patchwork)
library(DT)
library(data.table)
library(epitools)

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
hospital_data_difference <- calculate_pct_difference_hosp(hospital_data)

# For manuscript
manuscript_aggregate <- graph_data_difference

# Combining % difference to main dataframes
graph_data_difference <- graph_data_difference[ , -c(4,5)]
graph_data_difference$submission <- rep("pooled", nrow(graph_data_difference))
graph_data_difference$period <- rep("percentage", nrow(graph_data_difference))
names(graph_data_difference)[names(graph_data_difference) == "difference"] <- "number"
graph_data_difference <- graph_data_difference[ , c(1,2,6,5,3,4)]
graph_data <- rbind(graph_data, graph_data_difference)
graph_data[sapply(graph_data, is.infinite)] <- NA
graph_data$number[graph_data$type=="Total COVID admissions" & graph_data$period=="percentage"] <- NA

hospital_data_difference <- hospital_data_difference[ , -c(5,6)]
hospital_data_difference$submission <- rep("pooled", nrow(hospital_data_difference))
hospital_data_difference$period <- rep("percentage", nrow(hospital_data_difference))
names(hospital_data_difference)[names(hospital_data_difference) == "difference"] <- "number"
hospital_data_difference <- hospital_data_difference[ , c(1,2,7,6,3,4,5)]
hospital_data <- rbind(hospital_data, hospital_data_difference)
hospital_data[sapply(hospital_data, is.infinite)] <- NA
hospital_data$number[hospital_data$type=="Total COVID admissions" & hospital_data$period=="percentage"] <- NA


# Defining function for submitted data
extract_submission = function(x) {
  name <- basename(x)
  location <- gsub("\\.xlsx$","", name) # Identifier
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
  names(df1) <- c("type", "date", "current", "submission")
  df2 <- df_list[[2]]
  names(df2) <- c("type", "date", "past", "submission")
  df <- left_join(df1, df2, by=c("type", "date", "submission")) # Joining two dataframes into one
  df <- pivot_longer(df, cols=c('current', 'past'), 
                     names_to = 'period', values_to = 'number', values_drop_na = FALSE) # pivoting table
  df <- arrange(df, period, type, date) # Sorting by date
  df_rolling <- df %>% group_by(type, period) %>% 
    mutate(avg = rollmean(number, 4, na.pad = TRUE, align = "right"))
  df_rolling <- pivot_longer(df_rolling, cols = c('number', 'avg'), 
                             names_to = 'rolling', values_to = 'number', values_drop_na = FALSE)
  df_rolling$rolling  <- factor(df_rolling$rolling)
  levels(df_rolling$rolling)[levels(df_rolling$rolling) == 'number'] <- 'FALSE'
  levels(df_rolling$rolling)[levels(df_rolling$rolling) == 'avg'] <- 'TRUE'
  df_data <- arrange(df_rolling, period, rolling, type, date)
  df_data$location <- rep("Your data", nrow(df_data))
  df_data <- df_data[, c(1,2,7,4,5,6,3)]
  df_data$period[df_data$period=="current"] <- "2019-2020"
  df_data$period[df_data$period=="past"] <- "2018-2019"
  df_data$period <- factor(df_data$period,
                           levels = c("2019-2020", "2018-2019"),
                           ordered = TRUE)
  df_data <- rename_activity(df_data)
  df_data_difference <- calculate_pct_difference(df_data)
  df_data_difference <- df_data_difference[ , -c(4,5)]
  df_data_difference$submission <- rep("pooled", nrow(df_data_difference))
  df_data_difference$period <- rep("percentage", nrow(df_data_difference))
  names(df_data_difference)[names(df_data_difference) == "difference"] <- "number"
  df_data_difference <- df_data_difference[ , c(1,2,6,5,3,4)]
  df_data <- rbind(df_data, df_data_difference)
  df_data[sapply(df_data, is.infinite)] <- NA
  df_data$number[df_data$type=="Total COVID admissions" & df_data$period=="percentage"] <- NA
  df_data$location <- "Your data"
  df_data <- df_data[ , c(1,2,3,4,5,7,6)]
  df_data
}


# Defining dataframe for table ================================

table_data <- hospital_data %>% filter(rolling==FALSE) %>% select(., -c(rolling)) %>% filter(period!="percentage")
table_data <- mutate(table_data,
                     interval = case_when(date < as.Date("2020-01-31") ~ "Before 1st case",
                                          date >= as.Date("2020-01-31") & date < as.Date("2020-03-23") ~ "Between 1st case and lockdown",
                                          date >= as.Date("2020-03-23") ~ "After lockdown"))
table_data$interval <- factor(table_data$interval,
                              levels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                              labels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                              ordered = TRUE)

table_data_total <- table_data %>% select(period, type, interval, number) 

table_data_total <- table_data_total %>% group_by(period, type, interval) %>%
  summarise(aggregate = sum(number, na.rm=T)) %>% ungroup()

table_data_total$location <- "Total"
table_data_total <- table_data_total %>% select(location, period, type, interval, aggregate) %>% as.data.frame()

table_data <- table_data %>% group_by(location, period, type, interval) %>% 
  summarise(aggregate = sum(number, na.rm=T)) %>% ungroup()

table_data <- rbind(table_data_total, table_data)

rm(table_data_total)

table_data_current <- table_data %>% filter(period=="2019-2020")
table_data_past <- table_data %>% filter(period=="2018-2019")

table_data <- left_join(table_data_current, table_data_past, 
                        by=c("location", "type", "interval")) %>% ungroup()

table_data <- table_data %>% select(location, type, interval, aggregate.x, aggregate.y) %>% as.data.frame()

names(table_data) <- c("Hospital/Trust", "type", "interval", "current", "past")
table_data <- mutate(table_data,
                     pct = ((current-past)/past))

table_data$proportion <- binom.wilson(abs(table_data$current-table_data$past), table_data$past)$proportion * 100
table_data$pct_lb <- binom.wilson(abs(table_data$current-table_data$past), table_data$past)$lower * 100
table_data$pct_ub <- binom.wilson(abs(table_data$current-table_data$past), table_data$past)$upper * 100

# Inputting negative
manuscript_table <- table_data %>% mutate(
                          proportion = case_when(pct<0 & is.na(proportion)==FALSE ~ proportion * (-1),
                                                 TRUE ~ proportion),
                          pct_lb = case_when(pct<0 & is.na(pct_lb)==FALSE ~ pct_lb * (-1),
                                                 TRUE ~ pct_lb),
                          pct_ub = case_when(pct<0 & is.na(pct_ub)==FALSE ~ pct_ub * (-1),
                                                 TRUE ~ pct_ub),
                        )

table_data <- select(table_data, "Hospital/Trust", type, interval, pct)

table_data <- pivot_wider(table_data, names_from = interval, values_from = pct) %>% as.data.frame()

# Function for extracting data for table
extract_table <- function(x) {
  suppressWarnings(submission <- extract_submission(x))
  table_submitted <- submission  %>% filter(rolling==FALSE) %>% select(., -c(rolling)) %>% filter(period!="percentage")
  table_submitted <- mutate(table_submitted,
                            interval = case_when(date < as.Date("2020-01-31") ~ "Before 1st case",
                                                 date >= as.Date("2020-01-31") & date < as.Date("2020-03-23") ~ "Between 1st case and lockdown",
                                                 date >= as.Date("2020-03-23") ~ "After lockdown")) 
  table_submitted$interval <- factor(table_submitted$interval,
                                     levels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                                     labels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                                     ordered = TRUE)
  
  table_data <- hospital_data  %>% filter(rolling==FALSE) %>% select(., -c(rolling)) %>% filter(period!="percentage")
  table_data <- mutate(table_data,
                       interval = case_when(date < as.Date("2020-01-31") ~ "Before 1st case",
                                            date >= as.Date("2020-01-31") & date < as.Date("2020-03-23") ~ "Between 1st case and lockdown",
                                            date >= as.Date("2020-03-23") ~ "After lockdown")) 
  table_data$interval <- factor(table_data$interval,
                                levels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                                labels = c("Before 1st case", "Between 1st case and lockdown", "After lockdown"),
                                ordered = TRUE)
  table_data <- rbind(table_submitted, table_data)
  
  table_data_total <- table_data %>% select(period, type, interval, number) 
  
  table_data_total <- table_data_total %>% group_by(period, type, interval) %>%
    summarise(aggregate = sum(number, na.rm=T)) %>% ungroup()
  
  table_data_total$location <- "Total"
  table_data_total <- table_data_total %>% select(location, period, type, interval, aggregate) %>% as.data.frame()
  
  table_data <- table_data %>% group_by(location, period, type, interval) %>% 
    summarise(aggregate = sum(number, na.rm=T)) %>% ungroup()
  
  table_data <- rbind(table_data_total, table_data)
  
  rm(table_data_total)
  
  table_data_current <- table_data %>% filter(period=="2019-2020")
  table_data_past <- table_data %>% filter(period=="2018-2019")
  
  table_data <- left_join(table_data_current, table_data_past, 
                          by=c("location", "type", "interval")) %>% ungroup()
  
  table_data <- table_data %>% select(location, type, interval, aggregate.x, aggregate.y) %>% as.data.frame()
  
  names(table_data) <- c("Hospital/Trust", "type", "interval", "current", "past")
  table_data <- mutate(table_data,
                       pct = ((current-past)/past))
  
  table_data <- select(table_data, "Hospital/Trust", type, interval, pct)
  
  table_data <- pivot_wider(table_data, names_from = interval, values_from = pct) %>% as.data.frame()
  
  table_data
  
}


# Function to centre the cell values
rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('-')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"  
)
