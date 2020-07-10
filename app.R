

# Procesing existing data ==========================================
source("processing.R", local=TRUE)

# Define UI for application that draws a histogram
ui <- navbarPage('4C initiative', theme = shinytheme('flatly'),
            
                 
                 # shinydashboard dependencies
                 header = tagList(
                   useShinydashboard()
                 ),

                 tabPanel("Overview",
                          fluidRow(
                            column(7, h2("Clinical Care for CVD in the COVID-19 emergency", style="padding:3px;")),
                            column(5, 
                                   div(img(src="hdruk_main_rgb_jpeg.jpg", height = 90, align="right")),
                                   div(img(src="BHF DSC logo 2.png", height = 90, align="right"))
                                   )
                          ),
                          fluidRow(
                            column(4, 
                                   box(status = "danger", title = "Submit data", solidHeader = T, width = 12, collapsible = TRUE, collapsed = TRUE,
                                       h3(strong("Submitting your data")),
                                       p("Download the data submission template ",
                                         downloadLink("dataRequest", "here"),
                                         ". ",
                                         "Your data will be presented in the graphs after submission and ",
                                         "will not be shared with a third party. ",
                                         "Please put your hospital/trust name in the file name. "),
                                       p("Thank you for contributing to this project."),
                                       p("If you do ", strong("not"), " wish your data to be stored, please indicate in the checkbox below ",
                                         strong("before"),
                                         " submitting your data.",
                                         " You can still use this online tool to visualise your data. If you submitted your data in error, please contact ",
                                         em("mpoon@ed.ac.uk"),
                                         "."),
                                       tags$hr(),
                                       checkboxInput("bin_data",
                                                     p("Do", strong("not"), "store my data"), FALSE),
                                       tags$hr(),
                                       fileInput('file1', 'Choose file to upload',
                                                 accept = c(
                                                   'text/xlsx',
                                                   '.xlsx'
                                                 ))
                                   ),
                                   box(status="primary", title = "Hospital statistics", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       checkboxGroupInput("hospital_type", label = p("Select hospital statistics"), 
                                                          choices = list("Total A&E visits" = "Total A&E visits",
                                                                         "Total hospital admissions" = "Total hospital admissions",
                                                                         "Total COVID admissions" = "Total COVID admissions"),
                                                          selected = "Total hospital admissions")
                                   ),
                                   box(status="warning", title = "Graphical options", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       checkboxInput("individual_hosp",
                                                     "Show individual hospitals/trusts", FALSE),
                                       checkboxInput("pct",
                                                     "Show percentage change", FALSE),
                                       checkboxInput("smoothing",
                                                     "Show 4-week average", FALSE),
                                       p("Select period (mid-week dates)"),
                                       sliderInput("slider",
                                                   label=NULL,
                                                   min = as.Date(min(data$date)),
                                                   max = as.Date(max(data$date)),
                                                   value = c(as.Date(min(data$date)), as.Date(max(data$date))),
                                                   step = 7,
                                                   timeFormat = "%d %b")
                                   )
                            ),
                            column(8, align="centre",
                                   plotOutput("overviewPlot", height="78vh")),
                            
                          )),
                 
                 
                 # Cardiovascular statistics tab =======================================================================
                 # ################################################
                 # ################################################
                 tabPanel("Cardiac",
                          fluidRow(
                            column(7, h2("Clinical Care for CVD in the COVID-19 emergency", style="padding:3px;")),
                            column(5, 
                                   div(img(src="hdruk_main_rgb_jpeg.jpg", height = 90, align="right")),
                                   div(img(src="BHF DSC logo 2.png", height = 90, align="right"))
                            )                
                          ),
                          fluidRow(
                            column(4,
                                   box(status="success", title = "Info", solidHeader = T, width = 12, collapsible = TRUE, collapsed = TRUE,
                                   p(strong("Definitions")),
                                   p("A&E attendance with cardiac conditions: ",
                                     a("A&E diagnosis code", href="https://www.datadictionary.nhs.uk/web_site_content/supporting_information/clinical_coding/accident_and_emergency_diagnosis_tables.asp?shownav=1", target="_blank"),
                                     " 20"),
                                   p("Admission with ACS: ",
                                     a("ICD-10 codes", href="https://icd.who.int/browse10/2019/en#/IX", target="_blank"),
                                     " I21-24"),
                                   p("Admission with heart failure: ",
                                     a("ICD-10 codes", href="https://icd.who.int/browse10/2019/en#/IX", target="_blank"),
                                     " I50"),
                                   p("PCI performed: ",
                                     a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                     " K49, K50, K75"),
                                   p("Cardiac pacemaker & resynchronisation: ",
                                     a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                     " K59-60"),
                                   p("CABG performed: ",
                                     a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                     " K40-44")
                                   ),
                                   box(status="primary", title = "Cardiovascular statistics", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       checkboxGroupInput("cardio_type_cardio", label = p("Cardiovascular statistics"), 
                                                          choices = list("A&E attendance with cardiac conditions" = "A&E attendance with cardiac conditions",
                                                                         "Admission with ACS" = "Admission with ACS",
                                                                         "Admission with heart failure" = "Admission with heart failure",
                                                                         "PCI performed" = "PCI performed",
                                                                         "Cardiac pacemaker and resynchronisation performed" = "Cardiac pacemaker and resynchronisation performed",
                                                                         "CABG performed" = "CABG performed"),
                                                          selected="Admission with ACS"
                                       )
                                   ),
                                   box(status="warning", title = "Graphical options", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       checkboxInput("individual_hosp_cardio",
                                                     "Show individual hospitals/trusts", FALSE),
                                       checkboxInput("pct_cardio",
                                                     "Show percentage change", FALSE),
                                       checkboxInput("smoothing_cardio",
                                                     "4-week average", FALSE),
                                       p("Select period (mid-week dates)"),
                                       sliderInput("slider_cardio",
                                                   label=NULL,
                                                   min = as.Date(min(data$date)),
                                                   max = as.Date(max(data$date)),
                                                   value = c(as.Date(min(data$date)), as.Date(max(data$date))),
                                                   step = 7,
                                                   timeFormat = "%d %b")
                                   )
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error{visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            column(8, align="centre",
                                   plotOutput("cardioPlot", height="78vh")),
                            
                          )
                 ),
                 
                 
                 # Cerebrovascular statistics tab =======================================================================
                 # ################################################
                 # ################################################
                 tabPanel("Cerebrovascular",
                          fluidRow(
                            column(7, h2("Clinical Care for CVD in the COVID-19 emergency", style="padding:3px;")),
                            column(5, 
                                   div(img(src="hdruk_main_rgb_jpeg.jpg", height = 90, align="right")),
                                   div(img(src="BHF DSC logo 2.png", height = 90, align="right"))
                            )                
                          ),
                          fluidRow(
                            column(4,
                                   box(status="success", title = "Info", solidHeader = T, width = 12, collapsible = TRUE, collapsed = TRUE,
                                       p(strong("Definitions")),
                                         p("A&E attendance with cerebrovascular conditions: ",
                                         a("A&E diagnosis code", href="https://www.datadictionary.nhs.uk/web_site_content/supporting_information/clinical_coding/accident_and_emergency_diagnosis_tables.asp?shownav=1", target="_blank"),
                                          " 21"),
                                         p("Admission with acute stroke/TIA: ",
                                           a("ICD-10 codes", href="https://icd.who.int/browse10/2019/en#/IX", target="_blank"),
                                           " I60-61, I63-64, G45"),
                                         p("Stroke thrombolysis and thrombectomy : ",
                                           a("OPCS-4 code", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                           " X83.3 and ",
                                           a("ICD-10 code", href=" https://icd.who.int/browse10/2019/en#/IX", target="_blank"),
                                           " I63 for IV thrombolysis; ",
                                           a("OPCS-4 code", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                           " (v.4.9) 35.4 or combination of ",
                                           a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                           " (v4.8) L71.2 + Y53 + Z35 + ",
                                           a("ICD-10 code", href="https://icd.who.int/browse10/2019/en#/IX", target="_blank"),
                                           " I63 for thrombectomy"),
                                         p("Cerebral aneurysm coiling: ",
                                           a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                           " O01-04 + Y53 + Z53")
                                         ),
                                   box(status="primary", title = "Cerebrovascular statistics", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       checkboxGroupInput("cerebro_type_cerebral", label = p("Cerebrovascular statistics"), 
                                                          choices = list("A&E attendance with cerebrovascular conditions" = "A&E attendance with cerebrovascular conditions",
                                                                         "Admission with acute stroke/TIA" = "Admission with acute stroke/TIA",
                                                                         "Stroke thrombolysis and thrombectomy performed" = "Stroke thrombolysis and thrombectomy performed",
                                                                         "Cerebral aneurysm coiling procedures performed" = "Cerebral aneurysm coiling procedures performed"
                                                          ),
                                                          selected = "Admission with acute stroke/TIA"
                                       )
                                   ),
                                   box(status="warning", title = "Graphical options", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       checkboxInput("individual_hosp_cerebral",
                                                     "Show individual hospitals/trusts", FALSE),
                                       checkboxInput("pct_cerebral",
                                                     "Show percentage change", FALSE),
                                       checkboxInput("smoothing_cerebral",
                                                     "Show 4-week average", FALSE),
                                       p("Select period (mid-week dates)"),
                                       sliderInput("slider_cerebral",
                                                   label=NULL,
                                                   min = as.Date(min(data$date)),
                                                   max = as.Date(max(data$date)),
                                                   value = c(as.Date(min(data$date)), as.Date(max(data$date))),
                                                   step = 7,
                                                   timeFormat = "%d %b")
                                   )
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error{visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            column(8, align="centre",
                                   plotOutput("cerebralPlot", height="78vh")),
                            
                          )
                 ),

                 
                 # Other vascular statistics tab =======================================================================
                 # ################################################
                 # ################################################
                 tabPanel("Other vascular",
                          fluidRow(
                            column(7, h2("Clinical Care for CVD in the COVID-19 emergency", style="padding:3px;")),
                            column(5, 
                                   div(img(src="hdruk_main_rgb_jpeg.jpg", height = 90, align="right")),
                                   div(img(src="BHF DSC logo 2.png", height = 90, align="right"))
                            )                
                          ),
                          fluidRow(
                            column(4,
                                   box(status="success", title = "Info", solidHeader = T, width = 12, collapsible = TRUE, collapsed = TRUE,
                                       p(strong("Definitions")),
                                       p("A&E attendance with vascular conditions: ",
                                         a("A&E diagnosis code", href="https://www.datadictionary.nhs.uk/web_site_content/supporting_information/clinical_coding/accident_and_emergency_diagnosis_tables.asp?shownav=1", target="_blank"),
                                         " 22"),
                                       p("Admission with aortic aneurysms: ",
                                         a("ICD-10 code", href="https://icd.who.int/browse10/2019/en#/IX", target="_blank"),
                                         " I71"),
                                       p("Admission with DVT or PE : ",
                                         a("ICD-10 codes", href="https://icd.who.int/browse10/2019/en#/IX", target="_blank")," I26, I80"),
                                       p("Carotid endarterectomy / stenting: ",
                                         a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                         " L29.4, L29.5, L31.4"),
                                       p("Limb revascularisation, bypass or amputation: ",
                                         a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                         " L48-54, L56-63, L16-65, X09-11"),
                                       p("Aortic aneurysm repair: ",
                                         a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                         " L18-23, L25-28"),
                                       p("Peripheral angioplasty: ",
                                         a("OPCS-4 codes", href="https://classbrowser.nhs.uk/#/", target="_blank"),
                                         " L54-71")
                                   ),
                                   box(status="primary", title = "Vascular statistics", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       checkboxGroupInput("vascular_type_vasc", label = p("Other vascular statistics"), 
                                                          choices = list("A&E attendance with  other vascular conditions" = "A&E attendance with  other vascular conditions",
                                                                         "Admission with aortic aneurysms" = "Admission with aortic aneurysms",
                                                                         "Admission with peripheral arterial disease" = "Admission with peripheral arterial disease",
                                                                         "Admission with DVT or PE" = "Admission with DVT or PE",
                                                                         "Carotid endarterectomy / stenting performed" = "Carotid endarterectomy / stenting performed",
                                                                         "Limb revascularisation, bypass or amputation performed" = "Limb revascularisation, bypass or amputation performed",
                                                                         "Aortic aneurysm repair performed" = "Aortic aneurysm repair performed",
                                                                         "Peripheral angioplasty performed" = "Peripheral angioplasty performed"),
                                                          selected = "Admission with DVT or PE"
                                       )
                                   ),                                   
                                   box(status="warning", title = "Graphical options", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       checkboxInput("individual_hosp_vasc",
                                                     "Show individual hospitals/trusts", FALSE),
                                       checkboxInput("pct_vasc",
                                                     "Show percentage change", FALSE),
                                       checkboxInput("smoothing_vasc",
                                                     "4-week average", FALSE),
                                       p("Select period (mid-week dates)"),
                                       sliderInput("slider_vasc",
                                                   label=NULL,
                                                   min = as.Date(min(data$date)),
                                                   max = as.Date(max(data$date)),
                                                   value = c(as.Date(min(data$date)), as.Date(max(data$date))),
                                                   step = 7,
                                                   timeFormat = "%d %b")
                                   )
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error{visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            column(8, align="centre",
                                   plotOutput("vascPlot", height="78vh")),
                            
                          )
                 ),
                 
                 # Tabulated data tab =======================================================================
                 # ################################################
                 # ################################################
                 tabPanel("Tabulate",
                          fluidRow(
                            column(7, h2("Clinical Care for CVD in the COVID-19 emergency", style="padding:3px;")),
                            column(5, 
                                   div(img(src="hdruk_main_rgb_jpeg.jpg", height = 90, align="right")),
                                   div(img(src="BHF DSC logo 2.png", height = 90, align="right"))
                            )                
                          ),
                          fluidRow(
                            column(4,
                                   box(status="warning", title = "Options", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       selectInput("options", "Select hospital activity", choices = list(
                                                   `Hospital` = list("Total A&E visits", "Total hospital admissions"),
                                                   `Cardiac` = list("A&E attendance with cardiac conditions", "Admission with ACS", "Admission with heart failure",
                                                                      "PCI performed", "Cardiac pacemaker and resynchronisation performed", "CABG performed"),
                                                   `Cerebrovascular` = list("A&E attendance with cerebrovascular conditions", "Admission with acute stroke/TIA",
                                                                       "Stroke thrombolysis and thrombectomy performed", "Carotid endarterectomy / stenting performed",
                                                                       "Cerebral aneurysm coiling procedures performed"),
                                                   `Vascular` = list("A&E attendance with  other vascular conditions", "Admission with aortic aneurysms",
                                                                     "Admission with peripheral arterial disease" = "Admission with peripheral arterial disease",
                                                                "Admission with DVT or PE",
                                                                "Limb revascularisation, bypass or amputation performed", "Aortic aneurysm repair performed",
                                                                "Peripheral angioplasty performed"))
                                       , selectize = FALSE, selected = "Total hospital admissions"),
                                       hr(),
                                       p("Note that some hospital activities have low numbers witin individual hospitals. ",
                                         "Small absolute change in these numbers can give large percentage changes.",
                                         "Please consider this when interpreting this table. ",
                                         "For data governance, we do not provide raw counts.")
                                   )
                            ),
                            column(8, align="centre",
                                   dataTableOutput("table1")),
                            
                          )
                 ),
                 
                 # About page =====================================================================
                 tabPanel("About",
                          fluidRow(
                            column(7, h2("Clinical Care for CVD in the COVID-19 emergency", style="padding:3px;")),
                            column(5, 
                                   div(img(src="hdruk_main_rgb_jpeg.jpg", height = 90, align="right")),
                                   div(img(src="BHF DSC logo 2.png", height = 90, align="right"))
                            )
                          ),
                          fluidRow(
                            column(4,
                                   box(status="success", title = "Authors", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       h4("4C Initiative of the CVD-COVID-UK consortium"),
                                       p(strong("Authors listed alphabetically:"),
                                         "S Ball, A Banerjee, C Berry, J Boyle, B Bray, W Bradlow, A Chaudhry, R Crawley, ",
                                         "J Danesh, A Denniston, F Falter, JD Figueroa, C Hall, H Hemingway, E Jefferson, ",
                                         "T Johnson, G King, K Lee, P McKean, SM Mason, N Mills, E Pearson, M Pirmohamed, ",
                                         "MTC Poon, R Priedon, A Shah, R Sofat, J Sterne, F Strachan, CLM Sudlow, Z Szarka, ",
                                         "W Whiteley, M Wyatt")
                                   ),
                                   box(status="primary", title = "App development", solidHeader = T, width = 12, collapsible = TRUE, collapsed = FALSE,
                                       p("This web application is developed by ",
                                         a("Michael Poon", href = "https://www.research.ed.ac.uk/portal/en/persons/michael-poon(1c64fb66-b332-44cf-bc0e-0ba4110b5ef4).html", target="_blank"),
                                         ". Source code is available on ",
                                         a("GitHub", href = "", target="_blank"),
                                         ". For queries about this web application or suggestions for additional functionalities, please contact ",
                                         em("mpoon@ed.ac.uk"),
                                         "."),
                                       a(img(src="twitter.png", height = 40), href="https://twitter.com/MchaelPoon", target="_blank")
                                   )
                            ),
                            column(8, 
                                   box(status="danger", title = "Abstract", solidHeader = T, width = 12, collapsible = FALSE,
                                       h3("The 4C Initiative (Clinical Care for Cardiovascular disease in the COVID-19 pandemic)"),
                                       h4("Monitoring the indirect impact of the coronavirus pandemic on services for cardiovascular diseases in the UK"),
                                       br(),
                                       p(strong("Background:"),
                                       " The coronavirus (COVID-19) pandemic affects cardiovascular diseases (CVDs) directly through infection ",
                                       "and indirectly through health service reorganisation and public health policy. Real-time data are needed ",
                                       "to quantify direct and indirect effects. We aimed to monitor hospital activity for presentation, diagnosis ",
                                       "and treatment of CVDs during the pandemic to inform on indirect effects."),
                                       p(strong("Methods:"),
                                       "We analysed aggregate data on presentations, diagnoses and treatments or procedures for selected CVDs ",
                                       "(acute coronary syndromes, heart failure, stroke and transient ischaemic attack, venous thromboembolism, ",
                                       "peripheral arterial disease and aortic aneurysm) in UK hospitals before and during the COVID-19 epidemic. ",
                                       "We produced an online visualisation tool to enable near real-time monitoring of trends."),
                                       p(strong("Findings:"),
                                       "Nine hospitals across England and Scotland contributed hospital activity data from 28 Oct 2019 (pre-COVID-19) ",
                                       "to 10 May 2020 (pre-easing of lockdown), and for the same weeks during 2018-2019. Across all hospitals, total admissions ",
                                       "and emergency department (ED) attendances decreased after lockdown (23 March 2020) by 57.9% (57.1-58.6%) and 52.9% (52.2-53.5%) ",
                                       "respectively compared with the previous year. Activity for cardiac, cerebrovascular and other vascular conditions started to decline ",
                                       "1-2 weeks before lockdown, and fell by 31-88% after lockdown, with the greatest reductions observed for coronary artery bypass grafts, ",
                                       "carotid endarterectomy, aortic aneurysm repair and peripheral arterial disease procedures. Compared with before the first UK COVID-19 ",
                                       "(31 January 2020), activity declined across diseases and specialties between the first case and lockdown ",
                                       "(total ED attendances RR 0.94, 0.93-0.95; total hospital admissions RR 0.96, 0.95-0.97) and after lockdown ",
                                       "(attendances RR 0.63, 0.62-0.64; admissions RR 0.59, 0.57-0.60). There was limited recovery towards usual levels of some activities ",
                                       "from mid-April 2020."),
                                       p(strong("Interpretation:"),
                                         "Substantial reductions in total and cardiovascular activities are likely to contribute to a major burden of indirect effects of ",
                                         "the pandemic, suggesting they should be monitored and mitigated urgently."),
                                       hr(),
                                       p("Preprint will be available shortly."))),
                          )
                 )
                 
                 
                 # End of UI ===========================================================================================
)


# Define server logic  =================================================
server <- function(input, output) {
  
    output$dataRequest <- downloadHandler(
      filename = "4C data request.xlsx",
      content = function(file) {
        file.copy("data/4C data request.xlsx", file)
      }
    )
    
    axis_name_size = 12
    axis_label_size = 11
    plot_title_size = 16

    output$overviewPlot <- renderPlot({
        
        inFile <- input$file1
        
        data_plot <- graph_data %>%
          filter(period!="percentage") %>%
                     filter(rolling == input$smoothing) %>% 
                     filter(type %in% input$hospital_type |
                            type %in% input$cardio_type |
                            type %in% input$cerebro_type |
                            type %in% input$vascular_type)
        
        data_plot_pct <- graph_data %>%
          filter(period=="percentage") %>%
          filter(rolling == input$smoothing) %>% 
          filter(type %in% input$hospital_type |
                   type %in% input$cardio_type |
                   type %in% input$cerebro_type |
                   type %in% input$vascular_type)
        
        no_submission <- hospital_data
        
        hospital_plot <- no_submission %>% 
          filter(period!="percentage") %>% 
          filter(rolling == input$smoothing) %>% 
          filter(type %in% input$hospital_type |
                   type %in% input$cardio_type |
                   type %in% input$cerebro_type |
                   type %in% input$vascular_type)
        
        hospital_plot_pct <- no_submission %>% 
          filter(period=="percentage") %>% 
          filter(rolling == input$smoothing) %>% 
          filter(type %in% input$hospital_type |
                   type %in% input$cardio_type |
                   type %in% input$cerebro_type |
                   type %in% input$vascular_type)
        
        p <- ggplot(data_plot) +
                geom_line(aes(date, number, colour=type, linetype=period), size=1) +
                geom_vline(aes(xintercept=as.Date("2020-01-31")),
                           linetype = "dashed", colour="#047d24", size=0.5) +
                geom_vline(aes(xintercept=as.Date("2020-03-23")),
                           linetype = "dashed", colour="#4f009e", size=0.5) +
                scale_y_continuous(name="Mean no. of activity",
                                   minor_breaks = NULL) +
                scale_x_date(name="Date",
                                   limits= as.Date(c(input$slider[1], input$slider[2])),
                                   breaks = "months",
                                   labels = date_format("%b"),
                                   minor_breaks=NULL) +
                scale_color_discrete(guide=guide_legend(nrow=2)) +
                scale_linetype_discrete(guide=guide_legend(nrow=2)) +
                labs(title="Data from collaborators",
                     caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
                theme(
                    panel.background = element_rect(fill="transparent", colour = NA),
                    plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
                    plot.background = element_rect(fill = "transparent", colour = NA),
                    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                    panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
                    panel.grid.major.x = element_blank(),
                    plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
                    plot.caption.position =  "plot",
                    plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
        
        r <- ggplot(data_plot_pct) +
          geom_line(aes(date, number, colour=type), size=1) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          scale_y_continuous(name="Percentage change from 2018-2019",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider[1], input$slider[2])),
                       breaks = "months",
                       labels = date_format("%b %y"),
                       minor_breaks=NULL) +
          scale_color_discrete(guide=guide_legend(nrow=2)) +
          labs(title="Data from collaborators",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
        
        t <- ggplot(hospital_plot) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type, linetype=period), size=1) +
          facet_wrap(~location, ncol = 5) +
          scale_y_continuous(name="No. of activity",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider[1], input$slider[2])),
                       labels = date_format("%b"),
                       breaks = "2 months",
                       minor_breaks=NULL) +
          labs(title="Activity by hospitals",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
              size = 12, color = "white", face = "bold"
            ),
          )
        
        v <- ggplot(hospital_plot_pct) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type, linetype=period), size=1) +
          facet_wrap(~location, ncol = 5) +
          scale_y_continuous(name="Percentage change from 2018-2019",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider[1], input$slider[2])),
                       labels = date_format("%b %y"),
                       breaks = "2 months",
                       minor_breaks=NULL) +
          labs(title="Activity by hospitals",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),  
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
              size = 12, color = "white", face = "bold"
            ),
          ) + guides(linetype=FALSE)
        
        if (is.null(inFile) == TRUE & input$pct == FALSE & input$individual_hosp == FALSE) {
            p
        }
        else if (is.null(inFile) == FALSE & input$pct == FALSE & input$individual_hosp == FALSE) {
    
          inFile <- input$file1
          
          
            # Processing submitted file
            submission <- extract_submission(inFile$datapath)
            
            # Copying submitted file
            if (input$bin_data==FALSE) {
              file.copy(inFile$datapath, here::here("submit"), overwrite = TRUE)
              file.rename(from = here::here("submit/0.xlsx"), to = file.path("submit", inFile$name))
            } else if (input$bin_data==TRUE) {
              NULL
            }
            
            # Plot
            new_data_plot <- graph_data %>% 
              filter(period!="percentage") %>%
              filter(rolling == input$smoothing) %>% 
              filter(type %in% input$hospital_type |
                     type %in% input$cardio_type |
                     type %in% input$cerebro_type |
                     type %in% input$vascular_type)
            
            new_data <- submission %>% 
                filter(period!="percentage") %>%
                filter(rolling == input$smoothing) %>% 
                filter(type %in% input$hospital_type |
                           type %in% input$cardio_type |
                           type %in% input$cerebro_type |
                           type %in% input$vascular_type)
            
            q <- ggplot() +
                geom_line(data = new_data,
                          aes(date, number, colour=type, linetype=period), size=1) +
                geom_vline(aes(xintercept=as.Date("2020-01-31")),
                           linetype = "dashed", colour="#047d24", size=0.5) +
                geom_vline(aes(xintercept=as.Date("2020-03-23")),
                           linetype = "dashed", colour="#4f009e", size=0.5) +
                scale_y_continuous(name="No. of activity",
                                   minor_breaks = NULL) +
                scale_x_date(name="Date",
                             limits= as.Date(c(input$slider[1], input$slider[2])),
                             labels = date_format("%b"),
                             breaks = "months",
                             minor_breaks=NULL) +
                scale_colour_brewer(palette='Set2') +
                labs(title="Your submitted data",
                     caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
                theme(
                    panel.background = element_rect(fill="transparent", colour = NA),
                    plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
                    plot.background = element_rect(fill = "transparent", colour = NA),
                    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                    panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
                    panel.grid.major.x = element_blank(),
                    plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
                    plot.caption.position =  "plot",
                    plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
                    axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
                    axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
                    axis.text = element_text(size=axis_label_size),
                    legend.key = element_rect(fill = "transparent", color = NA),
                    legend.key.size = unit(1, "cm"),
                    legend.background = element_rect(fill = "transparent", colour = NA),
                    legend.text = element_text(size = axis_name_size),
                    legend.position="bottom",
                    legend.title = element_blank()
                )
            
            (p / q) & theme(plot.background = element_rect(fill = "transparent", colour = NA))
        }
            else if (is.null(inFile) == TRUE & input$pct == TRUE & input$individual_hosp == FALSE) {
            
              r
              
            }
        else if (is.null(inFile) == FALSE & input$pct == TRUE & input$individual_hosp == FALSE) {
          
          inFile <- input$file1
          
          submission <- extract_submission(inFile$datapath)
          
          new_data_pct <- submission %>% 
            filter(period=="percentage") %>%
            filter(rolling == input$smoothing) %>% 
            filter(type %in% input$hospital_type |
                     type %in% input$cardio_type |
                     type %in% input$cerebro_type |
                     type %in% input$vascular_type)
          
          s <- ggplot(new_data_pct) +
            geom_vline(aes(xintercept=as.Date("2020-01-31")),
                       linetype = "dashed", colour="#047d24", size=0.5) +
            geom_vline(aes(xintercept=as.Date("2020-03-23")),
                       linetype = "dashed", colour="#4f009e", size=0.5) +
            geom_line(aes(date, number, colour=type), size=1) +
            scale_y_continuous(name="Percentage change from 2018-2019",
                               minor_breaks = NULL) +
            scale_x_date(name="Date",
                         limits= as.Date(c(input$slider[1], input$slider[2])),
                         breaks = "months",
                         labels = date_format("%b %y"),
                         minor_breaks=NULL) +
            scale_color_discrete(guide=guide_legend(nrow=2)) +
            labs(title="Data from collaborators",
                 caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
            theme(
              panel.background = element_rect(fill="transparent", colour = NA),
              plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
              plot.caption.position =  "plot",
              plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
            ) + guides(linetype=FALSE)
          
          (r/ s) & theme(plot.background = element_rect(fill = "transparent", colour = NA))
          
          
        }
        
        else if (is.null(inFile) == TRUE & input$pct == FALSE & input$individual_hosp == TRUE) {

          t
        }

        else if (is.null(inFile) == FALSE & input$pct == FALSE & input$individual_hosp == TRUE) {

          inFile <- input$file1

          # Processing submitted file
          submission_hospital <- extract_submission(inFile$datapath)
          submission <- rbind(hospital_data, submission_hospital)
          hospital_plot_submit <- submission %>%
            filter(period!="percentage") %>%
            filter(rolling == input$smoothing) %>%
            filter(type %in% input$hospital_type |
                     type %in% input$cardio_type |
                     type %in% input$cerebro_type |
                     type %in% input$vascular_type)

          u <- ggplot(hospital_plot_submit) +
            geom_vline(aes(xintercept=as.Date("2020-01-31")),
                       linetype = "dashed", colour="#047d24", size=0.5) +
            geom_vline(aes(xintercept=as.Date("2020-03-23")),
                       linetype = "dashed", colour="#4f009e", size=0.5) +
            geom_line(aes(date, number, colour=type, linetype=period), size=1) +
            facet_wrap(~location, ncol = 5) +
            scale_y_continuous(name="No. of activity",
                               minor_breaks = NULL) +
            scale_x_date(name="Date",
                         limits= as.Date(c(input$slider[1], input$slider[2])),
                         labels = date_format("%b"),
                         breaks = "2 months",
                         minor_breaks=NULL) +
            labs(title="Activity by hospitals/trusts",
                 caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
            theme(
              panel.background = element_rect(fill="transparent", colour = NA),
              plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
              plot.caption.position =  "plot",
              plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
              axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
              axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
                size = 12, color = "white", face = "bold"
              ),
            )

          u
        }

        else if (is.null(inFile) == TRUE & input$pct == TRUE & input$individual_hosp == TRUE) {

          v
        }
        

        else if (is.null(inFile) == FALSE & input$pct == TRUE & input$individual_hosp == TRUE) {


          inFile <- input$file1
          
          # Processing submitted file
          submission_hospital <- extract_submission(inFile$datapath)
          submission <- rbind(hospital_data, submission_hospital)
          hospital_plot_submit <- submission %>%
            filter(period=="percentage") %>%
            filter(rolling == input$smoothing) %>%
            filter(type %in% input$hospital_type |
                     type %in% input$cardio_type |
                     type %in% input$cerebro_type |
                     type %in% input$vascular_type)

          w <- ggplot(hospital_plot_submit) +
            geom_vline(aes(xintercept=as.Date("2020-01-31")),
                       linetype = "dashed", colour="#047d24", size=0.5) +
            geom_vline(aes(xintercept=as.Date("2020-03-23")),
                       linetype = "dashed", colour="#4f009e", size=0.5) +
            geom_line(aes(date, number, colour=type, linetype=period), size=1) +
            facet_wrap(~location, ncol = 5) +
            scale_y_continuous(name="Percentage change from 2018-2019",
                               minor_breaks = NULL) +
            scale_x_date(name="Date",
                         limits= as.Date(c(input$slider[1], input$slider[2])),
                         labels = date_format("%b %y"),
                         breaks = "2 months",
                         minor_breaks=NULL) +
            labs(title="Activity by hospitals",
                 caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
            theme(
              panel.background = element_rect(fill="transparent", colour = NA),
              plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.border = element_rect(colour = "black", fill=NA, size=0.5),
              panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
              plot.caption.position =  "plot",
              plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
              axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
              axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
                size = 12, color = "white", face = "bold"
              ),
            ) + guides(linetype=FALSE)

          w
          
    }

        
    }, bg="transparent")
    

    #####################################################
    #####################################################
    # Cardio plot ####################################### ===========================================
    #####################################################
    #####################################################
    
    output$cardioPlot <- renderPlot({
      
      inFile <- input$file1
      
      data_plot <- graph_data %>%
        filter(period!="percentage") %>%
        filter(rolling == input$smoothing_cardio) %>% 
        filter(type %in% input$cardio_type_cardio)
      
      data_plot_pct <- graph_data %>%
        filter(period=="percentage") %>%
        filter(rolling == input$smoothing_cardio) %>% 
        filter(type %in% input$cardio_type_cardio)
      
      no_submission <- hospital_data
      
      hospital_plot <- no_submission %>% 
        filter(period!="percentage") %>% 
        filter(rolling == input$smoothing_cardio) %>% 
        filter(type %in% input$cardio_type_cardio)
      
      hospital_plot_pct <- no_submission %>% 
        filter(period=="percentage") %>% 
        filter(rolling == input$smoothing_cardio) %>% 
        filter(type %in% input$cardio_type_cardio)
      
      p <- ggplot(data_plot) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        scale_y_continuous(name="Mean no. of activity",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_cardio[1], input$slider_cardio[2])),
                     breaks = "months",
                     labels = date_format("%b"),
                     minor_breaks=NULL) +
        scale_color_discrete(guide=guide_legend(nrow=2)) +
        scale_linetype_discrete(guide=guide_legend(nrow=2)) +
        labs(title="Data from collaborators",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
      
      r <- ggplot(data_plot_pct) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type), size=1) +
        scale_y_continuous(name="Percentage change from 2018-2019",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_cardio[1], input$slider_cardio[2])),
                     breaks = "months",
                     labels = date_format("%b %y"),
                     minor_breaks=NULL) +
        scale_color_discrete(guide=guide_legend(nrow=2)) +
        labs(title="Data from collaborators",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
        ) + guides(linetype=FALSE)
      
      t <- ggplot(hospital_plot) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        facet_wrap(~location, ncol = 5) +
        scale_y_continuous(name="No. of activity",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_cardio[1], input$slider_cardio[2])),
                     labels = date_format("%b"),
                     breaks = "2 months",
                     minor_breaks=NULL) +
        labs(title="Activity by hospitals",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
          axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
          axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
            size = 12, color = "white", face = "bold"
          ),
        )
      
      v <- ggplot(hospital_plot_pct) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        facet_wrap(~location, ncol = 5) +
        scale_y_continuous(name="Percentage change from 2018-2019",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_cardio[1], input$slider_cardio[2])),
                     labels = date_format("%b %y"),
                     breaks = "2 months",
                     minor_breaks=NULL) +
        labs(title="Activity by hospitals",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
          axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
          axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
            size = 12, color = "white", face = "bold"
          ),
        )  + guides(linetype=FALSE)
      
      if (is.null(inFile) == TRUE & input$pct_cardio == FALSE & input$individual_hosp_cardio == FALSE) {
        p
      }
      else if (is.null(inFile) == FALSE & input$pct_cardio == FALSE & input$individual_hosp_cardio == FALSE) {
        
        inFile <- input$file1
        
        
        # Processing submitted file
        submission <- extract_submission(inFile$datapath)
        

        # Plot
        new_data_plot <- graph_data %>% 
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_cardio) %>% 
          filter(type %in% input$cardio_type_cardio)
        
        new_data <- submission %>% 
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_cardio) %>% 
          filter(type %in% input$cardio_type_cardio)
        
        q <- ggplot() +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(data = new_data,
                    aes(date, number, colour=type, linetype=period), size=1) +
          scale_y_continuous(name="No. of activity",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_cardio[1], input$slider_cardio[2])),
                       labels = date_format("%b"),
                       breaks = "months",
                       minor_breaks=NULL) +
          scale_colour_brewer(palette='Set2') +
          labs(title="Your submitted data",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
            axis.text = element_text(size=axis_label_size),
            legend.key = element_rect(fill = "transparent", color = NA),
            legend.key.size = unit(1, "cm"),
            legend.background = element_rect(fill = "transparent", colour = NA),
            legend.text = element_text(size = axis_name_size),
            legend.position="bottom",
            legend.title = element_blank()
          )
        
        (p / q) & theme(plot.background = element_rect(fill = "transparent", colour = NA))
      }
      else if (is.null(inFile) == TRUE & input$pct_cardio == TRUE & input$individual_hosp_cardio == FALSE) {
        
        r
        
      }
      else if (is.null(inFile) == FALSE & input$pct_cardio == TRUE & input$individual_hosp_cardio == FALSE) {
        
        inFile <- input$file1
        
        submission <- extract_submission(inFile$datapath)
        
        new_data_pct <- submission %>% 
          filter(period=="percentage") %>%
          filter(rolling == input$smoothing_cardio) %>% 
          filter(type %in% input$cardio_type_cardio)
        
        s <- ggplot(new_data_pct) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type), size=1) +
          scale_y_continuous(name="Percentage change from 2018-2019",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_cardio[1], input$slider_cardio[2])),
                       breaks = "months",
                       labels = date_format("%b %y"),
                       minor_breaks=NULL) +
          scale_color_discrete(guide=guide_legend(nrow=2)) +
          scale_linetype_discrete(guide=guide_legend(nrow=2)) +
          labs(title="Data from collaborators",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
          ) + guides(linetype=FALSE)
        
        (r/ s) & theme(plot.background = element_rect(fill = "transparent", colour = NA))
        
        
      }
      
      else if (is.null(inFile) == TRUE & input$pct_cardio == FALSE & input$individual_hosp_cardio == TRUE) {
        
        t
      }
      
      else if (is.null(inFile) == FALSE & input$pct_cardio == FALSE & input$individual_hosp_cardio == TRUE) {
        
        inFile <- input$file1
        
        # Processing submitted file
        submission_hospital <- extract_submission(inFile$datapath)
        submission <- rbind(hospital_data, submission_hospital)
        hospital_plot_submit <- submission %>%
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_cardio) %>%
          filter(type %in% input$cardio_type_cardio)
        
        u <- ggplot(hospital_plot_submit) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type, linetype=period), size=1) +
          facet_wrap(~location, ncol = 5) +
          scale_y_continuous(name="No. of activity",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_cardio[1], input$slider_cardio[2])),
                       labels = date_format("%b"),
                       breaks = "2 months",
                       minor_breaks=NULL) +
          labs(title="Activity by hospitals/trusts",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
              size = 12, color = "white", face = "bold"
            ),
          )
        
        u
      }
      
      else if (is.null(inFile) == TRUE & input$pct_cardio == TRUE & input$individual_hosp_cardio == TRUE) {
        
        v
      }
      
      
      else if (is.null(inFile) == FALSE & input$pct_cardio == TRUE & input$individual_hosp_cardio == TRUE) {
        
        inFile <- input$file1
        
        # Processing submitted file
        submission_hospital <- extract_submission(inFile$datapath)
        submission <- rbind(hospital_data, submission_hospital)
        hospital_plot_submit <- submission %>%
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_cardio) %>%
          filter(type %in% input$cardio_type_cardio)
        
        # Processing submitted file
        submission_hospital <- extract_submission(inFile$datapath)
        submission <- rbind(hospital_data, submission_hospital)
        hospital_plot_submit <- submission %>%
          filter(period=="percentage") %>%
          filter(rolling == input$smoothing_cardio) %>%
          filter(type %in% input$cardio_type_cardio)
        
        w <- ggplot(hospital_plot_submit) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type, linetype=period), size=1) +
          facet_wrap(~location, ncol = 5) +
          scale_y_continuous(name="Percentage change from 2018-2019",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_cardio[1], input$slider_cardio[2])),
                       labels = date_format("%b %y"),
                       breaks = "2 months",
                       minor_breaks=NULL) +
          labs(title="Activity by hospitals",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
              size = 12, color = "white", face = "bold"
            ),
          )  + guides(linetype=FALSE)
        
        w
        
      }
      
      
    }, bg="transparent")
    
    
    #####################################################
    #####################################################
    # Cerebral plot ##################################### ===========================================
    #####################################################
    #####################################################
    
    output$cerebralPlot <- renderPlot({
      
      inFile <- input$file1
      
      data_plot <- graph_data %>%
        filter(period!="percentage") %>%
        filter(rolling == input$smoothing_cerebral) %>% 
        filter(type %in% input$cerebro_type_cerebral)
      
      data_plot_pct <- graph_data %>%
        filter(period=="percentage") %>%
        filter(rolling == input$smoothing_cerebral) %>% 
        filter(type %in% input$cerebro_type_cerebral)
      
      no_submission <- hospital_data
      
      hospital_plot <- no_submission %>% 
        filter(period!="percentage") %>% 
        filter(rolling == input$smoothing_cerebral) %>% 
        filter(type %in% input$cerebro_type_cerebral)
      
      hospital_plot_pct <- no_submission %>% 
        filter(period=="percentage") %>% 
        filter(rolling == input$smoothing_cerebral) %>% 
        filter(type %in% input$cerebro_type_cerebral)
      
      p <- ggplot(data_plot) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        scale_y_continuous(name="Mean no. of activity",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_cerebral[1], input$slider_cerebral[2])),
                     breaks = "months",
                     labels = date_format("%b"),
                     minor_breaks=NULL) +
        scale_color_discrete(guide=guide_legend(nrow=2)) +
        scale_linetype_discrete(guide=guide_legend(nrow=2)) +
        labs(title="Data from collaborators",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
      
      r <- ggplot(data_plot_pct) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type), size=1) +
        scale_y_continuous(name="Percentage change from 2018-2019",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_cerebral[1], input$slider_cerebral[2])),
                     breaks = "months",
                     labels = date_format("%b %y"),
                     minor_breaks=NULL) +
        scale_color_discrete(guide=guide_legend(nrow=2)) +
        scale_linetype_discrete(guide=guide_legend(nrow=2)) +
        labs(title="Data from collaborators",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
        )  + guides(linetype=FALSE)
      
      t <- ggplot(hospital_plot) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        facet_wrap(~location, ncol = 5) +
        scale_y_continuous(name="No. of activity",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_cerebral[1], input$slider_cerebral[2])),
                     labels = date_format("%b"),
                     breaks = "2 months",
                     minor_breaks=NULL) +
        labs(title="Activity by hospitals",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
          axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
          axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
            size = 12, color = "white", face = "bold"
          ),
        )
      
      v <- ggplot(hospital_plot_pct) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        facet_wrap(~location, ncol = 5) +
        scale_y_continuous(name="Percentage change from 2018-2019",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_cerebral[1], input$slider_cerebral[2])),
                     labels = date_format("%b %y"),
                     breaks = "2 months",
                     minor_breaks=NULL) +
        labs(title="Activity by hospitals",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
          axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
          axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
            size = 12, color = "white", face = "bold"
          ),
        )  + guides(linetype=FALSE)
      
      if (is.null(inFile) == TRUE & input$pct_cerebral == FALSE & input$individual_hosp_cerebral == FALSE) {
        p
      }
      else if (is.null(inFile) == FALSE & input$pct_cerebral == FALSE & input$individual_hosp_cerebral == FALSE) {
        
        inFile <- input$file1
        
        
        # Processing submitted file
        submission <- extract_submission(inFile$datapath)
        
        # Plot
        new_data_plot <- graph_data %>% 
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_cerebral) %>% 
          filter(type %in% input$cerebro_type_cerebral)
        
        new_data <- submission %>% 
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_cerebral) %>% 
          filter(type %in% input$cerebro_type_cerebral)
        
        q <- ggplot() +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(data = new_data,
                    aes(date, number, colour=type, linetype=period), size=1.5) +
          scale_y_continuous(name="No. of activity",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_cerebral[1], input$slider_cerebral[2])),
                       labels = date_format("%b"),
                       breaks = "months",
                       minor_breaks=NULL) +
          scale_colour_brewer(palette='Set2') +
          labs(title="Your submitted data",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
            axis.text = element_text(size=axis_label_size),
            legend.key = element_rect(fill = "transparent", color = NA),
            legend.key.size = unit(1, "cm"),
            legend.background = element_rect(fill = "transparent", colour = NA),
            legend.text = element_text(size = axis_name_size),
            legend.position="bottom",
            legend.title = element_blank()
          )
        
        (p / q) & theme(plot.background = element_rect(fill = "transparent", colour = NA))
      }
      else if (is.null(inFile) == TRUE & input$pct_cerebral == TRUE & input$individual_hosp_cerebral == FALSE) {
        
        r
        
      }
      else if (is.null(inFile) == FALSE & input$pct_cerebral == TRUE & input$individual_hosp_cerebral == FALSE) {
        
        inFile <- input$file1
        
        submission <- extract_submission(inFile$datapath)
        
        new_data_pct <- submission %>% 
          filter(period=="percentage") %>%
          filter(rolling == input$smoothing_cerebral) %>% 
          filter(type %in% input$cerebro_type_cerebral)
        
        s <- ggplot(new_data_pct) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type), size=1) +
          scale_y_continuous(name="Percentage change from 2018-2019",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_cerebral[1], input$slider_cerebral[2])),
                       breaks = "months",
                       labels = date_format("%b %y"),
                       minor_breaks=NULL) +
          scale_color_discrete(guide=guide_legend(nrow=2)) +
          scale_linetype_discrete(guide=guide_legend(nrow=2)) +
          labs(title="Data from collaborators",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
          )  + guides(linetype=FALSE)
        
        (r/ s) & theme(plot.background = element_rect(fill = "transparent", colour = NA))
        
        
      }
      
      else if (is.null(inFile) == TRUE & input$pct_cerebral == FALSE & input$individual_hosp_cerebral == TRUE) {
        
        t
      }
      
      else if (is.null(inFile) == FALSE & input$pct_cerebral == FALSE & input$individual_hosp_cerebral == TRUE) {
        
        inFile <- input$file1
        
        # Processing submitted file
        submission_hospital <- extract_submission(inFile$datapath)
        submission <- rbind(hospital_data, submission_hospital)
        hospital_plot_submit <- submission %>%
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_cerebral) %>%
          filter(type %in% input$cerebro_type_cerebral)
        
        u <- ggplot(hospital_plot_submit) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type, linetype=period), size=1) +
          facet_wrap(~location, ncol = 5) +
          scale_y_continuous(name="No. of activity",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_cerebral[1], input$slider_cerebral[2])),
                       labels = date_format("%b"),
                       breaks = "2 months",
                       minor_breaks=NULL) +
          labs(title="Activity by hospitals/trusts",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
              size = 12, color = "white", face = "bold"
            ),
          )
        
        u
      }
      
      else if (is.null(inFile) == TRUE & input$pct_cerebral == TRUE & input$individual_hosp_cerebral == TRUE) {
        
        v
      }
      
      
      else if (is.null(inFile) == FALSE & input$pct_cerebral == TRUE & input$individual_hosp_cerebral == TRUE) {
        
        
        inFile <- input$file1
        
        # Processing submitted file
        submission_hospital <- extract_submission(inFile$datapath)
        submission <- rbind(hospital_data, submission_hospital)
        hospital_plot_submit <- submission %>%
          filter(period=="percentage") %>%
          filter(rolling == input$smoothing_cerebral) %>%
          filter(type %in% input$cerebro_type_cerebral)
        
        w <- ggplot(hospital_plot_submit) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type, linetype=period), size=1) +
          facet_wrap(~location, ncol = 5) +
          scale_y_continuous(name="Percentage change from 2018-2019",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_cerebral[1], input$slider_cerebral[2])),
                       labels = date_format("%b %y"),
                       breaks = "2 months",
                       minor_breaks=NULL) +
          labs(title="Activity by hospitals",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
              size = 12, color = "white", face = "bold"
            ),
          )  + guides(linetype=FALSE)
        
        w
        
      }
      
      
    }, bg="transparent")
    
    #####################################################
    #####################################################
    # Vascular plot ##################################### ===========================================
    #####################################################
    #####################################################
    
    output$vascPlot <- renderPlot({
      
      inFile <- input$file1
      
      data_plot <- graph_data %>%
        filter(period!="percentage") %>%
        filter(rolling == input$smoothing_vasc) %>% 
        filter(type %in% input$vascular_type_vasc)
      
      data_plot_pct <- graph_data %>%
        filter(period=="percentage") %>%
        filter(rolling == input$smoothing_vasc) %>% 
        filter(type %in% input$vascular_type_vasc)
      
      no_submission <- hospital_data
      
      hospital_plot <- no_submission %>% 
        filter(period!="percentage") %>% 
        filter(rolling == input$smoothing_vasc) %>% 
        filter(type %in% input$vascular_type_vasc)
      
      hospital_plot_pct <- no_submission %>% 
        filter(period=="percentage") %>% 
        filter(rolling == input$smoothing_vasc) %>% 
        filter(type %in% input$vascular_type_vasc)
      
      p <- ggplot(data_plot) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        scale_y_continuous(name="Mean no. of activity",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_vasc[1], input$slider_vasc[2])),
                     breaks = "months",
                     labels = date_format("%b"),
                     minor_breaks=NULL) +
        scale_color_discrete(guide=guide_legend(nrow=2)) +
        scale_linetype_discrete(guide=guide_legend(nrow=2)) +
        labs(title="Data from collaborators",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
      
      r <- ggplot(data_plot_pct) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type), size=1) +
        scale_y_continuous(name="Percentage change from 2018-2019",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_vasc[1], input$slider_vasc[2])),
                     breaks = "months",
                     labels = date_format("%b %y"),
                     minor_breaks=NULL) +
        scale_color_discrete(guide=guide_legend(nrow=2)) +
        scale_linetype_discrete(guide=guide_legend(nrow=2)) +
        labs(title="Data from collaborators",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
        )  + guides(linetype=FALSE)
      
      t <- ggplot(hospital_plot) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        facet_wrap(~location, ncol = 5) +
        scale_y_continuous(name="No. of activity",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_vasc[1], input$slider_vasc[2])),
                     labels = date_format("%b"),
                     breaks = "2 months",
                     minor_breaks=NULL) +
        labs(title="Activity by hospitals",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
          axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
          axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
            size = 12, color = "white", face = "bold"
          ),
        )
      
      v <- ggplot(hospital_plot_pct) +
        geom_vline(aes(xintercept=as.Date("2020-01-31")),
                   linetype = "dashed", colour="#047d24", size=0.5) +
        geom_vline(aes(xintercept=as.Date("2020-03-23")),
                   linetype = "dashed", colour="#4f009e", size=0.5) +
        geom_line(aes(date, number, colour=type, linetype=period), size=1) +
        facet_wrap(~location, ncol = 5) +
        scale_y_continuous(name="Percentage change from 2018-2019",
                           minor_breaks = NULL) +
        scale_x_date(name="Date",
                     limits= as.Date(c(input$slider_vasc[1], input$slider_vasc[2])),
                     labels = date_format("%b %y"),
                     breaks = "2 months",
                     minor_breaks=NULL) +
        labs(title="Activity by hospitals",
             caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
        
        theme(
          panel.background = element_rect(fill="transparent", colour = NA),
          plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
          axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
          axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
            size = 12, color = "white", face = "bold"
          ),
        )  + guides(linetype=FALSE)
      
      if (is.null(inFile) == TRUE & input$pct_vasc == FALSE & input$individual_hosp_vasc == FALSE) {
        p
      }
      else if (is.null(inFile) == FALSE & input$pct_vasc == FALSE & input$individual_hosp_vasc == FALSE) {
        
        inFile <- input$file1
        
        
        # Processing submitted file
        submission <- extract_submission(inFile$datapath)
        

        # Plot
        new_data_plot <- graph_data %>% 
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_vasc) %>% 
          filter(type %in% input$vascular_type_vasc)
        
        new_data <- submission %>% 
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_vasc) %>% 
          filter(type %in% input$vascular_type_vasc)
        
        q <- ggplot() +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(data = new_data,
                    aes(date, number, colour=type, linetype=period), size=1.5) +
          scale_y_continuous(name="No. of activity",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_vasc[1], input$slider_vasc[2])),
                       labels = date_format("%b"),
                       breaks = "months",
                       minor_breaks=NULL) +
          labs(title="Your submitted data",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
            axis.text = element_text(size=axis_label_size),
            legend.key = element_rect(fill = "transparent", color = NA),
            legend.key.size = unit(1, "cm"),
            legend.background = element_rect(fill = "transparent", colour = NA),
            legend.text = element_text(size = axis_name_size),
            legend.position="bottom",
            legend.title = element_blank()
          )
        
        (p / q) & theme(plot.background = element_rect(fill = "transparent", colour = NA))
      }
      else if (is.null(inFile) == TRUE & input$pct_vasc == TRUE & input$individual_hosp_vasc == FALSE) {
        
        r
        
      }
      else if (is.null(inFile) == FALSE & input$pct_vasc == TRUE & input$individual_hosp_vasc == FALSE) {
        
        inFile <- input$file1
        
        submission <- extract_submission(inFile$datapath)
        
        new_data_pct <- submission %>% 
          filter(period=="percentage") %>%
          filter(rolling == input$smoothing_vasc) %>% 
          filter(type %in% input$vascular_type_vasc)
        
        s <- ggplot(new_data_pct) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type), size=1) +
          scale_y_continuous(name="Percentage change from 2018-2019",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_vasc[1], input$slider_vasc[2])),
                       breaks = "months",
                       labels = date_format("%b %y"),
                       minor_breaks=NULL) +
          # scale_colour_brewer(palette='Set2') +
          scale_color_discrete(guide=guide_legend(nrow=2)) +
          scale_linetype_discrete(guide=guide_legend(nrow=2)) +
          labs(title="Data from collaborators",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
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
          )  + guides(linetype=FALSE)
        
        (r/ s) & theme(plot.background = element_rect(fill = "transparent", colour = NA))
        
        
      }
      
      else if (is.null(inFile) == TRUE & input$pct_vasc == FALSE & input$individual_hosp_vasc == TRUE) {
        
        t
      }
      
      else if (is.null(inFile) == FALSE & input$pct_vasc == FALSE & input$individual_hosp_vasc == TRUE) {
        
        inFile <- input$file1
        
        # Processing submitted file
        submission_hospital <- extract_submission(inFile$datapath)
        submission <- rbind(hospital_data, submission_hospital)
        hospital_plot_submit <- submission %>%
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_vasc) %>%
          filter(type %in% input$vascular_type_vasc)
        
        u <- ggplot(hospital_plot_submit) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type, linetype=period), size=1) +
          facet_wrap(~location, ncol = 5) +
          scale_y_continuous(name="No. of activity",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_vasc[1], input$slider_vasc[2])),
                       labels = date_format("%b"),
                       breaks = "2 months",
                       minor_breaks=NULL) +
          labs(title="Activity by hospitals/trusts",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
              size = 12, color = "white", face = "bold"
            ),
          )
        
        u
      }
      
      else if (is.null(inFile) == TRUE & input$pct_vasc == TRUE & input$individual_hosp_vasc == TRUE) {
        
        v
      }
      
      
      else if (is.null(inFile) == FALSE & input$pct_vasc == TRUE & input$individual_hosp_vasc == TRUE) {
        
        
        inFile <- input$file1
        
        # Processing submitted file
        submission_hospital <- extract_submission(inFile$datapath)
        submission <- rbind(hospital_data, submission_hospital)
        hospital_plot_submit <- submission %>%
          filter(period!="percentage") %>%
          filter(rolling == input$smoothing_vasc) %>%
          filter(type %in% input$vascular_type_vasc)
        
        w <- ggplot(hospital_plot_submit) +
          geom_vline(aes(xintercept=as.Date("2020-01-31")),
                     linetype = "dashed", colour="#047d24", size=0.5) +
          geom_vline(aes(xintercept=as.Date("2020-03-23")),
                     linetype = "dashed", colour="#4f009e", size=0.5) +
          geom_line(aes(date, number, colour=type, linetype=period), size=1) +
          facet_wrap(~location, ncol = 5) +
          scale_y_continuous(name="Percentage change from 2018-2019",
                             minor_breaks = NULL) +
          scale_x_date(name="Date",
                       limits= as.Date(c(input$slider_vasc[1], input$slider_vasc[2])),
                       labels = date_format("%b %y"),
                       breaks = "2 months",
                       minor_breaks=NULL) +
          labs(title="Activity by hospitals",
               caption="Green dotted vertical line represents the first UK confirmed case of COVID-19 on 31 Jan 2020; \nPurple dotted vertical line represents UK lockdown on 23 Mar 2020.") +
          theme(
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.margin = unit(c(1, 1, 0.1, 0.7), "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            panel.grid.major.y = element_line(size=0.25, linetype = "solid", colour="#c9dff5"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=plot_title_size, face="bold", vjust=3),
            plot.caption.position =  "plot",
            plot.caption = element_text(size = 13, hjust = 0, face= "italic"),
            axis.title.y = element_text(size=axis_name_size, vjust=3, margin = margin(t = 20, r = 0, b=30, l=0)),
            axis.title.x = element_text(size=axis_name_size, vjust=-1.5),
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
              size = 12, color = "white", face = "bold"
            ),
          )  + guides(linetype=FALSE)
        
        w
        
      }
      
      
    }, bg="transparent")
    
    #####################################################
    #####################################################
    # Table ##################################### ===========================================
    #####################################################
    #####################################################
    
    output$table1 <- renderDataTable({
        
          inFile <- input$file1
          
          if (is.null(inFile) == TRUE) {
            
            table_data %>% subset(type==input$options) %>% 
              select(-c("type")) %>%
              datatable(., 
                        caption = htmltools::tags$caption(
                          style = 'caption-side: top; text-align: center; color: black; font-size: 12; font-weight: bold',
                          'Percentage change of activity from previous year'
                        ),
                        rownames = FALSE,
                        options = list(rowCallback = JS(rowCallback),
                                       columnDefs = list(list(className = 'dt-center', targets = 0:3)))) %>% 
              formatPercentage(c('Before 1st case', 'Between 1st case and lockdown', 'After lockdown'), 1)
            
          } else if (is.null(inFile) == FALSE) {
            
            inFile <- input$file1
            
            # Processing submitted file
            table_new <- extract_table(inFile$datapath)
            
            table_new %>% subset(type==input$options) %>% 
              select(-c("type")) %>%
              datatable(., 
                        caption = htmltools::tags$caption(
                          style = 'caption-side: top; text-align: center; color: black; font-size: 12; font-weight: bold',
                          'Percentage change of activity from previous year'
                        ),
                        rownames = FALSE,
                        options = list(pageLength = 15,
                                       rowCallback = JS(rowCallback),
                                       columnDefs = list(list(className = 'dt-center', targets = 0:3)))) %>% 
              formatPercentage(c('Before 1st case', 'Between 1st case and lockdown', 'After lockdown'), 1)
          }
          
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

