#=============================================#
# Shiny App for HIVDR Sample Size Calculations
# 
# Results displayed at 
# https://smwu.shinyapps.io/HIVDR_2/
#=============================================#

# Load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyMatrix)
library(rhandsontable)

# modify_stop_propagation <- function(x) {
#   x$children[[1]]$attribs$onclick = "event.stopPropagation()"
#   x
# }

##########################
# Define UI code
##########################

ui <- dashboardPage(
  
  dashboardHeader(
    title = h2("Alternative approach for sample size calculations for laboratory-based acquired HIV drug resistance survey"),
    titleWidth = 1400,
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 75px}"),
            tags$style(".main-header .logo {height: 75px}")
    )        
  ),
  
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 75px; font-size: 28px; }"), width = 325,
    tags$style(HTML(".main-sidebar .sidebar .sidebar-menu a{font-size: 20px; }")),
    sidebarMenu(
      menuItem("Phase 1", tabName = "phase1", startExpanded = TRUE,
               menuItem("Inputs", tabName = "inputs_d1"),
               menuItem("Outputs", tabName = "outputs_d1", startExpanded = TRUE,
                        menuSubItem("Sample Sizes", tabName = "sizes_d1"),
                        menuSubItem("Allocation Across Laboratories", tabName = "allocation_d1"))),
      menuItem("Phase 2", tabName = "phase2", startExpanded = TRUE,
               menuItem("Inputs", tabName = "inputs"),
               menuItem("Outputs", tabName = "outputs", startExpanded = TRUE,
                        menuSubItem("Allocation Across Clinics", tabName = "allocation")))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "inputs_d1",
              useShinyjs(),
              fluidPage(
                fluidRow(
                  box(title = "Total number of clinics in each laboratory's catchment area", width = 12, 
                      status = "warning", solidHeader = TRUE,
                      numericInput("num_labs_d1", label = h4("How many viral load laboratories are there in your country?"), 3,
                                   min=1, step=1), br(),
                      htmlOutput("text_hot_d1"), br(),
                      rHandsontableOutput("hot_d1"))
                ),
                # fluidRow(
                #   box(width = 12, status = "primary", 
                #       numericInput("prop_complete", label = h4(HTML("Input the proportion of patients with all required survey 
                #       variables, prop<sub>complete</sub>. Must be a number between 0 and 1.")), 0.7, min=0, max=1, step=0.1)),
                # ),
                fluidRow(
                  box(title = "Available historical data", width = 12, 
                      status = "primary", solidHeader = TRUE,
                      
                      numericInput("prop_complete", 
                                   label = h4(HTML("Input the anticipated proportion of patients with all required survey 
                                                   variables, prop<sub>complete</sub>. Must be a number between 0 and 1.")), 
                                   0.8, min=0, max=1, step=0.1), hr(),
                      
                      radioButtons("historical_data_type", label = h4("What type of national-level historical data is available?"), 
                                   choiceNames = list(HTML("Number of patients with <b>viral non-suppression</b> among patients 
                                                  on DTG-containing and non-DTG-containing regimens"),
                                                  HTML("Number of patients who underwent <b>viral load testing</b> among patients 
                                                  on DTG-containing and non-DTG-containing regimens"),
                                                  HTML("Number of patients on <b>ART</b> among patients 
                                                  on DTG-containing and non-DTG-containing regimens")),
                                   choiceValues =c(1, 2, 3), selected = 1),
                      hr(),
                      htmlOutput("text_hot_historical"), br(),
                      div(style = 'word-wrap: break-word', rHandsontableOutput("hot_historical")), br(),
                      # uiOutput("historical_data")
                      h5(htmlOutput("anticipated_n"))
                      )
                ),
                fluidRow(
                  box(status = "warning", width = 12, title = "Finite population correction", solidHeader = TRUE,
                      h4(htmlOutput("fpc_text")),
                      radioButtons("fpc", label = h5("The finite population correction will reduce the required sample 
                      size and is recommended if the anticipated number of eligible case specimens is small. 
                      If the anticipated number of eligible case specimens is large, the finite population correction is not necessary."), 
                                   choices = list("Yes" = 1, "No" = 2), selected = 1))
                  
                ),
                fluidRow(
                  box(title = "Determining the recommended minimum number of clinics to be sampled", width = 12, status = "success", solidHeader = TRUE,
                      h4(htmlOutput("historical_clinic_text")),
                      radioButtons("historical_clinic", 
                                   label = h5("If not, the national-level historical data reported above will be used to 
                                              calculate mean clinic sizes."), 
                                   choices = list("Yes" = 1, "No" = 2), selected = 1),
                      conditionalPanel("input.historical_clinic == 1",
                           radioButtons("clinic_data_type", label = h4("What type?"), 
                                        choiceNames = list(HTML("Median number of individuals per clinic with 
                                        <b>viral non-suppression</b> among patients on DTG-containing and 
                                        non-DTG-containing regimens"),
                                                           HTML("Median number of individuals per clinic who 
                                        underwent <b>viral load testing</b> among patients 
                                        on DTG-containing and non-DTG-containing regimens"),
                                                           HTML("Median number of individuals per clinic 
                                        on <b>ART</b> among patients 
                                        on DTG-containing and non-DTG-containing regimens")),
                                        choiceValues = c(1, 2, 3), selected = 1),
                           tags$style('
                                      #mydiv{font-weight: normal;}
                                      #ydiv b{font-weight: bold;}'),
                           hr(), 
                           htmlOutput("text_hot_clinic"), br(),
                           div(style = 'word-wrap: break-word', rHandsontableOutput("hot_clinic")), br()),
                           # uiOutput("clinic_data")),
                      conditionalPanel("input.historical_clinic == 2", 
                                       htmlOutput("mean_clinics")),
                      hr(),
                      h4(strong(htmlOutput("min_clinics")))
                  )
                ),
                fluidRow(
                  box(title = "Number of clinics to be sampled", width = 12, status = "success", solidHeader = TRUE,
                      h4(htmlOutput("num_clinics_text")),
                      uiOutput("num_clinics"))
                ),
                div(style = 'text-align:center', actionButton("submit", "Submit", width = '150px', 
                                                              style = "font-weight: bold; border-color: black; font-size: 18px")), br()
              )
      ),
      
      tabItem(tabName = "sizes_d1",
              fluidRow(
                box(title = "DTG Estimate Sample Size", status = "primary",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    # Display table of assumptions and sample size required for DTG
                    h5(htmlOutput("text_DTG")), br(),
                    tableOutput("values_DTG"),
                    h4(htmlOutput("required_DTG")),
                    h4(htmlOutput("target_DTG")),
                ),
                
                box(title = "Overall Estimate Sample Size", status = "primary", 
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    # Display table of assumptions and sample size required for overall
                    h5(htmlOutput("text_O")), br(),
                    tableOutput("values_O"),
                    h4(htmlOutput("required_O")),
                    h4(htmlOutput("target_O"))
                )
              ),
              br(),
              br(),
              
              fluidRow(
                box(title = "Non-DTG Sample Size", status = "primary",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    # Display sample size required for non-DTG
                    h5(htmlOutput("text_non")), 
                    h4(htmlOutput("prop_non")),
                    h4(htmlOutput("required_non")),
                    h4(htmlOutput("target_non"))
                ),
                
                box(title = "Total Sample Size", status = "warning",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    # Display total sample size required
                    h5(htmlOutput("text_total")), br(), 
                    h4(htmlOutput("required_total")),
                    h4(htmlOutput("target_total"))
                )
              )
      ),
      tabItem(tabName = "allocation_d1",
              fluidPage(
                fluidRow(
                  box(title = "Allocation of sample clinics across laboratories", width = 12, status = "warning", 
                      solidHeader = TRUE, div(tableOutput("allocation_d1"), style = "font-size: 120%")))
              )
      ),
      tabItem(tabName = "inputs",
              useShinyjs(),
              fluidPage(
                fluidRow(
                  box(title = "Number of clinics sampled for each laboratory", width = 12, 
                      status = "warning", solidHeader = TRUE,
                      uiOutput("num_labs_d2"), br(),
                      htmlOutput("text_hot_d2"), br(),
                      rHandsontableOutput("hot_d2")),
                  box(title = "Input the total number of DTG and non-DTG eligible case specimens for each clinic", 
                      status = "primary", width = 12, solidHeader = TRUE,
                      htmlOutput("text_table"), br(),
                      htmlOutput("additional_text"), br(),
                      rHandsontableOutput("hot"))
                ),
                div(style = "text-align:center", actionButton("calculate", "Submit", width = '150px', 
                                                              style = "font-weight: bold; border-color: black; font-size: 18px"))
              )
      ),
      tabItem(tabName = "allocation",
              fluidPage(
                fluidRow(
                  box(title = "Allocation of sample sizes across clinics", width = 12, status = "warning", 
                      solidHeader = TRUE, div(tableOutput("allocation"), style = "font-size: 120%")))
              )
      )
    )
    
    
  )
  # 
  # # Change sidebar color to green
  # tags$head(tags$style(HTML(".skin-blue .main-sidebar {background-color: #2FA584;}")))
)


#################################
# Define Server code
#################################

server <- function(input, output) {
  
  values <- reactiveValues()
  num_labs <- reactive({as.integer(input$num_labs_d1)})
  
  ## Handsontable for Phase 1
  table_d1 <- reactive({
    lab <- 1:num_labs()
    c_j <- rep(c(30,50,20), times = ceiling(num_labs()/3), len = num_labs())
    labName <- paste("Lab", as.character(lab))
    df <- data.frame(lab, labName, c_j, stringsAsFactors = FALSE)
    colnames(df) <- c("Laboratory", "Laboratory Name", "Number of Clinics")
    return(df)
  })
  output$hot_d1 <- renderRHandsontable({
    if (!is.null(table_d1())) {
      values[["DF"]] <- table_d1()
      out <- rhandsontable(values[["DF"]], stretchH = "all") %>%
        hot_validate_numeric(col = 3, min=0) %>%
        hot_col(col = 3, format = "0", halign = "htCenter") %>%
        hot_col(col = 2, halign = "htCenter") %>%
        hot_col(col = 1, readOnly = TRUE, halign = "htCenter")
      return(out)
    }
  })
  output$text_hot_d1 <- renderText({
    return(paste0("In the table below, input names for the laboratories in the <b> 'Laboratory Name' </b> 
    column by double-clicking on the cells. <br><br> In the <b> 'Number of Clinics' </b> column, 
    input the total number of clinics served by each laboratory. This must be a whole number."))
  })
  
  # user-inputted data in the table
  data_d1 <- reactive({
    if(is.null(input$hot_d1)) return(NULL)
    hot_to_r(input$hot_d1)
  })
  
  # FPC text
  output$fpc_text <- renderText({
    return("Would you like to apply the finite population correction to the ADR prevalence estimates?")
  })
  
  # =========================================================

  # q <- reactiveValues(VNS_DTG = NULL, VNS_non = NULL, VT_DTG = NULL, VT_non = NULL)
  
  table_historical <- reactive({
    if (input$historical_data_type == 1) {
      text <- c("", "", 
      "Number of individuals, in a recent three-month period, who were on an ART regimen for at least six months, underwent viral load testing, and had <strong>viral non-suppression</strong>. Must be a whole number.")
      DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "N<sup>h</sup><sub>VNS,DTG</sub>")
      DTG2 <- c("", "Input", 4000)
      non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "N<sup>h</sup><sub>VNS,nonDTG</sub>")
      non2 <- c("", "Input", 2500)
      df <- data.frame(text, DTG1, DTG2, non1, non2, stringsAsFactors = FALSE)
      return(df)
    } else if (input$historical_data_type == 2) {
      text <- c("", "", 
      "Number of individuals, in a recent three-month period, who were on an ART regimen for at least six months and underwent <strong>viral load testing</strong>. Must be a whole number.",
      "Anticipated proportion of those receiving viral load tests that are virally non-suppressed. Must be between 0 and 1.")
      DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "N<sup>h</sup><sub>VT,DTG</sub>",
                "q<sub>VNS,DTG</sub>")
      DTG2 <- c("", "Input", 13500, 0.3)
      non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "N<sup>h</sup><sub>VT,nonDTG</sub>",
                "q<sub>VNS,nonDTG</sub>")
      non2 <- c("", "Input", 8500, 0.3)
      df <- data.frame(text, DTG1, DTG2, non1, non2, stringsAsFactors = FALSE)
      return(df)
    } else {
      text <- c("", "", 
      "Number of individuals, in a recent three-month period, who were on an <strong>ART</strong> regimen for at least six months. Must be a whole number.",
      "Anticipated proportion of those on ART who receive viral load tests. Must be between 0 and 1.",
      "Anticipated proportion of those receiving viral load tests that are virally non-suppressed. Must be between 0 and 1.")
      DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "N<sup>h</sup><sub>ART,DTG</sub>",
                "q<sub>VT,DTG</sub", "q<sub>VNS,DTG</sub>")
      DTG2 <- c("", "Input", 19000, 0.7, 0.3)
      non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "N<sup>h</sup><sub>ART,nonDTG</sub>",
                "q<sub>VT,nonDTG</sub>", "q<sub>VNS,nonDTG</sub>")
      non2 <- c("", "Input", 10000, 0.7, 0.3)
      df <- data.frame(text, DTG1, DTG2, non1, non2, stringsAsFactors = FALSE)
      return(df)
    }
  })
  output$hot_historical <- renderRHandsontable({
    values[["DF_historical"]] <- table_historical()
    out <- rhandsontable(values[["DF_historical"]],  
                         rowHeaders = NULL, colHeaders = NULL,
                         allowedTags = "<sup><sub><strong>") %>%
      hot_col(col = 1:5, halign = "htCenter") %>%
      hot_cols(manualColumnResize = TRUE, colWidths = c(150, 50, 50, 50, 50)) %>%
      hot_row(row = 1:2, readOnly = TRUE) %>%
      hot_col(col = 1, readOnly = TRUE) %>%
      hot_col(col = c(1,2,4), readOnly = TRUE, renderer = "html") %>%
      hot_col(col = c(1,2,4), renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_table(customBorders = list(list(
        range = list(from = list(row = 2, col = 2),
                     to = list(row = nrow(values[["DF_historical"]])-1, col = 2)),
        top = list(width = 2, color = "orange"),
        left = list(width = 2, color = "orange"),
        bottom = list(width = 2, color = "orange"),
        right = list(width = 2, color = "orange")
      ), list(
        range = list(from = list(row = 2, col = 4),
                     to = list(row = nrow(values[["DF_historical"]])-1, col = 4)),
        top = list(width = 2, color = "orange"),
        left = list(width = 2, color = "orange"),
        bottom = list(width = 2, color = "orange"),
        right = list(width = 2, color = "orange")
      ))) %>%
      hot_table(mergeCells = list(
        list(row = 0, col = 1, rowspan = 1, colspan = 2),
        list(row = 0, col = 3, rowspan = 1, colspan = 2)
      ), stretchH = "all")
    return(out)
  })
  output$text_hot_historical <- renderText({
    return(paste0("In the table below, input the national-level historical data by 
                  double-clicking on the highlighted cells."))
  })
  
  # user-inputted data in the table
  data_historical <- reactive({
    if(is.null(input$hot_historical)) return(NULL)
    hot_to_r(input$hot_historical)
  })
  
  
  # # National-level historical data
  # output$historical_data <- renderUI({
  #   if(input$historical_data_type == 1) {
  #     div(numericInput("N_vns_DTG", label = HTML("<div id='mydiv'>Input the national number of individuals, 
  #                                 in a recent three-month period, who were on a <b>DTG-containing</b> ART regimen for at least six months, 
  #                                 underwent viral load testing, and had <b>viral non-suppression</b>, N<sup>h</sup><sub>VNS,DTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                      4500, min=0, step=1),
  #         numericInput("N_vns_non", label = HTML("<div id='mydiv'>Input the national number of individuals, 
  #                                 in a recent three-month period, who were on a <b>non-DTG-containing</b> ART regimen for at least six months, 
  #                                 underwent viral load testing, and had <b>viral non-suppression</b>, N<sup>h</sup><sub>VNS,nonDTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                      2500, min=0, step=1))
  #   } else if(input$historical_data_type == 2) {
  #     div(numericInput("N_vt_DTG", label = HTML("<div id='mydiv'>Input the national number of individuals, 
  #                                 in a recent three-month period, who were on a <b>DTG-containing</b> ART regimen for at least six months and 
  #                                 underwent <b>viral load testing</b>, N<sup>h</sup><sub>VT,DTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                      15000, min=0, step=1),
  #         numericInput("N_vt_non", label = HTML("<div id='mydiv'>Input the national number of individuals, 
  #                                 in a recent three-month period, who were on a <b>non-DTG-containing</b> ART regimen for at least six months and  
  #                                 underwent <b>viral load testing</b>, N<sup>h</sup><sub>VT,nonDTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                      8500, min=0, step=1),
  #         numericInput("q_vns_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
  #                                who are also virally non-suppressed, for patients on <b>DTG-containing</b> regimens, q<sub>VNS,DTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
  #         numericInput("q_vns_non", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
  #                                who are also virally non-suppressed, for patients on <b>non-DTG-containing</b> regimens, q<sub>VNS,nonDTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1))
  #   } else {
  #     div(numericInput("N_art_DTG", label = HTML("<div id='mydiv'>Input the national number of individuals, 
  #                                 in a recent three-month period, who were on a <b>DTG-containing ART regimen </b> for at least six months, 
  #                                 N<sup>h</sup><sub>ART,DTG</sub>. Must be a whole number.</div>"), 
  #                      21000, min=0, step=1),
  #         numericInput("N_art_non", label = HTML("<div id='mydiv'>Input the national number of individuals, 
  #                                 in a recent three-month period, who were on a <b>non-DTG-containing ART regimen </b>for at least six months, 
  #                                 N<sup>h</sup><sub>ART,nonDTG</sub>. Must be a whole number.</div>"), 
  #                      10000, min=0, step=1),
  #         numericInput("q_vt_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those on ART who 
  #                                 receive viral load tests, for patients on <b>DTG-containing</b> regimens, q<sub>VT,DTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1),
  #         numericInput("q_vt_non", label = HTML("<div id='mydiv'>Input the national the proportion of those on ART who 
  #                                 receive viral load tests, for patients on <b>non-DTG-containing</b> regimens, q<sub>VT,nonDTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1),
  #         numericInput("q_vns_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
  #                                who are also virally non-suppressed, for patients on <b>DTG-containing</b> regimens, q<sub>VNS,DTG</sub>.
  #                                Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
  #         numericInput("q_vns_non", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
  #                                who are also virally non-suppressed, for patients on <b>non-DTG-containing</b> regimens, q<sub>VNS,nonDTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1)
  #         )
  #   }
  # })
  

  # ========================= Server code for DTG-specific calculations =====================================
  
  N_DTG <- reactive({
    if (input$historical_data_type == 1) {
      return(ceiling(as.numeric(data_historical()[3,3]) * input$prop_complete))
    } else if (input$historical_data_type == 2) {
      return(ceiling(as.numeric(data_historical()[3,3]) * as.numeric(data_historical()[4,3]) * input$prop_complete))
    } else {
      return(ceiling(as.numeric(data_historical()[3,3]) * as.numeric(data_historical()[4,3]) * as.numeric(data_historical()[5,3]) * input$prop_complete))
    }
  })
  
  N_non <- reactive({
    if (input$historical_data_type == 1) {
      return(ceiling(as.numeric(data_historical()[3,5]) * input$prop_complete))
    } else if (input$historical_data_type == 2) {
      return(ceiling(as.numeric(data_historical()[3,5]) * as.numeric(data_historical()[4,5]) * input$prop_complete))
    } else {
      return(ceiling(as.numeric(data_historical()[3,5]) * as.numeric(data_historical()[4,5]) * as.numeric(data_historical()[5,5]) * input$prop_complete))
    }
  })
  
  output$anticipated_n <- renderText({
    return(paste0("Based on your inputs, the anticipated number of DTG and non-DTG eligible case specimens, respectively: ", 
                  N_DTG(), " and ", N_non()))
  })
  
  # # Toggle for finite/infinite eligible population
  # output$N_DTG <- renderUI({
  #   switch(input$inf_DTG,
  #          infinite = div(style = "text-align:center; width:87%;", br(), 
  #                         h5("No finite population correction will be used.")),
  #          finite = numericInput("N_DTG", h5("Input the total number of eligible case specimens from patients on a DTG-containing regimen, 
  #               nationally, during the study period. Must be a whole number."),
  #                                20000, min=1, step=1))
  # })
  
  # N_DTG_vect <- reactive({
  #   sapply(1:as.integer(input$num_labs), function(i) {
  #     data()[i, 3]
  #     # as.integer(input[[paste0("N_DTG", i)]])[1]
  #   })
  # })
  # 
  # N_non_vect <- reactive({
  #   sapply(1:as.integer(input$num_labs), function(i) {
  #     data()[i, 4]
  #     # as.integer(input[[paste0("N_non", i)]])[1]
  #   })
  # })
  
  # N_DTG <- reactive({
  #   if (input$historical_data_type == 1) {
  #     return(ceiling(input$N_vns_DTG * input$prop_complete))
  #   } else if (input$historical_data_type == 2) {
  #     return(ceiling(input$N_vt_DTG * input$q_vns_DTG * input$prop_complete))
  #   } else {
  #     return(ceiling(input$N_art_DTG * input$q_vns_DTG * input$q_vt_DTG * input$prop_complete))
  #   }
  # })
  
  # test <- reactive({data_historical()[3,3]})
  # output$test <- renderText({ paste0(test())})
  # output$test <- renderText({paste(N_DTG(), N_non())})
  
  ### Define variables and functions
  prev_DTG <- 0.035
  CI_DTG <- prec_DTG <- 0.02
  alpha_DTG <- 0.05
  labFail_DTG <- 0.3
  DE_DTG <- 1.5
  # N_DTG <- reactive({ifelse(input$fpc_DTG == 1, sum(N_DTG_vect()), Inf)})
  # N_non <- reactive({ifelse(input$fpc_non == 1, sum(N_non_vect()), Inf)})
  
  # Function to calculate sample sizes using FPC and Wald-type intervals
  calc_sample_size <- function(alpha, prev, N, CI, DE, labFail) {
    if (N == Inf) {  # infinite population
      nEff <- qnorm(1-alpha/2)^2*prev*(1-prev) / (CI^2)
      n <- ceiling(nEff)
      m <- ceiling(n*DE/(1-labFail))
      return(list(n, m))
    } else if (N < 0 | N %% 1 != 0) {  # check positive integer
      return(list("NA. Population sizes must be positive integers.", "NA. Population sizes must be positive integers."))
    } else {  # finite population
      nEff <- qnorm(1-alpha/2)^2*prev*(1-prev)*N / 
        (N*CI^2 + (prev*(1-prev)*qnorm(1-alpha/2)^2))
      n <- min(N, ceiling(nEff))
      m <- min(N, ceiling(n*DE/(1-labFail)))  # adjust for genotyping failure and DE
      return(list(n, m))
    }
  }
  
  # Calculate sample size
  sampleSize_DTG <- reactive({
    if (input$fpc == 1) {
      calc_sample_size(alpha_DTG, prev_DTG, N_DTG(), CI_DTG, DE_DTG, labFail_DTG)
    } else {
      calc_sample_size(alpha_DTG, prev_DTG, Inf, CI_DTG, DE_DTG, labFail_DTG)
    }
  })
  n_DTG <- reactive({sampleSize_DTG()[[1]]})
  m_DTG <- reactive({sampleSize_DTG()[[2]]})
  # sampleSize_DTG <- eventReactive(input$calculate, {calc_sample_size(alpha_DTG, prev_DTG, N_DTG(), 
  #                                                                 CI_DTG, DE_DTG, labFail_DTG)})
  
  # Table of user-specified parameter values
  assumptions_DTG <- reactive({
    params <- data.frame(
                        Assumptions = c("Expected prevalence of DTG-specific resistance", 
                                        "Desired absolute precision (95% CI half-width)",
                                        "Population Size",
                                        "Significance Level",
                                        "Design Effect",
                                        "Genotyping Failure Rate"),
                        Value = as.character(c(paste0(prev_DTG*100, "%"),
                                               paste0("\u00B1", prec_DTG*100, "%"),
                                               N_DTG(),
                                               alpha_DTG,
                                               DE_DTG,
                                               paste0(labFail_DTG*100, "%"))),
                        stringsAsFactors = FALSE)
    return(params)
  })
  
  # Render table
  output$values_DTG <- renderTable({assumptions_DTG()})
  
  # Sample size output
  output$required_DTG <- renderText({
    return(paste0("Required sample size, n<sub>DTG</sub>: ", 
           a(sampleSize_DTG()[[1]], style = "color:red")))
    
  })
  
  output$target_DTG <- renderText({
    return(paste0("Target sample size (adjusted for design effect and genotyping failure), m<sub>DTG</sub>: ", 
           a(sampleSize_DTG()[[2]], style = "color:red")))
  })
  
  output$text_DTG <- renderText({
    return("Sample sizes necessary for estimating the prevalence of DTG-specific resistance
    among patients taking DTG-containing regimens.")
  })
  
  
  # ==================================== Server code for overall calculations ================================
  
  # # Toggle for finite/infinite eligible population
  # output$N_O <- renderUI({
  #   switch(input$inf_O,
  #          infinite = div(style = "text-align:center; width:87%;", br(), 
  #                         h5("No finite population correction will be used.")),
  #          finite = numericInput("N_O", h5("Input the total number of eligible case specimens from patients on all regimens, 
  #                        nationally, during the study period. Must be a whole number."), 
  #                                100000, min=1, step=1))
  # })
  
  ### Define variables
  prev_O <- 0.5
  CI_O <- prec_O <- 0.06
  alpha_O <- 0.05
  labFail_O <- 0.3
  DE_O <- 1.5
  N_O <- reactive({
    N_DTG() + N_non()
  })
  
  # Calculate sample size
  sampleSize_O <- reactive({
    if (input$fpc == 1) {
      calc_sample_size(alpha_O, prev_O, N_O(), CI_O, DE_O, labFail_O)
    } else {
      calc_sample_size(alpha_O, prev_O, Inf, CI_O, DE_O, labFail_O)
    } 
  })
    
  # sampleSize_O <- eventReactive(input$calculate,{calc_sample_size(alpha_O, prev_O, N_O(), 
  #                                                              CI_O, DE_O, labFail_O)})
  
  # Table of user-specified parameter values
  assumptions_O <- reactive({
    df <- data.frame(
      Assumptions = c("Expected prevalence of overall drug resistance", 
                      "Desired absolute precision (95% CI half-width)",
                      "Population Size",
                      "Significance Level",
                      "Design Effect",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_O*100, "%"),
                             paste0("\u00B1", prec_O*100, "%"),
                             N_O(),
                             alpha_O,
                             DE_O,
                             paste0(labFail_O*100, "%"))),
      stringsAsFactors = FALSE)
    return(df)
  })
  
  # Render table
  output$values_O <- renderTable({assumptions_O()})
  
  # Sample size output
  output$required_O <- renderText({
    return(paste0("Required sample size, n<sub>overall</sub>: ", 
           a(sampleSize_O()[[1]], style = "color:red")))
  })
  
  output$target_O <- renderText({
    return(paste0("Target sample size (adjusted for design effect and genotyping failure), m<sub>overall</sub>: ", 
           a(sampleSize_O()[[2]], style = "color:red")))
  })
  
  output$text_O <- renderText({
    return("Sample sizes necessary for estimating the prevalence of overall ADR among all patients.")
  })
  
  
  
  # ===================== Server code for non-DTG and total calculations ==================================
  
  # # Toggle for prop_nonDTG user input
  # infinite_toggle <- reactive({ list(input$inf_O, input$inf_DTG)})
  # 
  # observeEvent(infinite_toggle(), {
  #   if(input$inf_O=="infinite" | input$inf_DTG=="infinite") {
  #     shinyjs::show(id = "title_prop_nonDTG")
  #     shinyjs::show("prop_nonDTG")
  #   } else {
  #     shinyjs::hide(id = "title_prop_nonDTG")
  #     shinyjs::hide("prop_nonDTG")
  #   }
  # })
  
  prop_nonDTG <- reactive({min(max(0, N_non() / N_O()), 1)})
  
  # # Set prop_nonDTG variable to be user-specified if infinite populations are used
  # prop_nonDTG <- reactive({
  #   if (input$inf_DTG == "finite" & input$inf_O == "finite") {
  #     1 - min(max(0, input$N_DTG / input$N_O), 1)
  #   } else {
  #     input$prop_nonDTG
  #   }
  # })
  
  ## Calculate n_nonDTG, m_nonDTG, n_total, m_total
  n_nonDTG <- reactive({
    # if (prop_nonDTG() < 0 | prop_nonDTG() > 1) {
    #   return("NA. Proportion non-DTG must be between 0 and 1.")
    # } 
    if (N_DTG() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    if (N_non() < 0 | N_non() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    return(ceiling(sampleSize_O()[[1]] * prop_nonDTG()))
  })
  
  m_nonDTG <- reactive({
    if (N_DTG() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    if (N_non() < 0 | N_non() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    return(ceiling(n_nonDTG()*DE_O / (1 - labFail_O)))
  })
  
  n_total <- reactive({
    if (N_DTG() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    if (N_non() < 0 | N_non() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    return(sampleSize_DTG()[[1]] + ceiling(sampleSize_O()[[1]] * prop_nonDTG()))
  })
  
  m_total <- reactive({
    if (N_DTG() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    if (N_non() < 0 | N_non() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    return(sampleSize_DTG()[[2]] + m_nonDTG())
  })
  
  
  # Output text for non-DTG sample sizes
  output$required_non <- renderText({
    return(paste0("Required sample size, n<sub>nonDTG</sub>: ", 
           a(n_nonDTG(), style = "color:red")))
  })
  
  output$target_non <- renderText({
    return(paste0("Target sample size (adjusted for design effect and genotyping failure), m<sub>nonDTG</sub>: ", 
           a(m_nonDTG(), style = "color:red")))
  })
  
  output$text_non <- renderText({
    return("Sample sizes necessary from patients taking non-DTG-containing regimens to ensure sufficient sample 
    size for overall estimate of ADR.")
  })
  
  output$prop_non <- renderText({
    return(paste0("Proportion of eligible case specimens from patients on non-DTG-containing regimens, 
           prop<sub>nonDTG</sub>: ",
           a(round(prop_nonDTG(), 2), style = "color:blue")))
  })
  
  # Output text for total sample sizes
  output$required_total <- renderText({
    return(paste0("Total required sample size, n<sub>DTG</sub> + n<sub>nonDTG</sub>: ", 
           a(n_total(), style = "color:red")))
  })
  
  output$target_total <- renderText({
    return(paste0("Total target sample size (adjusted for design effect and genotyping failure), m<sub>DTG</sub> + m<sub>nonDTG</sub>: ", 
           a(m_total(), 
             style = "color:red")))
  })
  
  output$text_total <- renderText({
    return("Total sample sizes necessary for both the DTG and overall estimates.")
  })
  
  # ======================== Server code for determining the minimum number of clinics to sample ===================
  
  # Description text
  output$historical_clinic_text <- renderText({
    return("Do you have historical clinic-level data to calculate median clinic sizes?")
  })
  
  # Table for clinic data input
  table_clinic <- reactive({
    if (input$clinic_data_type == 1) {
      text <- c("", "", "Median number of individuals per clinic, in a recent three-month period, who were on an ART regimen for at least six months, underwent viral load testing, and had <strong>viral non-suppression</strong>. Must be a whole number.")
      DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>VNS,DTG</sub>")
      DTG2 <- c("", "Input", 40)
      non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>VNS,nonDTG</sub>")
      non2 <- c("", "Input", 25)
      df <- data.frame(text, DTG1, DTG2, non1, non2, stringsAsFactors = FALSE)
      return(df)
    } else if (input$clinic_data_type == 2) {
      if (input$historical_data_type == 1) {
        text <- c("", "", "Median number of individuals per clinic, in a recent three-month period, who were on an ART regimen for at least six months and underwent <strong>viral load testing</strong>. Must be a whole number.",
                  "Anticipated proportion of those receiving viral load tests that are virally non-suppressed. Must be between 0 and 1.")
        DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>VT,DTG</sub>",
                  "q<sub>VNS,DTG</sub>")
        DTG2 <- c("", "Input", 135, 0.3)
        non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>VT,nonDTG</sub>",
                  "q<sub>VNS,nonDTG</sub>")
        non2 <- c("", "Input", 85, 0.3)
      } else {
        text <- c("", "", "Median number of individuals per clinic, in a recent three-month period, who were on an ART regimen for at least six months and underwent <strong>viral load testing</strong>. Must be a whole number.")
        DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>VT,DTG</sub>")
        DTG2 <- c("", "Input", 135)
        non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>VT,nonDTG</sub>")
        non2 <- c("", "Input", 85)
      }  
      df <- data.frame(text, DTG1, DTG2, non1, non2, stringsAsFactors = FALSE)
      return(df)
    } else {
      if (input$historical_data_type == 1) {
        text <- c("", "", "Median number of individuals per clinic, in a recent three-month period, who were on an <strong>ART</strong> regimen for at least six months. Must be a whole number.",
                  "Anticipated proportion of those on ART who receive viral load tests. Must be between 0 and 1.",
                  "Anticipated proportion of those receiving viral load tests that are virally non-suppressed. Must be between 0 and 1.")
        DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>ART,DTG</sub>",
                  "q<sub>VT,DTG</sub", "q<sub>VNS,DTG</sub>")
        DTG2 <- c("", "Input", 190, 0.7, 0.3)
        non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>ART,nonDTG</sub>",
                  "q<sub>VT,nonDTG</sub>", "q<sub>VNS,nonDTG</sub>")
        non2 <- c("", "Input", 120, 0.7, 0.3)
      } else if (input$historical_data_type == 2) {
        text <- c("", "", "Median number of individuals per clinic, in a recent three-month period, who were on an <strong>ART</strong> regimen for at least six months. Must be a whole number.",
                  "Anticipated proportion of those on ART who receive viral load tests. Must be between 0 and 1.")
        DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>ART,DTG</sub>",
                  "q<sub>VT,DTG</sub")
        DTG2 <- c("", "Input", 190, 0.7)
        non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>ART,nonDTG</sub>",
                  "q<sub>VT,nonDTG</sub>")
        non2 <- c("", "Input", 120, 0.7)
      } else {
        text <- c("", "", "Median number of individuals per clinic, in a recent three-month period, who were on an <strong>ART</strong> regimen for at least six months. Must be a whole number.")
        DTG1 <- c("Those on <strong>DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>ART,DTG</sub>")
        DTG2 <- c("", "Input", 190)
        non1 <- c("Those on <strong>non-DTG</strong>-containing regimens", "Notation in the protocol", "M<sup>h</sup><sub>ART,nonDTG</sub>")
        non2 <- c("", "Input", 120)
      }
      
      df <- data.frame(text, DTG1, DTG2, non1, non2, stringsAsFactors = FALSE)
      return(df)
    }
  })
  output$hot_clinic <- renderRHandsontable({
    values[["DF_clinic"]] <- table_clinic()
    out <- rhandsontable(values[["DF_clinic"]], 
                         rowHeaders = NULL, colHeaders = NULL,
                         allowedTags = "<sup><sub><strong>") %>%
      hot_cols(manualColumnResize = TRUE, colWidths = c(150, 50, 50, 50, 50)) %>%
      hot_row(row = 1:2, readOnly = TRUE) %>%
      hot_col(col = c(2,3,4,5), halign = "htCenter") %>%
      hot_col(col = 1, readOnly = TRUE) %>%
      hot_col(col = c(1,2,4), readOnly = TRUE, renderer = "html") %>%
      hot_col(col = c(1,2,4), renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_table(customBorders = list(list(
        range = list(from = list(row = 2, col = 2),
                     to = list(row = nrow(table_clinic())-1, col = 2)),
        top = list(width = 2, color = "green"),
        left = list(width = 2, color = "green"),
        bottom = list(width = 2, color = "green"),
        right = list(width = 2, color = "green")
      ), list(
        range = list(from = list(row = 2, col = 4),
                     to = list(row = nrow(table_clinic())-1, col = 4)),
        top = list(width = 2, color = "green"),
        left = list(width = 2, color = "green"),
        bottom = list(width = 2, color = "green"),
        right = list(width = 2, color = "green")
      ))) %>%
      hot_table(mergeCells = list(
        list(row = 0, col = 1, rowspan = 1, colspan = 2),
        list(row = 0, col = 3, rowspan = 1, colspan = 2)
      ), stretchH = "all")
    return(out)
  })
  output$text_hot_clinic <- renderText({
    return(paste0("In the table below, input the clinic-level historical data by 
                  double-clicking on the highlighted cells."))
  })
  
  # user-inputted data in the table
  data_clinic <- reactive({
    if(is.null(input$hot_clinic)) return(NULL)
    hot_to_r(input$hot_clinic)
  })
  
  
  # # Clinic data input
  # output$clinic_data <- renderUI({
  #   
  #   if (input$clinic_data_type == 1) {
  #         div(numericInput("M_vns_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>DTG-containing</b> ART regimen for at least six months, 
  #                                 underwent viral load testing, and had <b>viral non-suppression</b>, in a recent three-month period,  
  #                                 M<sup>h</sup><sub>VNS,DTG</sub>. Must be a whole number.</div>"), 
  #                               40, min=0, step=1),
  #                  numericInput("M_vns_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>non-DTG-containing</b> ART regimen for at least six months, 
  #                                 underwent viral load testing, and had <b>viral non-suppression</b>, in a recent three-month period,
  #                                 M<sup>h</sup><sub>VNS,nonDTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                               25, min=0, step=1))
  #   } else if (input$clinic_data_type == 2) {
  #       if (input$historical_data_type == 1){
  #         div(numericInput("M_vt_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>non-DTG-containing</b> ART regimen for at least six months and 
  #                                 underwent <b>viral load testing</b>, in a recent three-month period, M<sup>h</sup><sub>VT,DTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                          130, min=0, step=1),
  #             numericInput("M_vt_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>non-DTG-containing</b> ART regimen for at least six months and  
  #                                 underwent <b>viral load testing</b>, in a recent three-month period, M<sup>h</sup><sub>VT,nonDTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                          85, min=0, step=1),
  #             numericInput("q_vns_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
  #                                who are also virally non-suppressed, for patients on <b>DTG-containing</b> regimens, q<sub>VNS,DTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
  #             numericInput("q_vns_non", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
  #                                who are also virally non-suppressed, for patients on <b>non-DTG-containing</b> regimens, q<sub>VNS,nonDTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1))
  #       } else {
  #         div(numericInput("M_vt_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>non-DTG-containing</b> ART regimen for at least six months and 
  #                                 underwent <b>viral load testing</b>, in a recent three-month period, M<sup>h</sup><sub>VT,DTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                          130, min=0, step=1),
  #             numericInput("M_vt_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>non-DTG-containing</b> ART regimen for at least six months and  
  #                                 underwent <b>viral load testing</b>, in a recent three-month period, M<sup>h</sup><sub>VT,nonDTG</sub>. 
  #                                 Must be a whole number.</div>"), 
  #                          85, min=0, step=1))
  #       }
  #   } else {
  #     if (input$historical_data_type == 1) {
  #       div(numericInput("M_art_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>DTG-containing ART</b> regimen for at least six months, 
  #                                 in a recent three-month period, M<sup>h</sup><sub>ART,DTG</sub>. Must be a whole number.</div>"), 
  #                        190, min=0, step=1),
  #           numericInput("M_art_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>non-DTG-containing ART</b> regimen for at least six months, 
  #                                 in a recent three-month period, M<sup>h</sup><sub>ART,nonDTG</sub>. Must be a whole number.</div>"), 
  #                        120, min=0, step=1),
  #           numericInput("q_vt_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those on ART who 
  #                                 receive viral load tests, for patients on <b>DTG-containing</b> regimens, q<sub>VT,DTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1),
  #           numericInput("q_vt_non", label = HTML("<div id='mydiv'>Input the national the proportion of those on ART who 
  #                                 receive viral load tests, for patients on <b>non-DTG-containing</b> regimens, q<sub>VT,nonDTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1),
  #           numericInput("q_vns_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
  #                                who are also virally non-suppressed, for patients on <b>DTG-containing</b> regimens, q<sub>VNS,DTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
  #           numericInput("q_vns_non", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
  #                                who are also virally non-suppressed, for patients on <b>non-DTG-containing</b> regimens, q<sub>VNS,nonDTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1)
  #           )
  #     } else if (input$historical_data_type == 2) {
  #       div(numericInput("M_art_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>DTG-containing ART</b> regimen for at least six months, 
  #                                 in a recent three-month period, M<sup>h</sup><sub>ART,DTG</sub>. Must be a whole number.</div>"), 
  #                        190, min=0, step=1),
  #           numericInput("M_art_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>non-DTG-containing ART</b> regimen for at least six months, 
  #                                 in a recent three-month period, M<sup>h</sup><sub>ART,nonDTG</sub>. Must be a whole number.</div>"), 
  #                        120, min=0, step=1),
  #           numericInput("q_vt_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those on ART who 
  #                                 receive viral load tests, for patients on <b>DTG-containing</b> regimens, q<sub>VT,DTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1),
  #           numericInput("q_vt_non", label = HTML("<div id='mydiv'>Input the national the proportion of those on ART who 
  #                                 receive viral load tests, for patients on <b>non-DTG-containing</b> regimens, q<sub>VT,nonDTG</sub>. 
  #                                Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1))
  #     } else {
  #       div(numericInput("M_art_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>DTG-containing ART</b> regimen for at least six months, 
  #                                 in a recent three-month period, M<sup>h</sup><sub>ART,DTG</sub>. Must be a whole number.</div>"), 
  #                        190, min=0, step=1),
  #           numericInput("M_art_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
  #                                 who were on a <b>non-DTG-containing ART</b> regimen for at least six months, 
  #                                 in a recent three-month period, M<sup>h</sup><sub>ART,nonDTG</sub>. Must be a whole number.</div>"), 
  #                        120, min=0, step=1))
  #     }
  #   } 
  # })
  avg_clinics <- reactive({
    if (input$historical_clinic == 2) {
      total <- sum(as.integer(data_d1()[,3]))
      mean_DTG <- round(N_DTG() / total, 1)
      mean_non <- round(N_non() / total, 1)
      return(list(mean_DTG, mean_non))
    }
    return(list(0, 0))
  })
  num_sample_clinics <- reactive({
    if(input$historical_clinic == 2) {
      total <- sum(as.integer(data_d1()[,3]))
      ceiling(max(total*m_DTG() / N_DTG(), total*m_nonDTG() / N_non()))
    } else {
      if(input$clinic_data_type == 1) {
        ceiling(max(m_DTG() / (as.numeric(data_clinic()[3,3])*input$prop_complete), m_nonDTG() / (as.numeric(data_clinic()[3,5])*input$prop_complete)))
      } 
      else if(input$clinic_data_type == 2) {
        if (input$historical_data_type == 1) {
          denom_DTG <- as.numeric(data_clinic()[3,3]) * as.numeric(data_clinic()[4,3]) * input$prop_complete
          denom_nonDTG <- as.numeric(data_clinic()[3,5]) * as.numeric(data_clinic()[4,5]) * input$prop_complete
        } else if (input$historical_data_type == 2) {
          denom_DTG <- as.numeric(data_clinic()[3,3]) * as.numeric(data_historical()[4,3]) * input$prop_complete
          denom_nonDTG <- as.numeric(data_clinic()[3,5]) * as.numeric(data_historical()[4,5]) * input$prop_complete
        } else {
          denom_DTG <- as.numeric(data_clinic()[3,3]) * as.numeric(data_historical()[5,3]) * input$prop_complete
          denom_nonDTG <- as.numeric(data_clinic()[3,5]) * as.numeric(data_historical()[5,5]) * input$prop_complete
        }
        ceiling(max(m_DTG() / denom_DTG, m_nonDTG() / denom_nonDTG))
      } else {
        if (input$historical_data_type == 1) {
          denom_DTG <- as.numeric(data_clinic()[3,3]) * as.numeric(data_clinic()[4,3])* as.numeric(data_clinic()[5,3]) * input$prop_complete
          denom_nonDTG <- as.numeric(data_clinic()[3,5]) * as.numeric(data_clinic()[4,5])* as.numeric(data_clinic()[5,5]) * input$prop_complete
        } else if (input$historical_data_type == 2) {
          denom_DTG <- as.numeric(data_clinic()[3,3]) * as.numeric(data_clinic()[4,3])* as.numeric(data_historical()[4,3]) * input$prop_complete
          denom_nonDTG <- as.numeric(data_clinic()[3,5]) * as.numeric(data_clinic()[4,5])* as.numeric(data_historical()[4,5]) * input$prop_complete
        } else {
          denom_DTG <- as.numeric(data_clinic()[3,3]) * as.numeric(data_historical()[4,3])* as.numeric(data_historical()[5,3]) * input$prop_complete
          denom_nonDTG <- as.numeric(data_clinic()[3,5]) * as.numeric(data_historical()[4,5])* as.numeric(data_historical()[5,5]) * input$prop_complete
        }
        ceiling(max(m_DTG() / denom_DTG, m_nonDTG() / denom_nonDTG))
      }
    }
  })
  # num_sample_clinics <- reactive({
  #   if(input$historical_clinic == 2) {
  #     total <- sum(as.integer(data_d1()[,3]))
  #     ceiling(max(total*m_DTG() / N_DTG(), total*m_nonDTG() / N_non()))
  #   } else {
  #     if(input$clinic_data_type == 1) {
  #       ceiling(max(m_DTG() / (input$M_vns_DTG*input$prop_complete), m_nonDTG() / (input$M_vns_non*input$prop_complete)))
  #     } 
  #     else if(input$clinic_data_type == 2) {
  #       denom <- input$M_vt_DTG * input$q_vns_DTG * input$prop_complete
  #       ceiling(max(m_DTG() / denom, m_nonDTG() / denom))
  #     } else {
  #       denom <- input$M_art_DTG * input$q_vt_DTG * input$q_vns_DTG * input$prop_complete
  #       ceiling(max(m_DTG() / denom, m_nonDTG() / denom))
  #     }
  #   }
  # })
  output$mean_clinics <- renderText({
    return(paste0("Mean anticipated number of DTG eligible case specimens per clinic: ", avg_clinics()[[1]], 
           ". <br>Mean anticipated number of non-DTG eligible case specimens per clinic: ", avg_clinics()[[2]], "."))
  })
  
  output$min_clinics <- renderText({
    return(paste0("Based on your inputs, the minimum number of clinics recommended to be sampled, c: ", 
                  a(num_sample_clinics(), style = "color:red; font-size: 20px")))
  })
  
  ## text for number of clinics to be sampled
  output$num_clinics_text <- renderText({
    return("Input the number of clinics that will be sampled. Must be a whole number.")
  })
  
  output$num_clinics <- renderUI({
    numericInput("num_clin_samp", 
                 label = h5(HTML("The default will be the recommended minimum number of clinics to be sampled, determined above. 
                            Sampling more clinics is preferred if the budget can accommodate. Sampling fewer clinics than the recommended amount 
                            may lead to increased variability and a larger risk of not achieving the target sample sizes. 
                            <br> <br>Note that a minimum of <strong>two</strong> clinics must be sampled from all laboratories serving two or more clinics 
                            in order to ensure estimation of within-laboratory variation for the ADR prevalence estimates. ")),
                 num_sample_clinics(), min=1, step = 1)
  })
  
  # ======================================= Allocation Across Labs ===================================================
  # Allocation of sampled clinics for each laboratory
  # c_j = c*C_j/C = input$num_clin_samp * data_d1()[j,3] / total
  allocation_table_d1 <- eventReactive(input$submit, {
    total <- sum(as.integer(data_d1()[,3]))
    df <- data.frame(
      labs = c(data_d1()[,2], "Total"),
      num_clinics <- c(data_d1()[,3], total),
      samp_clinics = c(sapply(1:num_labs(), function(i) {round(input$num_clin_samp*data_d1()[i,3]/total)}), 
                       sum(round(input$num_clin_samp*data_d1()[,3]/total))) 
    )
    colnames(df) <- c("Laboratory", "Number of Clinics", "Number of Clinics to Sample")
    return(df)
  })
  
  # Render table
  output$allocation_d1 <- renderTable({allocation_table_d1()}, digits = 0, striped = TRUE, width="100%", spacing = "l")
  
  # ====================================== Reactive Table for Phase 2 ================================================
  
  output$num_labs_d2 <- renderUI({
    numericInput("num_labs_d2", label = h4("How many viral load laboratories were clinics sampled from?"), 
                 num_labs(), min=1, step=1)
  })
  
  num_labs2 <- reactive({input$num_labs_d2})
  
  ## Handsontable for re-inputting labs and clinics sampled per lab
  table_d2 <- reactive({
    default_n <- nrow(allocation_table_d1()) -1
    lab <- 1:num_labs2()
    c_j <- rep(allocation_table_d1()[1:default_n, 3], times = ceiling(num_labs2()/default_n), len = num_labs2())
    labName <- paste("Lab", as.character(lab))
    df <- data.frame(lab, labName, c_j, stringsAsFactors = FALSE)
    colnames(df) <- c("Laboratory", "Laboratory Name", "Number of Clinics Sampled")
    return(df)
  })
  output$hot_d2 <- renderRHandsontable({
    if (!is.null(table_d2())) {
      values[["DF_d2"]] <- table_d2()
      out <- rhandsontable(values[["DF_d2"]], stretchH = "all") %>%
        hot_validate_numeric(col = 3, min=0) %>%
        hot_col(col = 3, format = "0", halign = "htCenter") %>%
        hot_col(col = 2, halign = "htCenter") %>%
        hot_col(col = 1, readOnly = TRUE, halign = "htCenter")
      return(out)
    }  
  })
  output$text_hot_d2 <- renderText({
    return(paste0("In the table below, input names for the laboratories in the <b> 'Laboratory Name' </b> 
    column by double-clicking on the cells. <br><br> In the <b> 'Number of Clinics Sampled' </b> column, 
    input the number of clinics sampled per laboratory. This must be a whole number, and at least two 
                  clinics must be sampled from all laboratories serving two or more clinics. Defaults determined 
                  in Phase 1 have been carried over but can be changed."))
  })
  
  # user-inputted data in the table
  data_d2 <- reactive({
    if(is.null(input$hot_d2)) return(NULL)
    hot_to_r(input$hot_d2)
  })
  
  ## Handsontable for Phase 2 inputting eligible case specimens
  table <- reactive({
    if (!is.null(data_d2())) {
      labName = unlist(sapply(1:num_labs2(), function(i) {rep(data_d2()[i, 2], data_d2()[i, 3])}))
      clinic = unlist(sapply(1:num_labs2(), function(i) { as.character(1:data_d2()[i,3])}))
      clinicName <- paste("Clinic", clinic)
      set.seed(1)
      rand1 <- runif(length(clinic))*100
      default_DTG <- round(rand1/sum(rand1)*944)
      set.seed(2)
      rand2 <- runif(length(clinic))*100
      default_nonDTG <- round(rand2 / sum(rand2)*945)
      
      df <- data.frame(labName, clinic, clinicName, default_DTG, default_nonDTG, stringsAsFactors = FALSE)
      colnames(df) <- c("Laboratory Name", "Clinic", "Clinic Name",
                        "DTG Eligible Case Specimens", "Non-DTG Eligible Case Specimens")
      return(df)
    }
  })
  
  output$text_table <- renderText({
    return(paste0("In the table below, double-click on the cells to input the appropriate values. <br> <br> 
    In the <b> 'DTG Eligible Case Specimens' </b> column, 
    input the total number of eligible case specimens<sup>&#8224</sup> from 
    patients on a <b>DTG-containing regimen</b> from each clinic during the study period. 
    This must be a whole number. <br> <br> In the <b> 'Non-DTG Eligible Case Specimens'</b> column, 
    input the total number of eligible case specimens from patients on a 
    <b>non-DTG-containing regimen </b> from each clinic during the study period. 
    This must be a whole number."))
  })
  
  output$additional_text <- renderText({
    return("<sup>&#8224</sup> A case specimen is defined as a remnant specimen with viral load &#8805 1000 copies/mL obtained during 
    the survey period from an individual with all four required survey variables.")
  })
  
  output$hot <- renderRHandsontable({
    if (!is.null(table())) {
      values[["DF2"]] <- table()
      rtable <- rhandsontable(values[["DF2"]], stretchH = "all") %>%
          hot_validate_numeric(col = 4, min=0) %>%
          hot_validate_numeric(col = 5, min=0) %>%
          hot_col(col = 4, format = "0", halign = "htCenter") %>%
          hot_col(col = 5, format = "0", halign = "htCenter") %>%
          hot_col(col = 1, readOnly = TRUE, halign = "htCenter") %>%
          hot_col(col = 2, halign = "htCenter") %>%
          hot_col(col = 3, halign = "htCenter")
      return(rtable)
    }  
  })
  
  data <- reactive({
    if(is.null(input$hot)) return(NULL)
    hot_to_r(input$hot)
  })
  
  # ======================================= Allocation Across Clinics =================================================
  
  # Table of user-specified parameter values
  allocation_table <- eventReactive(input$calculate, {
    N_DTG_d2 <- sum(data()[,4])
    N_non_d2 <- sum(data()[,5])
    df <- data.frame(
      labs = c(data()[,1], "Total"),
      clinics = c(data()[,3], ""),
      DTG  = c(sapply(1:nrow(data()), function(i) {data()[i, 4]}), N_DTG_d2),
      non = c(sapply(1:nrow(data()), function(i) {data()[i, 5]}), N_non_d2),
      target_DTG = c(sapply(1:nrow(data()), function(i) {ceiling(m_DTG()*data()[i, 4]/N_DTG_d2)}), 
                     sum(ceiling(m_DTG()*data()[, 4]/N_DTG_d2))),
      target_non = c(sapply(1:nrow(data()), function(i) {ceiling(m_nonDTG()*data()[i, 5]/N_non_d2)}), 
                     sum(ceiling(m_nonDTG()*data()[, 5]/N_non_d2))),
      total = c(sapply(1:nrow(data()), function(i) {
        ceiling(m_DTG()*data()[i, 4]/N_DTG_d2) + ceiling(m_nonDTG()*data()[i, 5]/N_non_d2)
      }), sum(ceiling(m_DTG()*data()[, 4]/N_DTG_d2) + ceiling(m_nonDTG()*data()[, 5]/N_non_d2)))
    )
    colnames(df) <- c("Laboratory", "Clinic", "DTG Eligible Case Specimens", "Non-DTG Eligible Case Specimens", "Target DTG Sample Size", 
                      "Target Non-DTG Sample Size", "Total Recommended Sample Size")
    return(df)
  })
  
  # Render table
  output$allocation <- renderTable({allocation_table()}, digits = 0, striped = TRUE, width="100%", spacing = "l")
  
}


############################
# Build Shiny app
############################

shinyApp(ui, server)

