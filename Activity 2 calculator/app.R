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
                  box(title = "Total number of clinics in each laboratory's catchment area.", width = 12, 
                      status = "warning", solidHeader = TRUE,
                      numericInput("num_labs_d1", label = h4("How many viral load laboratories are there in your country?"), 3,
                                   min=1, step=1), br(),
                      htmlOutput("text_hot_d1"), br(),
                      htmlOutput("test"),
                      rHandsontableOutput("hot_d1"))
                ),
                fluidRow(
                  box(status = "primary", width = 12, title = "Finite population correction", solidHeader = TRUE,
                      h4(htmlOutput("fpc_text")),
                      radioButtons("fpc", label = h5("The finite population correction will reduce the required sample 
                      size and is recommended if the number of eligible case specimens is known to be small. 
                      If there are a large number of eligible case specimens, the finite population correction is not necessary."), 
                                   choices = list("Yes" = 1, "No" = 2), selected = 1))
                      # radioButtons("fpc_O", label = h4("Would you like to apply the finite population correction to the
                      # estimate of prevalence of overall ADR among all patients?"), 
                      #              choices = list("Yes" = 1, "No" = 2), selected = 1)),
                ),
                # fluidRow(
                #   box(width = 12, status = "primary", 
                #       numericInput("prop_complete", label = h4(HTML("Input the proportion of patients with all required survey 
                #       variables, prop<sub>complete</sub>. Must be a number between 0 and 1.")), 0.7, min=0, max=1, step=0.1)),
                # ),
                fluidRow(
                  box(title = "Available historical data", width = 12, 
                      status = "warning", solidHeader = TRUE,
                      
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
                      uiOutput("historical_data")
                      )
                ),
                fluidRow(
                  box(title = "Determining the number of clinics to sample", width = 12, status = "success", solidHeader = TRUE,
                      radioButtons("historical_clinic", label = h4("Do you have historical clinic-level data?"), 
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
                           uiOutput("clinic_data")),
                      conditionalPanel("input.historical_clinic == 2", 
                                       htmlOutput("mean_clinics")),
                      hr(),
                      h4(htmlOutput("min_clinics"))
                  )
                ),
                fluidRow(
                  box(title = "Number of clinics to be sampled", width = 12, status = "success", solidHeader = TRUE,
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
                  box(title = "Input the total number of DTG and non-DTG eligible case specimens for each clinic", 
                      status = "warning", width = 12, solidHeader = TRUE,
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
    values[["DF"]] <- table_d1()
    out <- rhandsontable(values[["DF"]], stretchH = "all") %>%
      hot_validate_numeric(col = 3, min=0) %>%
      hot_col(col = 3, format = "0", halign = "htCenter") %>%
      hot_col(col = 2, halign = "htCenter") %>%
      hot_col(col = 1, readOnly = TRUE, halign = "htCenter")
    return(out)
  })
  output$text_hot_d1 <- renderText({
    return(paste0("In the table below, input names for the laboratories in the <b> 'Laboratory Name' </b> 
    column by double-clicking on the cells. <br> In the <b> 'Number of Clinics' </b> column, 
    input the total number of clinics served by each laboratory. This must be a whole number."))
  })
  
  # user-inputted data in the table
  data_d1 <- reactive({
    hot_to_r(input$hot_d1)
  })
  
  # FPC text
  output$fpc_text <- renderText({
    return("Would you like to apply the finite population correction to the ADR prevalence estimates?")
  })
  
  # =========================================================
  # National-level historical data
  output$historical_data <- renderUI({
    if(input$historical_data_type == 1) {
      div(numericInput("N_vns_DTG", label = HTML("<div id='mydiv'>Input the national number of individuals, 
                                  in a recent three-month period, who were on a <b>DTG-containing</b> ART regimen for at least six months, 
                                  underwent viral load testing, and had <b>viral non-suppression</b>, N<sup>h</sup><sub>VNS,DTG</sub>. 
                                  Must be a whole number.</div>"), 
                       4500, min=0, step=1),
          numericInput("N_vns_non", label = HTML("<div id='mydiv'>Input the national number of individuals, 
                                  in a recent three-month period, who were on a <b>non-DTG-containing</b> ART regimen for at least six months, 
                                  underwent viral load testing, and had <b>viral non-suppression</b>, N<sup>h</sup><sub>VNS,nonDTG</sub>. 
                                  Must be a whole number.</div>"), 
                       2500, min=0, step=1))
    } else if(input$historical_data_type == 2) {
      div(numericInput("N_vt_DTG", label = HTML("<div id='mydiv'>Input the national number of individuals, 
                                  in a recent three-month period, who were on a <b>non-DTG-containing</b> ART regimen for at least six months and 
                                  underwent <b>viral load testing</b>, N<sup>h</sup><sub>VT,DTG</sub>. 
                                  Must be a whole number.</div>"), 
                       15000, min=0, step=1),
          numericInput("N_vt_non", label = HTML("<div id='mydiv'>Input the national number of individuals, 
                                  in a recent three-month period, who were on a <b>non-DTG-containing</b> ART regimen for at least six months and  
                                  underwent <b>viral load testing</b>, N<sup>h</sup><sub>VT,nonDTG</sub>. 
                                  Must be a whole number.</div>"), 
                       8500, min=0, step=1),
          numericInput("q_vns_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
                                 who are also virally non-suppressed, for patients on <b>DTG-containing</b> regimens, q<sub>VNS,DTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
          numericInput("q_vns_non", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
                                 who are also virally non-suppressed, for patients on <b>non-DTG-containing</b> regimens, q<sub>VNS,nonDTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1))
    } else {
      div(numericInput("N_art_DTG", label = HTML("<div id='mydiv'>Input the national number of individuals, 
                                  in a recent three-month period, who were on a <b>DTG-containing ART regimen </b> for at least six months, 
                                  N<sup>h</sup><sub>ART,DTG</sub>. Must be a whole number.</div>"), 
                       21000, min=0, step=1),
          numericInput("N_art_non", label = HTML("<div id='mydiv'>Input the national number of individuals, 
                                  in a recent three-month period, who were on a <b>non-DTG-containing ART regimen </b>for at least six months, 
                                  N<sup>h</sup><sub>ART,nonDTG</sub>. Must be a whole number.</div>"), 
                       10000, min=0, step=1),
          numericInput("q_vns_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
                                 who are also virally non-suppressed, for patients on <b>DTG-containing</b> regimens, q<sub>VNS,DTG</sub>.
                                 Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
          numericInput("q_vns_non", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
                                 who are also virally non-suppressed, for patients on <b>non-DTG-containing</b> regimens, q<sub>VNS,nonDTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
          numericInput("q_vt_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those on ART who 
                                  receive viral load tests, for patients on <b>DTG-containing</b> regimens, q<sub>VT,DTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1),
          numericInput("q_vt_non", label = HTML("<div id='mydiv'>Input the national the proportion of those on ART who 
                                  receive viral load tests, for patients on <b>non-DTG-containing</b> regimens, q<sub>VT,nonDTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1))
    }
  })
  

  # ========================= Server code for DTG-specific calculations =====================================
  
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
  
  N_DTG <- reactive({
    if (input$historical_data_type == 1) {
      return(ceiling(input$N_vns_DTG * input$prop_complete))
    } else if (input$historical_data_type == 2) {
      return(ceiling(input$N_vt_DTG * input$q_vns_DTG * input$prop_complete))
    } else {
      return(ceiling(input$N_art_DTG * input$q_vns_DTG * input$q_vt_DTG * input$prop_complete))
    }
  })
  
  N_non <- reactive({
    if (input$historical_data_type == 1) {
      return(ceiling(input$N_vns_non * input$prop_complete))
    } else if (input$historical_data_type == 2) {
      return(ceiling(input$N_vt_non * input$q_vns_non * input$prop_complete))
    } else {
      return(ceiling(input$N_art_non * input$q_vns_non * input$q_vt_non * input$prop_complete))
    }
  })
  
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
    if (N_non() < 0 | N_DTG() %% 1 != 0) {
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
  # Clinic data input
  output$clinic_data <- renderUI({
    
    if (input$clinic_data_type == 1) {
          div(numericInput("M_vns_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>DTG-containing</b> ART regimen for at least six months, 
                                  underwent viral load testing, and had <b>viral non-suppression</b>, in a recent three-month period,  
                                  M<sup>h</sup><sub>VNS,DTG</sub>. Must be a whole number.</div>"), 
                                45, min=0, step=1),
                   numericInput("M_vns_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>non-DTG-containing</b> ART regimen for at least six months, 
                                  underwent viral load testing, and had <b>viral non-suppression</b>, in a recent three-month period,
                                  M<sup>h</sup><sub>VNS,nonDTG</sub>. 
                                  Must be a whole number.</div>"), 
                                25, min=0, step=1))
    } else if (input$clinic_data_type == 2) {
        if (input$historical_data_type == 1){
          div(numericInput("M_vt_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>non-DTG-containing</b> ART regimen for at least six months and 
                                  underwent <b>viral load testing</b>, in a recent three-month period, M<sup>h</sup><sub>VT,DTG</sub>. 
                                  Must be a whole number.</div>"), 
                           150, min=0, step=1),
              numericInput("M_vt_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>non-DTG-containing</b> ART regimen for at least six months and  
                                  underwent <b>viral load testing</b>, in a recent three-month period, M<sup>h</sup><sub>VT,nonDTG</sub>. 
                                  Must be a whole number.</div>"), 
                           85, min=0, step=1),
              numericInput("q_vns_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
                                 who are also virally non-suppressed, for patients on <b>DTG-containing</b> regimens, q<sub>VNS,DTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
              numericInput("q_vns_non", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
                                 who are also virally non-suppressed, for patients on <b>non-DTG-containing</b> regimens, q<sub>VNS,nonDTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1))
        } else {
          div(numericInput("M_vt_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>non-DTG-containing</b> ART regimen for at least six months and 
                                  underwent <b>viral load testing</b>, in a recent three-month period, M<sup>h</sup><sub>VT,DTG</sub>. 
                                  Must be a whole number.</div>"), 
                           150, min=0, step=1),
              numericInput("M_vt_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>non-DTG-containing</b> ART regimen for at least six months and  
                                  underwent <b>viral load testing</b>, in a recent three-month period, M<sup>h</sup><sub>VT,nonDTG</sub>. 
                                  Must be a whole number.</div>"), 
                           85, min=0, step=1))
        }
    } else {
      if (input$historical_data_type == 1) {
        div(numericInput("M_art_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>DTG-containing ART</b> regimen for at least six months, 
                                  in a recent three-month period, M<sup>h</sup><sub>ART,DTG</sub>. Must be a whole number.</div>"), 
                         215, min=0, step=1),
            numericInput("M_art_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>non-DTG-containing ART</b> regimen for at least six months, 
                                  in a recent three-month period, M<sup>h</sup><sub>ART,nonDTG</sub>. Must be a whole number.</div>"), 
                         120, min=0, step=1),
            numericInput("q_vns_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
                                 who are also virally non-suppressed, for patients on <b>DTG-containing</b> regimens, q<sub>VNS,DTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
            numericInput("q_vns_non", label = HTML("<div id='mydiv'>Input the national proportion of those receiving viral load tests 
                                 who are also virally non-suppressed, for patients on <b>non-DTG-containing</b> regimens, q<sub>VNS,nonDTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.3, min=0, max=1, step=0.1),
            numericInput("q_vt_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those on ART who 
                                  receive viral load tests, for patients on <b>DTG-containing</b> regimens, q<sub>VT,DTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1),
            numericInput("q_vt_non", label = HTML("<div id='mydiv'>Input the national the proportion of those on ART who 
                                  receive viral load tests, for patients on <b>non-DTG-containing</b> regimens, q<sub>VT,nonDTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1))
      } else if (input$historical_data_type == 2) {
        div(numericInput("M_art_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>DTG-containing ART</b> regimen for at least six months, 
                                  in a recent three-month period, M<sup>h</sup><sub>ART,DTG</sub>. Must be a whole number.</div>"), 
                         215, min=0, step=1),
            numericInput("M_art_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>non-DTG-containing ART</b> regimen for at least six months, 
                                  in a recent three-month period, M<sup>h</sup><sub>ART,nonDTG</sub>. Must be a whole number.</div>"), 
                         120, min=0, step=1),
            numericInput("q_vt_DTG", label = HTML("<div id='mydiv'>Input the national proportion of those on ART who 
                                  receive viral load tests, for patients on <b>DTG-containing</b> regimens, q<sub>VT,DTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1),
            numericInput("q_vt_non", label = HTML("<div id='mydiv'>Input the national the proportion of those on ART who 
                                  receive viral load tests, for patients on <b>non-DTG-containing</b> regimens, q<sub>VT,nonDTG</sub>. 
                                 Must be between 0 and 1.</div>"), 0.7, min=0, max=1, step=0.1))
      } else {
        div(numericInput("M_art_DTG", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>DTG-containing ART</b> regimen for at least six months, 
                                  in a recent three-month period, M<sup>h</sup><sub>ART,DTG</sub>. Must be a whole number.</div>"), 
                         215, min=0, step=1),
            numericInput("M_art_non", label = HTML("<div id='mydiv'>Input the <b>median</b> number of individuals, per clinic, 
                                  who were on a <b>non-DTG-containing ART</b> regimen for at least six months, 
                                  in a recent three-month period, M<sup>h</sup><sub>ART,nonDTG</sub>. Must be a whole number.</div>"), 
                         120, min=0, step=1))
      }
    } 
  })
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
        ceiling(max(m_DTG() / (input$M_vns_DTG*input$prop_complete), m_nonDTG() / (input$M_vns_non*input$prop_complete)))
      } 
      else if(input$clinic_data_type == 2) {
        denom <- input$M_vt_DTG * input$q_vns_DTG * input$prop_complete
        ceiling(max(m_DTG() / denom, m_nonDTG() / denom))
      } else {
        denom <- input$M_art_DTG * input$q_vt_DTG * input$q_vns_DTG * input$prop_complete
        ceiling(max(m_DTG() / denom, m_nonDTG() / denom))
      }
    }
  })
  output$mean_clinics <- renderText({
    return(paste0("Mean anticipated number of DTG eligible case specimens per clinic: ", avg_clinics()[[1]], 
           ". <br>Mean anticipated number of non-DTG eligible case specimens per clinic: ", avg_clinics()[[2]], "."))
  })
  
  output$min_clinics <- renderText({
    return(paste0("Minimum number of clinics that should be sampled, c: ", a(num_sample_clinics(), style = "color:red")))
  })
  
  output$num_clinics <- renderUI({
    numericInput("num_clin_samp", label = h4("Input the number of clinics that will be sampled. Must be a whole number."),
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
      samp_clinics = c(sapply(1:num_labs(), function(i) {ceiling(input$num_clin_samp*data_d1()[i,3]/total)}), 
                       sum(ceiling(input$num_clin_samp*data_d1()[,3]/total))) 
    )
    colnames(df) <- c("Laboratory", "Number of Clinics", "Number of Clinics to Sample")
    return(df)
  })
  
  # Render table
  output$allocation_d1 <- renderTable({allocation_table_d1()}, digits = 0, striped = TRUE, width="100%", spacing = "l")
  
  # ====================================== Reactive Table for Phase 2 ================================================
  
  ## Handsontable for Phase 2
  table <- reactive({
    labName = unlist(sapply(1:num_labs(), function(i) {rep(data_d1()[i, 2], allocation_table_d1()[i, 3])}))
    clinic = unlist(sapply(1:num_labs(), function(i) { as.character(1:allocation_table_d1()[i,3])}))
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
  })
  
  data <- reactive({
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

