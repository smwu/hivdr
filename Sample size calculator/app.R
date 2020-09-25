#=============================================#
# Shiny App for HIVDR Sample Size Calculations
# 
# Results displayed at 
# https://smwu.shinyapps.io/HIVDR/
#=============================================#

# Load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)

##########################
# Define UI code
##########################

ui <- dashboardPage(
  
  dashboardHeader(
    title = h2("Sample Size Calculations for Acquired HIV Drug Resistance"),
    titleWidth = 800,
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 75px}"),
            tags$style(".main-header .logo {height: 75px}")
    )        
  ),
  
  dashboardSidebar(
    useShinyjs(),
    tags$style(".left-side, .main-sidebar {padding-top: 75px}"),
    
    # Display population size input options
    div(style = "text-align:center", h3("Input eligible population sizes")),
    hr(),
    
    # DTG estimate finite population choices
    radioButtons("inf_DTG", label = h4("DTG Estimate"),
                 choices = list("Finite population" = "finite", "Infinite population" = "infinite"), 
                 selected = "infinite"),
    uiOutput("N_DTG"),
    hr(),
    
    # Overall estimate finite population choices
    radioButtons("inf_O", label = h4("Overall Estimate"),
                 choices = list("Finite population" = "finite", "Infinite population" = "infinite"), 
                 selected = "infinite"),
    uiOutput("N_O"),
    hr(),
    
    # Add prop_nonDTG input if infinite population is used
    shinyjs::hidden(
      div(id = "title_prop_nonDTG", style = "text-align:center; width:56%;", h4("Proportion Non-DTG")),
      numericInput("prop_nonDTG", label=h5("Input the proportion of all eligible case specimens belonging to 
                                         patients on non-DTG-containing regimens. Must be a value between 0 and 1."), 
                   0.8, min=0, max=1, step=0.1)
    ),
    
    actionButton("submit", "Submit"),
    width = 325
  ),
  
  dashboardBody(
    fluidRow(
      box(title = "DTG Estimate Sample Size", status = "primary",
          collapsible = TRUE, width = 6, solidHeader = TRUE,
          # Display table of assumptions and sample size required for DTG
          h5(htmlOutput("text_DTG")), br(),
          tableOutput("values_DTG"),
          h4(htmlOutput("required_DTG")),
          h4(htmlOutput("target_DTG"))
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
          h5(htmlOutput("text_non")), br(),
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
  tags$head(tags$style(HTML(".skin-blue .main-sidebar {background-color: #2FA584;}")))
  
)


#################################
# Define Server code
#################################

server <- function(input, output) {
  
  # =========================================
  # Server code for DTG-specific calculations
  
  # Toggle for finite/infinite eligible population
  output$N_DTG <- renderUI({
    switch(input$inf_DTG,
           infinite = div(style = "text-align:center; width:87%;", br(), 
                          h5("No finite population correction will be used.")),
           finite = numericInput("N_DTG", h5("Input the total number of eligible case specimens from patients on a DTG-containing regimen, 
                nationally, during the study period. Must be a whole number."),
                                 20000, min=1, step=1))
  })
  
  ### Define variables and functions
  prev_DTG <- 0.035
  CI_DTG <- prec_DTG <- 0.02
  alpha_DTG <- 0.05
  labFail_DTG <- 0.3
  N_DTG <- reactive({ifelse(input$inf_DTG == "finite", input$N_DTG, Inf)})
  
  # Function to calculate sample sizes using FPC and Wald-type intervals
  calc_sample_size <- function(alpha, prev, N, CI, labFail) {
    if (N == Inf) {  # infinite population
      nEff <- qnorm(1-alpha/2)^2*prev*(1-prev) / (CI^2)
      n <- ceiling(nEff)
      m <- ceiling(n/(1-labFail))
      return(list(n, m))
    } else if (N < 0 | N %% 1 != 0) {  # check positive integer
      return(list("NA. Population sizes must be positive integers.", "NA. Population sizes must be positive integers."))
    } else {
      nEff <- qnorm(1-alpha/2)^2*prev*(1-prev)*N / 
        (N*CI^2 + (prev*(1-prev)*qnorm(1-alpha/2)^2))
      n <- min(N, ceiling(nEff))
      m <- min(N, ceiling(n/(1-labFail)))
      return(list(n, m))
    }
  }
  
  # Calculate sample size
  sampleSize_DTG <- eventReactive(input$submit, {calc_sample_size(alpha_DTG, prev_DTG, N_DTG(), 
                                                                  CI_DTG, labFail_DTG)})
  
  # Table of user-specified parameter values
  assumptions_DTG <- eventReactive(input$submit, {
    data.frame(
      Assumptions = c("Expected prevalence of DTG-specific resistance", 
                      "Desired absolute precision (95% CI half-width)",
                      "Population Size",
                      "Significance Level",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_DTG*100, "%"),
                             paste0("\u00B1", prec_DTG*100, "%"),
                             N_DTG(),
                             alpha_DTG,
                             paste0(labFail_DTG*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render table
  output$values_DTG <- renderTable({assumptions_DTG()})
  
  # Sample size output
  output$required_DTG <- renderText({
    paste0("Required sample size, n<sub>DTG</sub>: ", 
           a(sampleSize_DTG()[[1]], style = "color:red"))
    
  })
  
  output$target_DTG <- renderText({
    paste0("Target sample size (adjusted for genotyping failure), m<sub>DTG</sub>: ", 
           a(sampleSize_DTG()[[2]], style = "color:red"))
  })
  
  output$text_DTG <- renderText({
    "Sample sizes necessary for estimating the prevalence of ADR<sub>DTG</sub> 
    among patients taking DTG-containing regimens."
  })
  
  
  # ====================================
  # Server code for overall calculations
  
  # Toggle for finite/infinite eligible population
  output$N_O <- renderUI({
    switch(input$inf_O,
           infinite = div(style = "text-align:center; width:87%;", br(), 
                          h5("No finite population correction will be used.")),
           finite = numericInput("N_O", h5("Input the total number of eligible case specimens from patients on all regimens, 
                         nationally, during the study period. Must be a whole number."), 
                                 100000, min=1, step=1))
  })
  
  ### Define variables
  prev_O <- 0.5
  CI_O <- prec_O <- 0.06
  alpha_O <- 0.05
  labFail_O <- 0.3
  N_O <- reactive({ifelse(input$inf_O == "finite", input$N_O, Inf)})
  
  # Calculate sample size
  sampleSize_O <- eventReactive(input$submit,{calc_sample_size(alpha_O, prev_O, N_O(), 
                                                               CI_O, labFail_O)})
  
  # Table of user-specified parameter values
  assumptions_O <- eventReactive(input$submit, {
    data.frame(
      Assumptions = c("Expected prevalence of overall drug resistance", 
                      "Desired absolute precision (95% CI half-width)",
                      "Population Size",
                      "Significance Level",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_O*100, "%"),
                             paste0("\u00B1", prec_O*100, "%"),
                             N_O(),
                             alpha_O,
                             paste0(labFail_O*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render table
  output$values_O <- renderTable({assumptions_O()})
  
  # Sample size output
  output$required_O <- renderText({
    paste0("Required sample size, n<sub>overall</sub>: ", 
           a(sampleSize_O()[[1]], style = "color:red"))
  })
  
  output$target_O <- renderText({
    paste0("Target sample size (adjusted for genotyping failure), m<sub>overall</sub>: ", 
           a(sampleSize_O()[[2]], style = "color:red"))
  })
  
  output$text_O <- renderText({
    "Sample sizes necessary for estimating the prevalence of overall ADR among all patients."
  })
  
  ##########################################
  # Server code for non-DTG and total calculations
  
  # Toggle for prop_nonDTG user input
  infinite_toggle <- reactive({ list(input$inf_O, input$inf_DTG)})
  
  observeEvent(infinite_toggle(), {
    if(input$inf_O=="infinite" | input$inf_DTG=="infinite") {
      shinyjs::show(id = "title_prop_nonDTG")
      shinyjs::show("prop_nonDTG")
    } else {
      shinyjs::hide(id = "title_prop_nonDTG")
      shinyjs::hide("prop_nonDTG")
    }
  })
  
  
  # Set prop_nonDTG variable to be user-specified if infinite populations are used
  prop_nonDTG <- reactive({
    if (input$inf_DTG == "finite" & input$inf_O == "finite") {
      1 - min(max(0, input$N_DTG / input$N_O), 1)
    } else {
      input$prop_nonDTG
    }
  })
  
  ## Calculating n_nonDTG, m_nonDTG, n_total, m_total
  n_nonDTG <- eventReactive(input$submit, {
    if (prop_nonDTG() < 0 | prop_nonDTG() > 1) {
      return("NA. Proportion non-DTG must be between 0 and 1.")
    } 
    if (N_DTG() < Inf & (N_DTG() < 0 | N_DTG() %% 1 != 0)) {
      return("NA. Population sizes must be positive integers.")
    }
    if (N_DTG() < Inf & (N_DTG() < 0 | N_DTG() %% 1 != 0)) {
      return("NA. Population sizes must be positive integers.")
    } 
    return(ceiling(sampleSize_O()[[1]] * prop_nonDTG()))
  })
  
  m_nonDTG <- eventReactive(input$submit, {
    if (prop_nonDTG() < 0 | prop_nonDTG() > 1) {
      return("NA. Proportion non-DTG must be between 0 and 1.")
    } 
    if (N_DTG() < Inf & (N_DTG() < 0 | N_DTG() %% 1 != 0)) {
      return("NA. Population sizes must be positive integers.")
    }
    if (N_DTG() < Inf & (N_DTG() < 0 | N_DTG() %% 1 != 0)) {
      return("NA. Population sizes must be positive integers.")
    } 
    return(ceiling(n_nonDTG() / (1 - labFail_O)))
  })
  
  n_total <- eventReactive(input$submit, {
    if (prop_nonDTG() < 0 | prop_nonDTG() > 1) {
      return("NA. Proportion non-DTG must be between 0 and 1.")
    } 
    if (N_DTG() < Inf & (N_DTG() < 0 | N_DTG() %% 1 != 0)) {
      return("NA. Population sizes must be positive integers.")
    }
    if (N_DTG() < Inf & (N_DTG() < 0 | N_DTG() %% 1 != 0)) {
      return("NA. Population sizes must be positive integers.")
    } 
    return(sampleSize_DTG()[[1]] + ceiling(sampleSize_O()[[1]] * prop_nonDTG()))
  })
  
  m_total <- eventReactive(input$submit, {
    if (prop_nonDTG() < 0 | prop_nonDTG() > 1) {
      return("NA. Proportion non-DTG must be between 0 and 1.")
    } 
    if (N_DTG() < Inf & (N_DTG() < 0 | N_DTG() %% 1 != 0)) {
      return("NA. Population sizes must be positive integers.")
    }
    if (N_DTG() < Inf & (N_DTG() < 0 | N_DTG() %% 1 != 0)) {
      return("NA. Population sizes must be positive integers.")
    } 
    return(sampleSize_DTG()[[2]] + m_nonDTG())
  })
  
  # Text output for non-DTG and total
  output$required_non <- renderText({
    paste0("Required sample size, n<sub>nonDTG</sub>: ", 
           a(n_nonDTG(), style = "color:red"))
  })
  
  output$target_non <- renderText({
    paste0("Target sample size (adjusted for design effect and genotyping failure), m<sub>nonDTG</sub>: ", 
           a(m_nonDTG(), style = "color:red"))
  })
  
  output$text_non <- renderText({
    "Sample sizes necessary among patients taking non-DTG-containing regimens to ensure the overall estimate 
    sample sizes are met."
  })
  
  ## Calculating total sample sizes required
  output$required_total <- renderText({
    paste0("Total required sample size, n<sub>DTG</sub> + n<sub>nonDTG</sub>: ", 
           a(n_total(), style = "color:red"))
  })
  
  output$target_total <- renderText({
    paste0("Total target sample size (adjusted for design effect and genotyping failure), m<sub>DTG</sub> + m<sub>nonDTG</sub>: ", 
           a(m_total(), 
             style = "color:red"))
  })
  
  output$text_total <- renderText({
    "Total sample sizes necessary for both the DTG and overall estimates."
  })
}


############################
# Build Shiny app
############################

shinyApp(ui, server)

