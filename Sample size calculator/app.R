#=============================================#
# Shiny App for HIVDR Sample Size Calculations
# 
# Results displayed at 
# https://worldhealthorg.shinyapps.io/HIVDR/ 
# https://smwu.shinyapps.io/shinyapp/
#=============================================#

# Load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyMatrix)
library(rhandsontable)

##########################
# Define UI code
##########################

ui <- dashboardPage(
  
  dashboardHeader(
    title = h2("Sample size calculations for laboratory-based acquired HIV drug resistance survey"),
    titleWidth = 1100,
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 75px}"),
            tags$style(".main-header .logo {height: 75px}")
    )        
  ),
  
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 75px; font-size: 28px; }"), width = 325,
    tags$style(HTML(".main-sidebar .sidebar .sidebar-menu a{font-size: 20px; }")),
    sidebarMenu(
      menuItem("Inputs", tabName = "inputs"),
      menuItem("Outputs", tabName = "outputs", startExpanded = TRUE,
               menuSubItem("Sample Sizes", tabName = "sizes"),
               menuSubItem("Allocation Across Laboratories", tabName = "allocation"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "inputs",
              useShinyjs(),
              fluidPage(
                fluidRow(
                  box(title = "Input the total number of DTG and non-DTG eligible case specimens for each laboratory.", width = 12, 
                      status = "warning", solidHeader = TRUE,
                      numericInput("num_labs", label = h4("How many viral load laboratories are there in your country?"), 3,
                                   min=1, step=1)),
                ),
                # fluidRow(
                #   box(width=6, uiOutput("N_DTG"), status="warning"),
                #   box(width=6, uiOutput("N_nonDTG"), status="warning")
                # ),
                fluidRow(
                  # matrixInput("inputTable",
                  #             value = uiOutput("matrix"),
                  #             inputClass = "",
                  #             rows = list(),
                  #             cols = list(names = TRUE, extend = TRUE),
                  #             class = "character",
                  #             paste = FALSE,
                  #             copy = FALSE)
                  box(status = "warning", width = 12,
                      htmlOutput("text_table"), br(),
                      htmlOutput("additional_text"), br(),
                      rHandsontableOutput("hot"))
                ),
                div(style = "text-align:center", actionButton("calculate", "Submit"))
              )
      ),
      
      tabItem(tabName = "sizes",
              fluidRow(
                box(title = "Sample Size for DTG Case Specimens", status = "primary",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    # Display table of assumptions and sample size required for DTG
                    h5(htmlOutput("text_DTG")), br(),
                    tableOutput("values_DTG"),
                    h4(htmlOutput("required_DTG")),
                    h4(htmlOutput("target_DTG")),
                ),
                
                box(title = "Overall Estimated Sample Size", status = "primary", 
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
                box(title = "Sample Size for Non-DTG Case Specimens", status = "primary",
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
      
      tabItem(tabName = "allocation",
              fluidPage(
                fluidRow(
                  box(title = "Allocation of sample sizes across laboratories", width = 12, status = "warning", 
                      solidHeader = TRUE, div(tableOutput("allocation"), style = "font-size: 120%")))
                )
              )
    )
    
    
  )
  # tags$head(tags$style(HTML(".skin-blue .main-sidebar {background-color: #2FA584;}")))
  
)


#################################
# Define Server code
#################################

server <- function(input, output) {
  
  # n_labs <- reactive({as.integer(input$num_labs)})
  # DF <- reactive({
  # })
  # 
  # DF <- reactive({data.frame(Laboratory = 1:3, DTG = rep(300,n_labs()))})
  
  values <- reactiveValues()
  
  ## Handsontable
  table <- reactive({
    # if (!is.null(input$hot)) {
    #   DF = hot_to_r(input$hot)
    # } else {
    #   if (is.null(values[["DF"]])) {
    #     DF <- DF
    #   } else {
    #     DF <- values[["DF"]]
    #   }
    # }
    num_labs <- as.integer(input$num_labs)
    lab <- 1:num_labs
    default_DTG <- c(3000, 9000, 800)
    default_nonDTG <- c(3000, 1000, 3200)
    labName <- paste("Lab", as.character(lab))
    DTG <- rep(default_DTG, times = ceiling(num_labs / 3), len = num_labs)
    non <- rep(default_nonDTG, times = ceiling(num_labs / 3), len = num_labs)
    df <- data.frame(lab, labName, DTG, non, stringsAsFactors = FALSE)
    colnames(df) <- c("Laboratory", "Laboratory Name", 
                      "DTG Eligible Case Specimens", "Non-DTG Eligible Case Specimens")
    df
  })
  
  output$text_table <- renderText({
    paste0("In the table below, input names for the laboratories in the <b> 'Laboratory Name' </b> 
    column by double-clicking on the cells. <br> <br> In the <b> 'DTG Eligible Case Specimens' </b> column, 
    input the total number of eligible case specimens<sup>&#8224</sup> from 
    patients on a <b>DTG-containing regimen</b> from each laboratory during the study period. 
    This must be a whole number. <br> <br> In the <b> 'Non-DTG Eligible Case Specimens'</b> column, 
    input the total number of eligible case specimens from patients on a 
    <b>non-DTG-containing regimen </b> from each laboratory during the study period. 
    This must be a whole number.")
  })
  
  output$additional_text <- renderText({
    "<sup>&#8224</sup> A case specimen is defined as a remnant specimen with viral load &#8805 1000 copies/mL obtained during 
    the survey period from an individual with all four required survey variables."
  })
  
  output$hot <- renderRHandsontable({
    values[["DF"]] <- table()
    if(!is.null(values[["DF"]])) {
      rhandsontable(values[["DF"]], stretchH = "all") %>%
        hot_validate_numeric(col = 3, min=0) %>%
        hot_validate_numeric(col = 4, min=0) %>%
        hot_col(col = 3, format = "0", halign = "htCenter") %>%
        hot_col(col = 4, format = "0", halign = "htCenter") %>%
        hot_col(col = 2, halign = "htCenter") %>%
        hot_col(col = 1, readOnly = TRUE, halign = "htCenter")
    }
  })
  
  data <- reactive({
     hot_to_r(input$hot)
  })
  
  # =========================================
  # Server code for DTG-specific calculations
  
  # output$N_DTG <- renderUI({
  #   default_N_DTG <- c(3000, 9000, 800)
  #   # num_labs <- reactive({as.integer(input$num_labs)}) # default 3
  #   lapply(1:as.integer(input$num_labs), function(i) {
  #     numericInput(inputId = paste0("N_DTG", i), 
  #                  label = div(h5(paste("Input the total number of eligible case specimens from patients on a"), 
  #                                 style = "display: inline;"), "DTG-containing", 
  #                              h5(paste("regimen from"), style = "display: inline;"), paste("Laboratory", i), 
  #                              h5(paste("during the study period. Must be a whole number."), style = "display: inline;")),
  #                  ifelse(as.integer(input$num_labs)==3, default_N_DTG[i], 3000), min=1, step=1)
  #   })
  # })
  # 
  # output$N_nonDTG <- renderUI({
  #   default_N_nonDTG <- c(3000, 1000, 3200)
  #   # num_labs <- reactive({as.integer(input$num_labs)}) # default 3
  #   lapply(1:as.integer(input$num_labs), function(i) {
  #     numericInput(inputId = paste0("N_non", i), 
  #                  label = div(h5(paste("Input the total number of eligible case specimens from patients on a"), 
  #                                 style = "display: inline;"), "non-DTG-containing", 
  #                              h5(paste("regimen from"), style = "display: inline;"), paste("Laboratory", i), 
  #                              h5(paste("during the study period. Must be a whole number."), style = "display: inline;")),
  #                  ifelse(as.integer(input$num_labs)==3, default_N_nonDTG[i], 3000), min=1, step=1)
  #   })
  # })
  
  N_DTG_vect <- reactive({
    sapply(1:as.integer(input$num_labs), function(i) {
      data()[i, 3]
      # as.integer(input[[paste0("N_DTG", i)]])[1]
    })
  })
  
  N_non_vect <- reactive({
    sapply(1:as.integer(input$num_labs), function(i) {
      data()[i, 4]
      # as.integer(input[[paste0("N_non", i)]])[1]
    })
  })
  
  # # Toggle for finite/infinite eligible population
  # output$N_DTG <- renderUI({
  #   switch(input$inf_DTG,
  #          infinite = div(style = "text-align:center; width:87%;", br(), 
  #                         h5("No finite population correction will be used.")),
  #          finite = numericInput("N_DTG", h5("Input the total number of eligible case specimens from patients on a DTG-containing regimen, 
  #               nationally, during the study period. Must be a whole number."),
  #                                  20000, min=1, step=1))
  # })
  # 
  ### Define variables and functions
  prev_DTG <- 0.035
  CI_DTG <- prec_DTG <- 0.02
  alpha_DTG <- 0.05
  labFail_DTG <- 0.3
  N_DTG <- reactive({sum(N_DTG_vect())})
  N_non <- reactive({sum(N_non_vect())})
  # N_DTG <- reactive({ifelse(input$inf_DTG == "finite", input$N_DTG, Inf)})
  
  
  # Function to calculate sample sizes using FPC and Wald-type intervals
  calc_sample_size <- function(alpha, prev, N, CI, labFail) {
    if (N == Inf) {  # infinite population
      nEff <- qnorm(1-alpha/2)^2*prev*(1-prev) / (CI^2)
      n <- ceiling(nEff)
      m <- ceiling(n/(1-labFail))
      return(list(n, m))
    } else if (N < 0 | N %% 1 != 0) {  # check positive integer
      return(list("NA. Number of eligible case specimens must be a whole number.", "NA. Number of eligible case specimens must be a whole number."))
    } else {
      nEff <- qnorm(1-alpha/2)^2*prev*(1-prev)*N /
        (N*CI^2 + (prev*(1-prev)*qnorm(1-alpha/2)^2))
      n <- min(N, ceiling(nEff))
      m <- min(N, ceiling(n/(1-labFail)))
      return(list(n, m))
    }
  }
  
  # Calculate sample size
  sampleSize_DTG <- eventReactive(input$calculate, {calc_sample_size(alpha_DTG, prev_DTG, N_DTG(),
                                                                  CI_DTG, labFail_DTG)})

  # Table of user-specified parameter values
  assumptions_DTG <- eventReactive(input$calculate, {
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

  # output$text_DTG <- renderText({
  #   "Sample sizes necessary for estimating the prevalence of ADR<sub>DTG</sub>
  #   among patients taking DTG-containing regimens."
  # })
  output$text_DTG <- renderText({
    "Sample sizes necessary for estimating the prevalence of DTG-specific resistance
    among patients taking DTG-containing regimens."
  })


  # ====================================
  # Server code for overall calculations

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
  N_O <- reactive({N_DTG() + N_non()})
  # N_O <- reactive({ifelse(input$inf_O == "finite", input$N_O, Inf)})

  # Calculate sample size
  sampleSize_O <- eventReactive(input$calculate,{calc_sample_size(alpha_O, prev_O, N_O(),
                                                               CI_O, labFail_O)})

  # Table of user-specified parameter values
  assumptions_O <- eventReactive(input$calculate, {
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
  
  prop_nonDTG <- reactive({min(max(0, N_non() / N_O()), 1)})

  ## Calculating n_nonDTG, m_nonDTG, n_total, m_total
  n_nonDTG <- eventReactive(input$calculate, {
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

  m_nonDTG <- eventReactive(input$calculate, {
    # if (prop_nonDTG() < 0 | prop_nonDTG() > 1) {
    #   return("NA. Proportion non-DTG must be between 0 and 1.")
    # }
    if (N_DTG() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    if (N_non() < 0 | N_non() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    return(ceiling(n_nonDTG() / (1 - labFail_O)))
  })

  n_total <- eventReactive(input$calculate, {
    # if (prop_nonDTG() < 0 | prop_nonDTG() > 1) {
    #   return("NA. Proportion non-DTG must be between 0 and 1.")
    # }
    if (N_DTG() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    if (N_non() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    return(sampleSize_DTG()[[1]] + ceiling(sampleSize_O()[[1]] * prop_nonDTG()))
  })

  m_total <- eventReactive(input$calculate, {
    # if (prop_nonDTG() < 0 | prop_nonDTG() > 1) {
    #   return("NA. Proportion non-DTG must be between 0 and 1.")
    # }
    if (N_DTG() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    if (N_DTG() < 0 | N_DTG() %% 1 != 0) {
      return("NA. Number of eligible case specimens must be a whole number.")
    }
    return(sampleSize_DTG()[[2]] + m_nonDTG())
  })

  # Text output for non-DTG and total
  output$required_non <- renderText({
    paste0("Required sample size, n<sub>nonDTG</sub>: ",
           a(n_nonDTG(), style = "color:red"))
  })

  output$target_non <- renderText({
    paste0("Target sample size (adjusted for genotyping failure), m<sub>nonDTG</sub>: ",
           a(m_nonDTG(), style = "color:red"))
  })

  output$text_non <- renderText({
    "Sample sizes necessary from patients taking non-DTG-containing regimens to ensure sufficient sample 
    size for overall estimate of ADR."
  })
  
  # output$text_non <- renderText({
  #   "Sample sizes necessary among patients taking non-DTG-containing regimens to ensure the sample 
  #   sizes for the overall estimate are met."
  # })
  
  output$prop_non <- renderText({
    paste0("Proportion of eligible case specimens from patients on non-DTG-containing regimens, 
           prop<sub>nonDTG</sub>: ",
           a(round(prop_nonDTG(), 2), style = "color:blue"))
  })

  ## Calculating total sample sizes required
  output$required_total <- renderText({
    paste0("Total required sample size, n<sub>DTG</sub> + n<sub>nonDTG</sub>: ",
           a(n_total(), style = "color:red"))
  })

  output$target_total <- renderText({
    paste0("Total target sample size (adjusted for genotyping failure), m<sub>DTG</sub> + m<sub>nonDTG</sub>: ",
           a(m_total(),
             style = "color:red"))
  })

  output$text_total <- renderText({
    "Total sample sizes necessary for both the DTG and overall estimates."
  })
  
  ####################### Allocation Tab #######################################
  # Table of user-specified parameter values
  allocation_table <- eventReactive(input$calculate, {
    num_labs <- reactive({as.integer(input$num_labs)}) # default 3
    df <- data.frame(
      labs = c(data()[,2], "Total"),
      DTG  = c(sapply(1:num_labs(), function(i) {N_DTG_vect()[i]}), N_DTG()),
      non = c(sapply(1:num_labs(), function(i) {N_non_vect()[i]}), N_non()),
      target_DTG = c(sapply(1:num_labs(), function(i) {ceiling(sampleSize_DTG()[[2]]*N_DTG_vect()[i]/N_DTG())}), 
                     sum(ceiling(sampleSize_DTG()[[2]]*N_DTG_vect()/N_DTG()))),
      target_non = c(sapply(1:num_labs(), function(i) {ceiling(m_nonDTG()*N_non_vect()[i]/N_non())}), 
                     sum(ceiling(m_nonDTG()*N_non_vect()/N_non()))),
      total = c(sapply(1:num_labs(), function(i) {
          ceiling(sampleSize_DTG()[[2]]*N_DTG_vect()[i]/N_DTG()) + ceiling(m_nonDTG()*N_non_vect()[i]/N_non())
        }), sum(ceiling(sampleSize_DTG()[[2]]*N_DTG_vect()/N_DTG()) + ceiling(m_nonDTG()*N_non_vect()/N_non())))
    )
    colnames(df) <- c("Laboratory", "DTG Eligible Case Specimens", "Non-DTG Eligible Case Specimens", "Target DTG Sample Size", 
                      "Target Non-DTG Sample Size", "Total Recommended Sample Size")
    df
  })
  
  # allocation_table <- eventReactive(input$calculate, {
  #   num_labs <- reactive({as.integer(input$num_labs)}) # default 3
  #   df <- data.frame(
  #     Variable = c("DTG eligible case specimens", "Non-DTG eligible case specimens", "Target DTG sample size", 
  #                  "Target non-DTG sample size", "Total recommended sample size"), stringsAsFactors = FALSE
  #   )
  #   for(i in 1:num_labs()) {
  #     # name <- paste0("Laboratory ", i)
  #     df <- cbind(df, c(N_DTG_vect()[i], 
  #                       N_non_vect()[i], 
  #                       ceiling(sampleSize_DTG()[[2]]*N_DTG_vect()[i]/N_DTG()),
  #                       ceiling(m_nonDTG()*N_non_vect()[i]/N_non()),
  #                       ceiling(sampleSize_DTG()[[2]]*N_DTG_vect()[i]/N_DTG()) + ceiling(m_nonDTG()*N_non_vect()[i]/N_non())))
  #   }
  #   df$Total <- sapply(1:5, function(i){sum(as.numeric(df[i,-1]))})
  #   colnames(df) <- c("", sapply(1:num_labs(), function(i){paste("Laboratory", i)}), "Total")
  #   df
  # })

  
  # Render table
  output$allocation <- renderTable({allocation_table()}, digits = 0, striped = TRUE, width="100%", spacing = "l")
}


############################
# Build Shiny app
############################

shinyApp(ui, server)

