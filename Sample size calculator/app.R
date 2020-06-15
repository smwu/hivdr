#=============================================#
# Shiny App for HIVDR Sample Size Calculations
# 
# Results displayed at 
# https://smwu.shinyapps.io/HIVDR/
#=============================================#

# Load required packages
library(shiny)
library(shinyjs)

##########################
# Define UI code
##########################

ui <- fluidPage(
        shinyjs::useShinyjs(),
  
        titlePanel(title = "Sample Size Calculations for HIV Drug Resistance"),
                      
        sidebarLayout(
          
          # Display population size input options          
          sidebarPanel(h3("Input Population Sizes"),
                       br(),
                       br(),
                       br(),
                       numericInput("N_DTG", "Input the underlying population size for DTG-specific patients. Must be a whole number.", 20000, min=1, step=1),
                       br(),
                       br(),
                       br(),
                       numericInput("N_O", "Input the underlying population size for patients on all regimens. Must be a whole number.", 10700, min=1, step=1),
                       br(),
                       br(),
                       br(),
                       actionButton("submit", "Submit"),
                       width=4
          ),
          
          mainPanel(
            
            # Display table of assumptions and sample size required for DTG
            tableOutput("values_DTG"),
            h4(textOutput("sampleSize_DTG")),
            br(),
            
            # Display table of assumptions and sample size required for overall
            tableOutput("values_O"),
            h4(textOutput("sampleSize_O")), width=8
          )
        )
)


#################################
# Define Server code
#################################

server <- function(input, output) {
  
  # =========================================
  # Server code for DTG-specific calculations

  ### Define variables and functions
  prev_DTG <- 0.035
  CI_DTG <- prec_DTG <- 0.02
  alpha_DTG <- 0.05
  labFail_DTG <- 0.2
  
  # Function to calculate sample sizes using FPC and Wald-type intervals
  # No inflation factor
  calcSampleSize <- function(alpha, prev, N, CI, labFail) {
    if (N < 0 | N %% 1 != 0) {
      return("NA. Population sizes must be positive integers.")
    } else {
      nEff <- qnorm(1-alpha/2)^2*prev*(1-prev)*N / 
        (N*CI^2 + (prev*(1-prev)*qnorm(1-alpha/2)^2))
      n <- nEff/(1-labFail)
      return(min(N, ceiling(n)))
    }
  }
  
  # Calculate sample size
  sampleSize_DTG <- eventReactive(input$submit, {calcSampleSize(alpha_DTG, prev_DTG, input$N_DTG, 
                                         CI_DTG, labFail_DTG)})
  
  # Table of user-specified parameter values
  assumptions_DTG <- eventReactive(input$submit, {
    data.frame(
      Assumptions = c("Expected prevalence of drug specific resistance", 
                      "Desired absolute precision (95% CI half-width)",
                      "Population Size",
                      "Significance Level",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_DTG*100, "%"),
                             paste0("\u00B1", prec_DTG*100, "%"),
                             input$N_DTG,
                             alpha_DTG,
                             paste0(labFail_DTG*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render table
  output$values_DTG <- renderTable({assumptions_DTG()})
  
  # Sample size output
  output$sampleSize_DTG <- renderText({
    paste0("Sample size for DTG-specific estimate: ", sampleSize_DTG())
  })

  
  # ====================================
  # Server code for overall calculations
  
  ### Define variables
  prev_O <- 0.5
  CI_O <- prec_O <- 0.06
  alpha_O <- 0.05
  labFail_O <- 0.2
  
  # Calculate sample size
  sampleSize_O <- eventReactive(input$submit,{calcSampleSize(alpha_O, prev_O, input$N_O, 
                                             CI_O, labFail_O)})
  
  # Table of user-specified parameter values
  assumptions_O <- eventReactive(input$submit, {
    data.frame(
      Assumptions = c("Expected prevalence of drug specific resistance", 
                      "Desired absolute precision (95% CI half-width)",
                      "Population Size",
                      "Significance Level",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_O*100, "%"),
                             paste0("\u00B1", prec_O*100, "%"),
                             input$N_O,
                             alpha_O,
                             paste0(labFail_O*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render table
  output$values_O <- renderTable({assumptions_O()})
  
  # Sample size output
  output$sampleSize_O <- renderText({
    paste0("Sample size for overall estimate: ", sampleSize_O())
  })
  
}


############################
# Build Shiny app
############################

shinyApp(ui, server)

