#=============================================#
# Shiny App for HIVDR Sample Size Calculations
# 
# Results displayed at 
# https://smwu.shinyapps.io/shinyapp/
#=============================================#

# Load required packages
library(shiny)
library(ggplot2)

##########################
# Define UI code
##########################

ui <- fluidPage(
  
  # Set up sidebar page
  pageWithSidebar(
      headerPanel("Sample size calculations for clinic-based acquired HIV drug resistance survey"),
        
      # Sidebar options to specify sample size calculation
      sidebarPanel(h4("Input Values"),
                   sliderInput("prev_VS_DTG", "Expected prevalence of VS for patients on DTG-containing regimens (%)", 
                               min=0, max=100, value=95, step=5),
                   sliderInput("prev_VS_O", "Expected prevalence of VS for patients overall (%)", 
                               min=0, max=100, value=90, step=5),
                   
                   sliderInput("prec_VS_DTG", "Absolute precision (95% CI half-width) of VS outcome for patients on DTG-containing regimens (%)", 
                               min=0, max=30, value=3, step=0.5),
                   sliderInput("prec_VS_O", "Absolute precision (95% CI half-width) of VS outcome for patients overall (%)", 
                               min=0, max=30, value=5, step=0.5),
                   hr(),
                   
                   numericInput("n", "Number of clinics to sample", min=17, value=20, step=1),
                   numericInput("N", "Total number of clinics", min=17, value=100000, step=1),
                   numericInput("M", "Total number of individuals on ART", min=0, value=100000000, step=1),
                   
                   hr(),
                   
                   # sliderInput("q_VNS_DTG", "Percentage of individuals on DTG-containing ART with viral non-suppression (%)", 
                   #             min=0, max=100, value=95, step=5),
                   # sliderInput("q_VNS_O", "Percentage of individuals on ART with viral non-suppression (%)", 
                   #             min=0, max=100, value=90, step=5),
                   
                   sliderInput("q_DTG", "National percentage of individuals on ART who are on DTG-containing regimens (%)", 
                               min=0, max=100, value=60, step=1),
                   sliderInput("prev_ADR_DTG", "Expected prevalence of ADR for patients on DTG-containing regimens and with viral non-suppression (%)", 
                               min=0, max=100, value=3.5, step=0.5),
                   sliderInput("prev_ADR_O", "Expected prevalence of ADR for all patients with viral non-suppression (%)", 
                               min=0, max=100, value=50, step=0.5)
      ),

      # Main panel display consisting of two tables and two plots
      mainPanel(
        fluidRow(
          column(6,
            wellPanel(
              h3("Sample Size for DTG Case Specimens"),
              # Display table of assumptions and sample size required for DTG
              h5(htmlOutput("text_DTG")), br(),
              tableOutput("values_DTG"),
              h4(htmlOutput("clinic_DTG")),
              h4(htmlOutput("sample_size_DTG"))
            )  
          ),
          column(6,
            wellPanel(
              h3("Overall Estimated Sample Size"),
              # Display table of assumptions and sample size required for overall
              h5(htmlOutput("text_O")), br(),
              tableOutput("values_O"),
              h4(htmlOutput("clinic_O")),
              h4(htmlOutput("sample_size_O"))
            )  
          )   
        ),

        fluidRow(
          column(6,
            wellPanel(
              h3("Sample Size for Non-DTG Case Specimens"),
              # Display sample size required for non-DTG
              h5(htmlOutput("text_non")),
              h4(htmlOutput("prop_non")),
              h4(htmlOutput("clinic_non")),
              h4(htmlOutput("sample_size_non"))
            )
          ),
          column(6,
            wellPanel(
              h3("Total Sample Size"),
              # Display total sample size required
              h5(htmlOutput("text_total")), br(),
              h4(htmlOutput("clinic_total")),
              h4(htmlOutput("sample_size_total"))
            )
          )
        ),
        
        fluidRow(
          column(6,
            wellPanel(
              h3("Precision for DTG ADR Estimates"),
              # Display precision for ADR estimates for DTG and overall
              tableOutput("values_ADR_DTG"),
              h4(htmlOutput("prec_ADR_DTG"))
            )
          )  ,
          column(6,
            wellPanel(
              h3("Precision for Overall ADR Estimates"),
              tableOutput("values_ADR_O"),
              h4(htmlOutput("prec_ADR_O"))
            )
          )  
        )
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
  
  # Define reactive variables
  prev_VS_DTG <- reactive({input$prev_VS_DTG/100})
  prec_VS_DTG <- reactive({input$prec_VS_DTG/100})
  q_DTG <- reactive({input$q_DTG/100})
  alpha <- 0.05
  labFail <- 0.15
  ICC <- 0.004278927
  DE_info <- 1.5

  # Function to calculate sample size per clinic using FPC and Wald-type intervals
  calcSampleSize <- function(prev, CI, n, N, M, q, ICC, DE_info, labFail, alpha) {
    # q: proportion of the total eligible population that belongs to the subgroup of interest
    k_eff <- (qt(1-alpha/2, df=n-1))^2*prev*(1-prev)/CI^2
    m <- (1-ICC)/(n/(DE_info*k_eff) - ICC*(1-n/N) + N/(M*q)*(1-ICC))
    m <- m/(1-labFail)
    return(ceiling(m))
  }

  ### Calculate sample size
  sample_size_DTG_clinic <- reactive({calcSampleSize(prev=prev_VS_DTG(), CI=prec_VS_DTG(), 
                                                     n=input$n, N=input$N, M=input$M,
                                         q=q_DTG(), ICC=ICC, DE_info=DE_info, 
                                         labFail=labFail, alpha=alpha)})
  sample_size_DTG <- reactive({sample_size_DTG_clinic()*input$n})

  ### Output tables
  # Table of user-specified parameter values
  assumptions_DTG <- reactive({
    data.frame(
      Assumptions = c("Expected prevalence of VS for patients on DTG-containing regimens",
                      "Desired absolute precision (95% CI half-width)",
                      "Number of clinics sampled",
                      "Total number of clinics",
                      "Total number of individuals on ART",
                      "ICC",
                      "Design effect due to imperfect weights",
                      "Significance Level",
                      "Laboratory Failure Rate"),
      Value = as.character(c(paste0(prev_VS_DTG()*100, "%"),
                             paste0("\u00B1", prec_VS_DTG()*100, "%"),
                             input$n,
                             input$N,
                             input$M,
                             ICC,
                             DE_info,
                             alpha,
                             paste0(labFail*100, "%"))),
      stringsAsFactors = FALSE)
  })

  # Render table
  output$values_DTG <- renderTable({assumptions_DTG()})

  # Sample size output
  output$clinic_DTG <- renderText({
    paste0("Sample size per clinic, m<sub>DTG</sub>: ",
           a(sample_size_DTG_clinic(), style = "color:red"))

  })

  output$sample_size_DTG <- renderText({
    paste0("Sample size across clinics:  ",
           a(sample_size_DTG(), style = "color:red"))
  })

  output$text_DTG <- renderText({
    "Sample sizes necessary for estimating the prevalence of viral suppression
    among patients taking DTG-containing regimens."
  })


  # ====================================
  # Server code for overall calculations

  ### Define variables and functions

  # Define reactive variables
  prev_VS_O <- reactive({input$prev_VS_O/100})
  prec_VS_O <- reactive({input$prec_VS_O/100})

  # Calculate sample size
  sample_size_O_clinic <- reactive({calcSampleSize(prev=prev_VS_O(), CI=prec_VS_O(),
                                                   n=input$n, N=input$N, M=input$M,
                                                     q=1, ICC=ICC, DE_info=DE_info, 
                                                     labFail=labFail, alpha=alpha)})
  sample_size_O <- reactive({sample_size_O_clinic()*input$n})

  ### Output tables
  # Table of user-specified parameter values
  assumptions_O <- reactive({
    data.frame(
      Assumptions = c("Expected prevalence of VS for patients overall",
                      "Desired absolute precision (95% CI half-width)",
                      "Number of clinics sampled",
                      "Total number of clinics",
                      "Total number of individuals on ART",
                      "ICC",
                      "Design effect due to imperfect weights",
                      "Significance Level",
                      "Laboratory Failure Rate"),
      Value = as.character(c(paste0(prev_VS_O()*100, "%"),
                             paste0("\u00B1", prec_VS_O()*100, "%"),
                             input$n,
                             input$N,
                             input$M,
                             ICC,
                             DE_info,
                             alpha,
                             paste0(labFail*100, "%"))),
      stringsAsFactors = FALSE)
  })

  # Render table
  output$values_O <- renderTable({assumptions_O()})

  # Sample size output
  output$clinic_O <- renderText({
    paste0("Sample size per clinic, m<sub>overall</sub>: ",
           a(sample_size_O_clinic(), style = "color:red"))

  })

  output$sample_size_O <- renderText({
    paste0("Sample size across clinics: ",
           a(sample_size_O(), style = "color:red"))
  })

  output$text_O <- renderText({
    "Sample sizes necessary for estimating the prevalence of viral suppression among all patients."
  })
  
  
  ##================================================================================
  # Server code for non-DTG and total calculations
  
  q_nonDTG <- reactive({min(max(0, 1-q_DTG()), 1)})
  
  ## Calculating m_nonDTG and m_total
  sample_size_nonDTG_clinic <- reactive({
    ceiling(sample_size_O_clinic() * q_nonDTG())
  })
  
  sample_size_nonDTG <- reactive({sample_size_nonDTG_clinic()*input$n})
  
  sample_size_total_clinic <- reactive({
    ceiling(sample_size_nonDTG_clinic() + sample_size_DTG_clinic())
  })
  
  sample_size_total <- reactive({sample_size_total_clinic()*input$n})
  
  # Text output for non-DTG and total
  output$clinic_non <- renderText({
    paste0("Sample size per clinic, m<sub>nonDTG</sub>: ",
           a(sample_size_nonDTG_clinic(), style = "color:red"))
  })
  
  output$sample_size_non <- renderText({
    paste0("Sample size across clinics: ",
           a(sample_size_nonDTG(), style = "color:red"))
  })
  
  output$text_non <- renderText({
    "Sample sizes necessary from patients taking non-DTG-containing regimens to ensure sufficient sample 
    size for overall estimate of ADR."
  })
  
  output$prop_non <- renderText({
    paste0("Percentage of ART patients on non-DTG-containing regimens: ", 
           a(paste0(q_nonDTG()*100, "%"), style = "color:blue"))
  })
  
  ## Calculating total sample sizes required
  output$clinic_total <- renderText({
    paste0("Sample size per clinic, m<sub>DTG</sub> + m<sub>nonDTG</sub>: ",
           a(sample_size_total_clinic(), style = "color:red"))
  })
  
  output$sample_size_total <- renderText({
    paste0("Sample size across clinics: ",
           a(sample_size_total(), style = "color:red"))
  })
  
  output$text_total <- renderText({
    "Total sample sizes necessary for both the DTG and overall estimates."
  })
  
  
  ##===========================================================================
  ### Serve code for precision of ADR estimates for DTG and overall 
  
  # Define variables
  prev_ADR_DTG <- reactive({input$prev_ADR_DTG/100})
  prev_ADR_O <- reactive({input$prev_ADR_O/100})
  genoFail <- 0.3
  
  # Function to calculate the precision using FPC and Wald-type intervals
  calc_precision <- function(m, prev, n, N, M, q, q_VNS, ICC, DE_info, labFail, genoFail, alpha) {
    # m: per-clinic sample size for VS estimate
    # q: proportion of the total eligible population that belongs to the subgroup of interest
    # q_VNS: proportion of individuals on ART with VNS
    
    m_ADR <- ceiling(m*(1-labFail)*(1-genoFail)*q_VNS)
    M_ADR <- ceiling(M*q*(1-labFail))
    
    DE_clust <- (1-m_ADR*N/M_ADR) + ((1-n/N)*m_ADR - (1-m_ADR*N/M_ADR))*ICC
    k_eff_ADR <- n*m_ADR/(DE_info*DE_clust)
    prec <- qt(1-alpha/2, df=n-1)*sqrt(prev*(1-prev)/k_eff_ADR)
    
    return(round(prec, 3))
  }
  
  ### Calculate precision
  prec_ADR_DTG <- reactive({calc_precision(m=sample_size_DTG_clinic(), prev=prev_ADR_DTG(), 
                                           n=input$n, N=input$N, M=input$M,
                                           q=q_DTG(), q_VNS=1-prev_VS_DTG(), ICC=ICC, DE_info=DE_info, 
                                           labFail=labFail, genoFail=genoFail, alpha=alpha)})
  prec_ADR_O <- reactive({calc_precision(m=sample_size_total_clinic(), prev=prev_ADR_O(), 
                                         n=input$n, N=input$N, M=input$M,
                                         q=1, q_VNS=1-prev_VS_O(), ICC=ICC, DE_info=DE_info, 
                                         labFail=labFail, genoFail=genoFail, alpha=alpha)})
  
  # Render output
  output$prec_ADR_DTG <- renderText({
    paste0("Precision for ADR estimate of DTG-specific resistance among patients on DTG-containing regimens: ",
           a(paste0("\u00B1", prec_ADR_DTG()*100, "%"), style = "color:red"))
  })
  output$prec_ADR_O <- renderText({
    paste0("Precision for ADR estimate of overall resistance among all patients: ",
           a(paste0("\u00B1", prec_ADR_O()*100, "%"), style = "color:red"))
  })
  
  # Table of user-specified parameter values
  assumptions_ADR_DTG <- reactive({
    data.frame(
      Assumptions = c("Expected prevalence of DTG-specific ADR for patients on DTG-containing regimens with VNS",
                      "VS sample size for patients on DTG-containing regimens",
                      "Expected proportion of patients with VNS on DTG-containing regimens",
                      "Number of clinics sampled",
                      "Total number of clinics",
                      "Total number of individuals on ART",
                      "ICC",
                      "Design effect due to imperfect weights",
                      "Significance Level",
                      "Laboratory Failure Rate",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_ADR_DTG()*100, "%"),
                             sample_size_DTG(),
                             1-prev_VS_DTG(),
                             input$n,
                             input$N,
                             input$M,
                             ICC,
                             DE_info,
                             alpha,
                             paste0(labFail*100, "%"),
                             paste0(genoFail*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render table
  output$values_ADR_DTG <- renderTable({assumptions_ADR_DTG()})
  
  # Table of user-specified parameter values
  assumptions_ADR_O <- reactive({
    data.frame(
      Assumptions = c("Expected prevalence of overall ADR for all patients with VNS",
                      "VS sample size for all patients",
                      "Expected proportion of patients with VNS",
                      "Number of clinics sampled",
                      "Total number of clinics",
                      "Total number of individuals on ART",
                      "ICC",
                      "Design effect due to imperfect weights",
                      "Significance Level",
                      "Laboratory Failure Rate",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_ADR_O()*100, "%"),
                             sample_size_total(),
                             1-prev_VS_O(),
                             input$n,
                             input$N,
                             input$M,
                             ICC,
                             DE_info,
                             alpha,
                             paste0(labFail*100, "%"),
                             paste0(genoFail*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render table
  output$values_ADR_O <- renderTable({assumptions_ADR_O()})
  
}


############################
# Build Shiny app
############################

shinyApp(ui, server)