#==========================================================#
# Shiny App for HIVDR Sample Size Calculations
# 
# Results displayed at 
# https://worldhealthorg.shinyapps.io/ADR_LabBasedMethod_2/
# https://smwu.shinyapps.io/HIVDR_2/
#==========================================================#

# Load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)

##########################
# Define UI code
##########################

ui <- dashboardPage(
  
  dashboardHeader(
    title = h2("Sample size calculations for clinic-based acquired HIV drug resistance survey"),
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
      menuItem("Inputs", tabName = "inputs"),
      menuItem("Outputs", tabName = "outputs")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "inputs",
              useShinyjs(),
              fluidPage(
                fluidRow(
                  box(title = "Total number of clinics and patients on ART", width = 12, 
                      status = "warning", solidHeader = TRUE,
                      numericInput("N", label = h4("What is the total number of clinics in your country?"), 300,
                                   min=1, step=1),
                      numericInput("M", label = h4("What is the total number of patients on ART in your country?"), 20000,
                                   min=1, step=1)
                  )
                ),
                fluidRow(
                  box(title = "Percentage on DTG-containing regimens", width = 12, 
                      status = "primary", solidHeader = TRUE,
                      
                      numericInput("q_DTG", label = h4(HTML("Input the national percentage of individuals on ART who are 
                                                            on DTG-containing regimens (%).")), 
                                   60, min=0, max=100, step=1)
                  )
                ),
                fluidRow(
                  box(status = "warning", width = 12, title = "Finite population correction", solidHeader = TRUE,
                      h4(htmlOutput("fpc_text")),
                      radioButtons("fpc", label = h5("The finite population correction will reduce the required sample 
                      size and is recommended if the total number of clinics or the total number of patients on ART is small."), 
                                   choices = list("Yes" = 1, "No" = 2), selected = 1))
                  
                ),
                fluidRow(
                  box(title = "Number of clinics to be sampled", width = 12, status = "success", solidHeader = TRUE,
                      numericInput("n", label = h4(HTML("Input the number of clinics to sample. 
                                                        Must be a whole number")), 
                                   30, min=1, step=1),
                      uiOutput("minimum_clinics")
                  )
                ),
                div(style = 'text-align:center', actionButton("submit", "Submit", width = '150px', 
                                                              style = "font-weight: bold; border-color: black; font-size: 18px")), br()
              )),
      
      tabItem(tabName = "outputs",
              fluidRow(
                box(title = "Sample Size for DTG Case Specimens", status = "primary",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    # Display table of assumptions and sample size required for DTG
                    h5(htmlOutput("text_DTG")), br(),
                    tableOutput("values_DTG"),
                    h4(htmlOutput("clinic_DTG")),
                    h4(htmlOutput("sample_size_DTG"))
                ),
                
                box(title = "Overall Estimated Sample Size", status = "primary", 
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    # Display table of assumptions and sample size required for overall
                    h5(htmlOutput("text_O")), br(),
                    tableOutput("values_O"),
                    h4(htmlOutput("clinic_O")),
                    h4(htmlOutput("sample_size_O"))
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
                    h4(htmlOutput("clinic_non")),
                    h4(htmlOutput("sample_size_non"))
                ),
                
                box(title = "Total Sample Size", status = "warning",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    # Display total sample size required
                    h5(htmlOutput("text_total")), br(), 
                    h4(htmlOutput("clinic_total")),
                    h4(htmlOutput("sample_size_total"))
                )),
              fluidRow(
                box(title = "Precision for DTG ADR Estimates", status = "success",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    tableOutput("values_ADR_DTG"),
                    h4(htmlOutput("prec_ADR_DTG"))
                ),
                
                box(title = "Precision for Overall ADR Estimates", status = "success",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    tableOutput("values_ADR_O"),
                    h4(htmlOutput("prec_ADR_O"))
                ))
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
  prev_VS_DTG <- 0.9
  prec_VS_DTG <- 0.05
  prev_VS_O <- 0.85
  prec_VS_O <- 0.05
  q_DTG <- reactive({input$q_DTG/100})
  q_nonDTG <- reactive({min(max(0, 1-q_DTG()), 1)})
  alpha <- 0.05
  labFail <- 0.15
  ICC <- reactive({ifelse(input$option == 1, 0.09, 0.06)})  # Previous ICC <- 0.004278927 
  DE_info <- 1.5
  
  # Define N and M according to whether FPC is to be used. If not, N=100000 and M=100000000
  N <- reactive({ifelse(input$fpc == 1, input$N, 100000)})
  M <- reactive({ifelse(input$fpc == 1, input$M, 100000000)})
  
  # Function to calculate sample size per clinic using FPC and Wald-type intervals
  # Returns number of samples per clinic and effective sample size
  calcSampleSize <- function(prev, CI, n, N, M, q, ICC, DE_info, labFail, alpha) {
    # q: proportion of the total eligible population that belongs to the subgroup of interest
    k_eff <- (qt(1-alpha/2, df=n-1))^2*prev*(1-prev)/CI^2
    m <- (1-ICC)/(n/(DE_info*k_eff) - ICC*(1-n/N) + N/(M*q)*(1-ICC))
    m <- m/(1-labFail)
    return(list(m=ceiling(m), keff = k_eff))
  }
  
  # FPC text
  output$fpc_text <- renderText({
    return("Would you like to apply the finite population correction to the sample size estimates?")
  })
  
  ### Obtain the minimum required number of clinics to ensure the total sample size is under 1500
  get_min_clinics <- function(prev_VS_DTG, prev_VS_O, prec_VS_DTG, prec_VS_O, 
                              n, N, M, q, ICC, DE_info, labFail, alpha) {
    
    m_DTG <- calcSampleSize(prev=prev_VS_DTG, CI=prec_VS_DTG, n=n, N=N, M=M,
                            q=q, ICC=ICC, DE_info=DE_info, labFail=labFail, alpha=alpha)
    m_O <- calcSampleSize(prev=prev_VS_O, CI=prec_VS_O, n=n, N=N, M=M,
                          q=1, ICC=ICC, DE_info=DE_info, labFail=labFail, alpha=alpha)
    m_DTG_mod <- max(m_DTG[[1]], ceiling(m_O[[1]]*q))
    m_nonDTG <- ceiling(m_O[[1]]*(1-q))
    m_total <- ceiling(m_nonDTG + m_DTG_mod)
    total_SS <- m_total * n
    min_clinics_DTG <- ceiling((N+M)*ICC / M / (1/(DE_info*m_DTG[[2]]) + ICC/N))
    min_clinics_O <- ceiling((N+M)*ICC / M / (1/(DE_info*m_O[[2]]) + ICC/N))
    min_clinics <- max(min_clinics_DTG, min_clinics_O)
    n <- min_clinics
    
    while(any(c(m_DTG[[1]], m_O[[1]], m_nonDTG, m_total) < 0) | total_SS > 1500) {
      n <- n + 1  # increment the number of clinics to be sampled
      
      m_DTG <- calcSampleSize(prev=prev_VS_DTG, CI=prec_VS_DTG, n=n, N=N, M=M,
                              q=q, ICC=ICC, DE_info=DE_info, labFail=labFail, alpha=alpha)
      m_O <- calcSampleSize(prev=prev_VS_O, CI=prec_VS_O, n=n, N=N, M=M,
                            q=1, ICC=ICC, DE_info=DE_info, labFail=labFail, alpha=alpha)
      m_DTG_mod <- max(m_DTG[[1]], ceiling(m_O[[1]]*q))
      m_nonDTG <- ceiling(m_O[[1]]*(1-q))
      m_total <- ceiling(m_nonDTG + m_DTG_mod)
      total_SS <- m_total * n
    }
    
    min_clinics <- n
    
    return(list(min_clinics, m_DTG_mod, m_O[[1]], m_nonDTG, m_total, total_SS))
  }
  
  
  #========================================================================================
  # Check if number of clinics to sample is larger than required minimum number of clinics
  # Option 1 with ICC=0.09
  min_clinics <- reactive({get_min_clinics(prev_VS_DTG=prev_VS_DTG, prev_VS_O=prev_VS_O, 
                                           prec_VS_DTG=prec_VS_DTG, prec_VS_O=prec_VS_O, 
                                           n=input$n, N=N(), M=M(), q=q_DTG(), ICC=0.09, 
                                           DE_info=DE_info, labFail=labFail, alpha=alpha)})
  
  # Option 2 with ICC=0.06
  min_clinics_op2 <- reactive({get_min_clinics(prev_VS_DTG=prev_VS_DTG, prev_VS_O=prev_VS_O,
                                               prec_VS_DTG=prec_VS_DTG, prec_VS_O=prec_VS_O,
                                               n=input$n, N=N(), M=M(), q=q_DTG(), ICC=0.06,
                                               DE_info=DE_info, labFail=labFail, alpha=alpha)})
  
  # values <- reactiveValues(revised = FALSE)
  revised <- FALSE
  
  output$minimum_clinics <- renderUI({
    if (min_clinics()[[1]] > input$n) {
      revised <- TRUE
      tagList(
        a(HTML("Number of clinics to be sampled is too small."), style = "color:red; font-size: 20px"),
        radioButtons("option", label = h4(paste0("Can you sample at least ", min_clinics()[[1]], " clinics?")),
                     choices = list("Yes" = 1, "No" = 2), selected = 1),
        conditionalPanel("input.option == 1",
                         # a(HTML(paste0("A minimum of ", min_clinics()[[1]], " clinics must be sampled.")), 
                         #   style = "color:red; font-size: 20px"),
                         numericInput("n_sampled",
                                      label = h4(paste("Input the updated number of clinics to be sampled. Minimum number of clinics required is ",
                                                       min_clinics()[[1]], ".")), min_clinics()[[1]],
                                      min=min_clinics()[[1]], step=1),
        ),
        conditionalPanel("input.option == 2",   # ICC=0.06
                         numericInput("n_sampled",
                                      label = h4(paste("Input the updated number of clinics to be sampled. Minimum number of clinics required is ",
                                                       min_clinics_op2()[[1]], ".")), min_clinics_op2()[[1]],
                                      min=min_clinics_op2()[[1]], step=1)
        ))
    }
  })
  
  # # Define number of clinics sampled as "n" or "n_sampled"
  n_clinics <- eventReactive(input$submit, {
    ifelse(min_clinics()[[1]] > input$n, input$n_sampled, input$n)
  })
  
  
  #================================== Sample Size Outputs =========================================
  
  ### DTG 
  
  # Calculate sample size
  sample_size_DTG_clinic <- eventReactive(input$submit, {calcSampleSize(prev=prev_VS_DTG, CI=prec_VS_DTG, 
                                                                        n=n_clinics(), N=N(), M=M(),
                                                                        q=q_DTG(), ICC=ICC(), 
                                                                        DE_info=DE_info, labFail=labFail, alpha=alpha)})
  
  ### Overall calculations
  
  # Calculate sample size
  sample_size_O_clinic <- eventReactive(input$submit, {calcSampleSize(prev=prev_VS_O, CI=prec_VS_O,
                                                                      n=n_clinics(), N=N(), M=M(),
                                                                      q=1, ICC=ICC(), 
                                                                      DE_info=DE_info, labFail=labFail, alpha=alpha)})
  sample_size_O <- eventReactive(input$submit, {sample_size_O_clinic()[[1]]*n_clinics()})
  
  # ===========================================================
  # Server code for potential modification of DTG sample size 
  # Depends on overall sample size and proportion on DTG
  
  ## Modified sample size for DTG for when n_overall > n_DTG
  sample_size_DTG_clinic_mod <- eventReactive(input$submit, {
    n_DTG_1 <- sample_size_DTG_clinic()[[1]]
    n_DTG_2 <- ceiling(sample_size_O_clinic()[[1]] * q_DTG())
    max(n_DTG_1, n_DTG_2)
  })
  
  ## Sample size for DTG across clinics
  sample_size_DTG <- eventReactive(input$submit, {sample_size_DTG_clinic_mod()*n_clinics()})
  
  # Sample size output
  output$clinic_DTG <- renderText({
    paste0("Sample size per clinic, m<sub>DTG</sub>: ",
           a(sample_size_DTG_clinic_mod(), style = "color:red"))
  })
  
  output$sample_size_DTG <- renderText({
    paste0("Sample size across clinics:  ",
           a(sample_size_DTG(), style = "color:red"))
  })
  
  
  ### Output tables
  # Table of user-specified parameter values
  assumptions_DTG <- eventReactive(input$submit, {
    data.frame(
      Assumptions = c("Expected prevalence of VS for patients on DTG-containing regimens",
                      "Desired absolute precision (95% CI half-width)",
                      "Number of clinics sampled",
                      "Total number of clinics",
                      "Total number of individuals on ART",
                      "ICC",
                      "Design effect due to imperfect weights",
                      # "Significance Level",
                      "Laboratory Failure Rate"),
      Value = as.character(c(paste0(prev_VS_DTG*100, "%"),
                             paste0("\u00B1", prec_VS_DTG*100, "%"),
                             n_clinics(),
                             N(),
                             M(),
                             ICC(),
                             DE_info,
                             # alpha,
                             paste0(labFail*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render table
  output$values_DTG <- renderTable({assumptions_DTG()})
  
  output$text_DTG <- renderText({
    "Sample sizes necessary for estimating the prevalence of viral suppression
    among patients taking DTG-containing regimens."
  })
  
  
  ### Output tables
  # Table of user-specified parameter values
  assumptions_O <- eventReactive(input$submit, {
    data.frame(
      Assumptions = c("Expected prevalence of VS for patients overall",
                      "Desired absolute precision (95% CI half-width)",
                      "Number of clinics sampled",
                      "Total number of clinics",
                      "Total number of individuals on ART",
                      "ICC",
                      "Design effect due to imperfect weights",
                      # "Significance Level",
                      "Laboratory Failure Rate"),
      Value = as.character(c(paste0(prev_VS_O*100, "%"),
                             paste0("\u00B1", prec_VS_O*100, "%"),
                             n_clinics(),
                             N(),
                             M(),
                             ICC(),
                             DE_info,
                             # alpha,
                             paste0(labFail*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render table
  output$values_O <- renderTable({assumptions_O()})
  
  # Sample size output
  output$clinic_O <- renderText({
    paste0("Sample size per clinic, m<sub>overall</sub>: ",
           a(sample_size_O_clinic()[[1]], style = "color:red"))
  })
  
  output$sample_size_O <- renderText({
    paste0("Sample size across clinics: ",
           a(sample_size_O(), style = "color:red"))
  })
  
  output$text_O <- renderText({
    "Sample sizes necessary for estimating the prevalence of viral suppression among all patients."
  })
  
  
  ### Non-DTG and total calculations
  
  ## Calculating m_nonDTG and m_total
  sample_size_nonDTG_clinic <- eventReactive(input$submit, {
    ceiling(sample_size_O_clinic()[[1]] * q_nonDTG())
  })
  
  sample_size_nonDTG <- eventReactive(input$submit, {sample_size_nonDTG_clinic()*n_clinics()})
  
  sample_size_total_clinic <- eventReactive(input$submit, {
    ceiling(sample_size_nonDTG_clinic() + sample_size_DTG_clinic_mod())
  })
  
  sample_size_total <- eventReactive(input$submit, {sample_size_total_clinic()*n_clinics()})
  
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
  prev_ADR_DTG <- 0.035
  prev_ADR_O <- 0.5
  genoFail <- 0.3
  DE <- 1.5
  
  # Function to calculate the precision using FPC and Wald-type intervals using the simple DE=1.5 adjustment
  calc_precision <- function(m, prev, n, q, q_VNS, DE, labFail, genoFail, alpha) {
    # m: per-clinic sample size for VS estimate
    # q: proportion of the total eligible population that belongs to the subgroup of interest
    # q_VNS: proportion of individuals on ART with VNS
    
    M_ADR <- ceiling(n*m*(1-labFail)*(1-genoFail)*q_VNS)
    
    k_eff_ADR <- M_ADR/DE
    prec <- qt(1-alpha/2, df=n-1)*sqrt(prev*(1-prev)/k_eff_ADR)
    
    return(round(prec, 3))
  }
  
  ### Calculate precision
  prec_ADR_DTG <- eventReactive(input$submit, {calc_precision(m=sample_size_DTG_clinic_mod(), prev=prev_ADR_DTG,
                                                              n=n_clinics(), 
                                                              q=q_DTG(), q_VNS=1-prev_VS_DTG, DE=DE,
                                                              labFail=labFail, genoFail=genoFail, alpha=alpha)})
  prec_ADR_O <- eventReactive(input$submit, {calc_precision(m=sample_size_total_clinic(), prev=prev_ADR_O,
                                                            n=n_clinics(),
                                                            q=1, q_VNS=1-prev_VS_O, DE=DE,
                                                            labFail=labFail, genoFail=genoFail, alpha=alpha)})
  
  
  # # Function to calculate the precision using FPC and Wald-type intervals
  # calc_precision <- function(m, prev, n, N, M, q, q_VNS, ICC, DE_info, labFail, genoFail, alpha) {
  #   # m: per-clinic sample size for VS estimate
  #   # q: proportion of the total eligible population that belongs to the subgroup of interest
  #   # q_VNS: proportion of individuals on ART with VNS
  #   
  #   m_ADR <- ceiling(m*(1-labFail)*(1-genoFail)*q_VNS)
  #   M_ADR <- ceiling(M*q*(1-labFail))
  #   
  #   DE_clust <- (1-m_ADR*N/M_ADR) + ((1-n/N)*m_ADR - (1-m_ADR*N/M_ADR))*ICC
  #   k_eff_ADR <- n*m_ADR/(DE_info*DE_clust)
  #   prec <- qt(1-alpha/2, df=n-1)*sqrt(prev*(1-prev)/k_eff_ADR)
  #   
  #   return(round(prec, 3))
  # }
  
  # ### Calculate precision
  # prec_ADR_DTG <- eventReactive(input$submit, {calc_precision(m=sample_size_DTG_clinic_mod(), prev=prev_ADR_DTG, 
  #                                          n=n_clinics(), N=N(), M=M(),
  #                                          q=q_DTG(), q_VNS=1-prev_VS_DTG, ICC=ICC, DE_info=DE_info, 
  #                                          labFail=labFail, genoFail=genoFail, alpha=alpha)})
  # prec_ADR_O <- eventReactive(input$submit, {calc_precision(m=sample_size_total_clinic(), prev=prev_ADR_O, 
  #                                        n=n_clinics(), N=N(), M=M(),
  #                                        q=1, q_VNS=1-prev_VS_O, ICC=ICC, DE_info=DE_info, 
  #                                        labFail=labFail, genoFail=genoFail, alpha=alpha)})
  
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
  assumptions_ADR_DTG <- eventReactive(input$submit, {
    data.frame(
      Assumptions = c("Expected prevalence of DTG-specific ADR for patients on DTG-containing regimens with VNS",
                      "VS sample size for patients on DTG-containing regimens",
                      "Expected proportion of patients with VNS on DTG-containing regimens",
                      "Number of clinics sampled",
                      "Design effect",
                      # "Significance Level",
                      "Laboratory Failure Rate",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_ADR_DTG*100, "%"),
                             sample_size_DTG(),
                             paste0((1-prev_VS_DTG)*100, "%"),
                             n_clinics(),
                             DE,
                             # alpha,
                             paste0(labFail*100, "%"),
                             paste0(genoFail*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  
  # Table of user-specified parameter values
  assumptions_ADR_O <- eventReactive(input$submit, {
    data.frame(
      Assumptions = c("Expected prevalence of DTG-specific ADR for all patients VNS",
                      "VS sample size for all patients",
                      "Expected percentage of patients with VNS",
                      "Number of clinics sampled",
                      "Design effect",
                      # "Significance Level",
                      "Laboratory Failure Rate",
                      "Genotyping Failure Rate"),
      Value = as.character(c(paste0(prev_ADR_O*100, "%"),
                             sample_size_total(),
                             paste0((1-prev_VS_O)*100, "%"),
                             n_clinics(),
                             DE,
                             # alpha,
                             paste0(labFail*100, "%"),
                             paste0(genoFail*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # # Table of user-specified parameter values
  # assumptions_ADR_DTG <- eventReactive(input$submit, {
  #   data.frame(
  #     Assumptions = c("Expected prevalence of DTG-specific ADR for patients on DTG-containing regimens with VNS",
  #                     "VS sample size for patients on DTG-containing regimens",
  #                     "Expected proportion of patients with VNS on DTG-containing regimens",
  #                     "Number of clinics sampled",
  #                     "Total number of clinics",
  #                     "Total number of individuals on ART",
  #                     "ICC",
  #                     "Design effect due to imperfect weights",
  #                     # "Significance Level",
  #                     "Laboratory Failure Rate",
  #                     "Genotyping Failure Rate"),
  #     Value = as.character(c(paste0(prev_ADR_DTG*100, "%"),
  #                            sample_size_DTG(),
  #                            paste0((1-prev_VS_DTG)*100, "%"),
  #                            n_clinics(),
  #                            N(),
  #                            M(),
  #                            ICC,
  #                            DE_info,
  #                            # alpha,
  #                            paste0(labFail*100, "%"),
  #                            paste0(genoFail*100, "%"))),
  #     stringsAsFactors = FALSE)
  # })
  
  # Render table
  output$values_ADR_DTG <- renderTable({assumptions_ADR_DTG()})
  
  # # Table of user-specified parameter values
  # assumptions_ADR_O <- eventReactive(input$submit, {
  #   data.frame(
  #     Assumptions = c("Expected prevalence of DTG-specific ADR for all patients VNS",
  #                     "VS sample size for all patients",
  #                     "Expected percentage of patients with VNS",
  #                     "Number of clinics sampled",
  #                     "Total number of clinics",
  #                     "Total number of individuals on ART",
  #                     "ICC",
  #                     "Design effect due to imperfect weights",
  #                     # "Significance Level",
  #                     "Laboratory Failure Rate",
  #                     "Genotyping Failure Rate"),
  #     Value = as.character(c(paste0(prev_ADR_O*100, "%"),
  #                            sample_size_total(),
  #                            paste0((1-prev_VS_O)*100, "%"),
  #                            n_clinics(),
  #                            N(),
  #                            M(),
  #                            ICC,
  #                            DE_info,
  #                            # alpha,
  #                            paste0(labFail*100, "%"),
  #                            paste0(genoFail*100, "%"))),
  #     stringsAsFactors = FALSE)
  # })
  
  # Render table
  output$values_ADR_O <- renderTable({assumptions_ADR_O()})
  
  
  
}


############################
# Build Shiny app
############################

shinyApp(ui, server)

