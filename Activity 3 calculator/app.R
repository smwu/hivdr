#==========================================================#
# Shiny App for HIVDR Sample Size Calculations for the
# Clinic-Based Method
# 
# Results displayed at 
# https://worldhealthorg.shinyapps.io/ADR_ClinicBasedMethod/
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
                  box(title = "Total number of clinics and people receiving ART", width = 12, 
                      status = "warning", solidHeader = TRUE,
                      radioButtons("population", label = h4("What is the survey population of interest?"),
                                   choices = list("Adults" = 1, 
                                                  "Children and adolescents" = 2), selected = 1),
                      uiOutput("C"),
                      uiOutput("N")
                  )
                ),
                fluidRow(
                  box(title = "Percentage of people receiving dolutegravir-containing regimens", width = 12, 
                      status = "primary", solidHeader = TRUE,
                      uiOutput("q_DTG")

                  )
                ),

                fluidRow(
                  box(title = "Number of clinics to be sampled", width = 12, status = "success", solidHeader = TRUE,
                      uiOutput("minimum_clinics"),
                      h4(htmlOutput("warning"))
                  )
                ),
                div(style = 'text-align:center', actionButton("submit", "Submit", width = '150px', 
                                                              style = "font-weight: bold; border-color: black; font-size: 18px")), br()
              )),
      
      tabItem(tabName = "outputs",
          fluidPage(  
            # make the horizontal line more visible
            tags$head(
              tags$style(HTML("hr {border-bottom: 1px solid #000000;}"))
            ),
              fluidRow(
                column(6, box(title = "Assumptions for Sample Size Calculations", status = "warning", collapsible = TRUE, width = NULL, solidHeader = TRUE,
                              # h5(htmlOutput("table_text")),
                              tableOutput("values_DTG"))),
                column(6, fluidRow(box(title = textOutput("title_ss_DTG"), status = "primary",
                                       collapsible = TRUE, width = NULL, solidHeader = TRUE,
                                       # Display sample size required for DTG
                                       h5(htmlOutput("text_DTG")),
                                       h4(htmlOutput("clinic_DTG")),
                                       h4(htmlOutput("sample_size_DTG"))
                )),
                fluidRow(box(title = textOutput("title_ss_non"), status = "primary",
                             collapsible = TRUE, width = NULL, solidHeader = TRUE,
                             # Display sample size required for non-DTG
                             h5(htmlOutput("text_non")),
                             h4(htmlOutput("prop_non")),
                             h4(htmlOutput("clinic_non")),
                             h4(htmlOutput("sample_size_non"))
                )),
                fluidRow(box(title = "Total sample size", status = "primary",
                             collapsible = TRUE, width = NULL, solidHeader = TRUE,
                             # Display total sample size required
                             h5(htmlOutput("text_total")),
                             h4(htmlOutput("clinic_total")),
                             h4(htmlOutput("sample_size_total"))
                ))
              )),

              hr(), 
              fluidRow(align = "center", h3("Resulting Anticipated Precision for the ADR Estimates")),
              br(),
              br(),

              fluidRow(
                box(title = "Precision for dolutegravir ADR estimates", status = "success",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    tableOutput("values_ADR_DTG"),
                    h4(htmlOutput("prec_ADR_DTG"))
                ),
                
                box(title = "Precision for overall ADR estimates", status = "success",
                    collapsible = TRUE, width = 6, solidHeader = TRUE,
                    tableOutput("values_ADR_O"),
                    h4(htmlOutput("prec_ADR_O"))
                )
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
  prev_VS_DTG <- 0.9
  prec_VS_DTG <- 0.05
  prev_VS_O <- 0.85
  prec_VS_O <- 0.05
  q_DTG <- reactive({input$q_DTG/100})
  q_nonDTG <- reactive({min(max(0, 1-q_DTG()), 1)})
  alpha <- 0.05
  labFail <- 0.1
  DE_info <- 1.5

  
  # Function to calculate sample size per clinic using FPC and Wald-type intervals
  # Returns number of samples per clinic and effective sample size
  calcSampleSize <- function(prev, CI, c, C, N, q, ICC, DE_info, labFail, alpha) {
    # q: proportion of the total eligible population that belongs to the subgroup of interest
    # c: Number of clinics to be sampled
    # C: Total number of clinics providing ART to the population of interest in the country
    # N: Total number of individuals receiving ART in the country in the population of interest
    
    k_eff <- (qt(1-alpha/2, df=c-1))^2*prev*(1-prev)/CI^2
    m <- (1-ICC)/(c/(DE_info*k_eff) - ICC*(1-c/C) + C/(N*q)*(1-ICC))
    m <- m/(1-labFail)
    return(list(m=ceiling(m), keff = ceiling(k_eff)))
  }
  
  # FPC text
  output$fpc_text <- renderText({
    return("Would you like to apply the finite population correction to the sample size estimates?")
  })
  
  ### Obtain the minimum required number of clinics to ensure the total sample size is under 1500
  get_min_clinic_threshold <- function(prev_VS_DTG, prev_VS_O, prec_VS_DTG, prec_VS_O, 
                                       C, N, q, ICC, DE_info, labFail, alpha) {
    for (c in 2:C) {
      m_DTG <- calcSampleSize(prev=prev_VS_DTG, CI=prec_VS_DTG, c=c, C=C, N=N,
                              q=q, ICC=ICC, DE_info=DE_info, labFail=labFail, alpha=alpha)
      m_O <- calcSampleSize(prev=prev_VS_O, CI=prec_VS_O, c=c, C=C, N=N,
                            q=1, ICC=ICC, DE_info=DE_info, labFail=labFail, alpha=alpha)
      
      min_clinics_DTG <- ceiling(((C+N*q)*ICC-C) / (N*q) / (1/(DE_info*m_DTG[[2]]) + ICC/C))
      min_clinics_O <- ceiling(((C+N)*ICC-C) / N / (1/(DE_info*m_O[[2]]) + ICC/C))
      min_clinics <- max(min_clinics_DTG, min_clinics_O)
      
      m_DTG_mod <- max(m_DTG[[1]], ceiling(m_O[[1]]*q))
      m_nonDTG <- ceiling(m_O[[1]]*(1-q))
      m_total <- ceiling(m_nonDTG + m_DTG_mod)
      total_SS <- m_total * c

      if ((c >= min_clinics) & (total_SS <= 1500)) {
        return(c)
      }
    }
    return(C)
  }
  
  #========================================================================================
  # Get input values depending on population (adults vs. children/adolescents)
  pop <- reactive({ifelse(input$population==1, "adults", "children and adolescents")})
  output$C <- renderUI({
    numericInput("C", label = h4(HTML(paste0("What is the total number of clinics providing ART to ", pop(), " in your country?"))), 
                 300, min=1, step=1)
  })
  output$N <- renderUI({
    numericInput("N", label = h4(HTML(paste0("What is the total number of ", pop(), " receiving ART in your country?"))), 
                 20000, min=1, step=1)
  })
  output$q_DTG <- renderUI({
    numericInput("q_DTG", label = h4(HTML("What is the national percentage of ", pop(), " receiving ART who receive dolutegravir-containing regimens (%)?")), 
                 60, min=0, max=100, step=1)
  })
  
  
  #========================================================================================
  # Check if number of clinics to sample is larger than required minimum number of clinics
  # Option 1 with ICC=0.09
  min_clinics <- reactive({get_min_clinic_threshold(prev_VS_DTG=prev_VS_DTG, prev_VS_O=prev_VS_O,
                                                                       prec_VS_DTG=prec_VS_DTG, prec_VS_O=prec_VS_O,
                                                                       C=input$C, N=input$N, q=q_DTG(), ICC=0.09,
                                                                       DE_info=DE_info, labFail=labFail, alpha=alpha)})
  
  # Option 2 with ICC=0.06
  min_clinics_op2 <- reactive({get_min_clinic_threshold(prev_VS_DTG=prev_VS_DTG, prev_VS_O=prev_VS_O,
                                                                           prec_VS_DTG=prec_VS_DTG, prec_VS_O=prec_VS_O,
                                                                           C=input$C, N=input$N, q=q_DTG(), ICC=0.06,
                                                                           DE_info=DE_info, labFail=labFail, alpha=alpha)})
  
  output$minimum_clinics <- renderUI({

    tagList(
      # a(HTML("Number of clinics to be sampled is too small."), style = "color:red; font-size: 20px"),
      radioButtons("option", label = h4(HTML(paste0("<strong> Can at least ","<span style='color:red'>", 
                                                    min_clinics()[[1]], "</span>",
                                      " clinics be sampled? </strong> This is the minimum number of clinics 
                                      necessary to achieve a total sample size less than or equal to 1500."))),
                   choices = list("Yes (ideal and more conservative option that accounts for a higher level of 
                                  clustering)." = 1, 
                                  "No (less ideal and less conservative option that accounts for a lower level 
                                  of clustering)." = 2), selected = 1),
      conditionalPanel("input.option == 1",
                       numericInput("n_sampled",
                                    label = h4(HTML(paste0("<strong> Input the number of clinics to be sampled. </strong> 
                                                     Minimum number of clinics required is ", "<span style='color:red'>", 
                                                     min_clinics()[[1]], "</span>. Sampling more clinics is preferable from 
                                                     a statistical standpoint."))), min_clinics()[[1]],
                                    min=min_clinics()[[1]], step=1),
      ),
      conditionalPanel("input.option == 2",   # ICC=0.06
                       numericInput("n_sampled_2",
                                    label = h4(HTML(paste0("<strong> Input the number of clinics to be sampled. </strong> 
                                                      Minimum number of clinics required is ", "<span style='color:red'>", 
                                                     min_clinics_op2()[[1]], "</span>. Sampling more clinics is preferable from 
                                                     a statistical standpoint."))), min_clinics_op2()[[1]],
                                    min=min_clinics_op2()[[1]], step=1)
      ),
    )
  })
  
  output$warning <- renderText({
    if (!is.null(input$option)) {
      if (input$option == 1) {
        if (input$n_sampled < min_clinics()[[1]]) {
          paste0("<font color=\"#FF0000\">", "Error: Number of clinics must be at least ",
                 min_clinics()[[1]], "</font>")
        }
      } else if (input$option ==2) {
        if (input$n_sampled_2 < min_clinics_op2()[[1]]) {
          paste0("<font color=\"#FF0000\">", "Error: Number of clinics must be at least ",
                 min_clinics_op2()[[1]], "</font>")
        }
      } 
    }
  })
  
  # ICC based on whether option 1 or 2 was chosen
  ICC <- eventReactive(input$submit, {# Previous ICC <- 0.004278927
    ifelse(input$option == 1, 0.09, 0.06)
  }) 
  
  # Define number of clinics sampled as "c" or "n_sampled"
  n_clinics <- eventReactive(input$submit, {
    ifelse(input$option == 1, input$n_sampled, input$n_sampled_2)
    # ifelse(min_clinics()[[1]] > input$c, input$n_sampled, input$c)
  })
  
  
  #===========================================================================
  # Sample Size Outputs
  
  ### DTG 
  
  # Calculate sample size
  sample_size_DTG_clinic <- eventReactive(input$submit, {calcSampleSize(prev=prev_VS_DTG, CI=prec_VS_DTG, 
                                                                        c=n_clinics(), C=input$C, N=input$N,
                                                                        q=q_DTG(), ICC=ICC(), 
                                                                        DE_info=DE_info, labFail=labFail, alpha=alpha)})
  
  ### Overall calculations
  
  # Calculate sample size
  sample_size_O_clinic <- eventReactive(input$submit, {calcSampleSize(prev=prev_VS_O, CI=prec_VS_O,
                                                                      c=n_clinics(), C=input$C, N=input$N,
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
  
  #=============================================================================
  # Output tables
  # Table of user-specified parameter values
  assumptions_DTG <- eventReactive(input$submit, {
    if(input$population == 1) {
      data.frame(
        Assumptions = c("Expected prevalence of viral suppression for adults receiving dolutegravir-containing regimens",
                        "Desired absolute precision (95% CI half-width)",
                        "Expected prevalence of viral suppression for adults receiving ART (overall)",
                        "Desired absolute precision (95% CI half-width)",
                        "Number of clinics sampled",
                        "Total number of clinics",
                        "Total number of adults receiving ART",
                        "Intracluster correlation coefficient",
                        "Design effect due to imperfect weights",
                        # "Significance Level",
                        "Viral load testing failure rate"),
        Value = as.character(c(paste0(prev_VS_DTG*100, "%"),
                               paste0("\u00B1", prec_VS_DTG*100, "%"),
                               paste0(prev_VS_O*100, "%"),
                               paste0("\u00B1", prec_VS_O*100, "%"),
                               n_clinics(),
                               input$C,
                               input$N,
                               ICC(),
                               DE_info,
                               # alpha,
                               paste0(labFail*100, "%"))),
        stringsAsFactors = FALSE)
    } else {
      data.frame(
      Assumptions = c("Expected prevalence of viral suppression for children and adolescents receiving dolutegravir-containing regimens",
                      "Desired absolute precision (95% CI half-width)",
                      "Expected prevalence of viral suppression for children and adolescents receiving ART (overall)",
                      "Desired absolute precision (95% CI half-width)",
                      "Number of clinics sampled",
                      "Total number of clinics",
                      "Total number of children and adolescents receiving ART",
                      "Intracluster correlation coefficient",
                      "Design effect due to imperfect weights",
                      # "Significance Level",
                      "Viral load testing failure rate"),
      Value = as.character(c(paste0(prev_VS_DTG*100, "%"),
                             paste0("\u00B1", prec_VS_DTG*100, "%"),
                             paste0(prev_VS_O*100, "%"),
                             paste0("\u00B1", prec_VS_O*100, "%"),
                             n_clinics(),
                             input$C,
                             input$N,
                             ICC(),
                             DE_info,
                             # alpha,
                             paste0(labFail*100, "%"))),
      stringsAsFactors = FALSE)
    }
  })
  
  # Render table
  output$values_DTG <- renderTable({assumptions_DTG()})
  
  # Label for assumptions table
  output$table_text <- renderText({
    "Assumptions used to calculate sample sizes."
  })
  
  # Text for box title on DTG sample size
  output$title_ss_DTG <- renderText({
    ifelse(input$population == 1, "Sample size for adults receiving dolutegravir-containing regimens",
           "Sample size for children and adolescents receiving dolutegravir-containing regimens")
  })
  
  output$text_DTG <- renderText({
    ifelse(input$population == 1, 
           "Sample sizes necessary for estimating the prevalence of viral suppression among adults taking dolutegravir-containing regimens.",
           "Sample sizes necessary for estimating the prevalence of viral suppression among children and adolescents taking dolutegravir-containing regimens."
           )
  })
  
  #=================================================================================
  # Non-DTG and total calculations
  
  ## Calculating m_nonDTG and m_total
  sample_size_nonDTG_clinic <- eventReactive(input$submit, {
    ceiling(sample_size_O_clinic()[[1]] * q_nonDTG())
  })
  
  sample_size_nonDTG <- eventReactive(input$submit, {sample_size_nonDTG_clinic()*n_clinics()})
  
  sample_size_total_clinic <- eventReactive(input$submit, {
    ceiling(sample_size_nonDTG_clinic() + sample_size_DTG_clinic_mod())
  })
  
  sample_size_total <- eventReactive(input$submit, {sample_size_total_clinic()*n_clinics()})
  
  
  # Text for box title on non-DTG sample size
  output$title_ss_non <- renderText({
    ifelse(input$population == 1, "Sample size for adults receiving non-dolutegravir-containing regimens",
           "Sample size for children and adolescents receiving non-dolutegravir-containing regimens")
  })
  
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
    ifelse(input$population == 1,
           "Sample sizes necessary from adults taking non-dolutegravir-containing regimens to ensure sufficient sample size for overall estimates.",
           "Sample sizes necessary from children and adolescents taking non-dolutegravir-containing regimens to ensure sufficient sample size for overall estimates."
           )
  })
  
  output$prop_non <- renderText({
    ifelse(input$population == 1, 
           paste0("Percentage of adults receiving non-dolutegravir-containing regimens: ", 
                  a(paste0(q_nonDTG()*100, "%"), style = "color:blue")),
           paste0("Percentage of children and adolescents receiving non-dolutegravir-containing regimens: ", 
                  a(paste0(q_nonDTG()*100, "%"), style = "color:blue"))
           )
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
  
  
  
  ##===========================================================================================================
  ### Server code for precision of ADR estimates for DTG and overall 
  
  # Define variables
  prev_ADR_DTG <- 0.035
  prev_ADR_O <- 0.5
  genoFail <- 0.3
  DE <- 1.5
  
  # Function to calculate the precision using Wald-type intervals using the simple DE=1.5 adjustment
  calc_precision <- function(m, prev, c, q_VNS, DE, labFail, genoFail, alpha) {
    # m: per-clinic sample size for viral suppression estimate
    # prev: expected ADR prevalence
    # c: number of clinics to be sampled
    # q_VNS: expected proportion of individuals on ART with VNS
    
    M_ADR <- ceiling(c*m*(1-labFail)*q_VNS*(1-genoFail))
    
    k_eff_ADR <- M_ADR/DE
    # margin of error uses t-distribution with df = c-2 to account for 2 strata: DTG and non-DTG
    prec <- qt(1-alpha/2, df=c-2)*sqrt(prev*(1-prev)/k_eff_ADR)
    
    return(round(prec, 3))
  }
  
  ### Calculate precision
  prec_ADR_DTG <- eventReactive(input$submit, {calc_precision(m=sample_size_DTG_clinic_mod(), prev=prev_ADR_DTG,
                                                              c=n_clinics(), 
                                                              q_VNS=1-prev_VS_DTG, DE=DE,
                                                              labFail=labFail, genoFail=genoFail, alpha=alpha)})
  prec_ADR_O <- eventReactive(input$submit, {calc_precision(m=sample_size_total_clinic(), prev=prev_ADR_O,
                                                            c=n_clinics(),
                                                            q_VNS=1-prev_VS_O, DE=DE,
                                                            labFail=labFail, genoFail=genoFail, alpha=alpha)})
  
  
      # # Function to calculate the precision using FPC and Wald-type intervals
      # calc_precision <- function(m, prev, c, C, N, q, q_VNS, ICC, DE_info, labFail, genoFail, alpha) {
      #   # m: per-clinic sample size for VS estimate
      #   # q: proportion of the total eligible population that belongs to the subgroup of interest
      #   # q_VNS: proportion of individuals on ART with VNS
      #   
      #   m_ADR <- ceiling(m*(1-labFail)*(1-genoFail)*q_VNS)
      #   M_ADR <- ceiling(N*q*(1-labFail))
      #   
      #   DE_clust <- (1-m_ADR*C/M_ADR) + ((1-c/C)*m_ADR - (1-m_ADR*C/M_ADR))*ICC
      #   k_eff_ADR <- c*m_ADR/(DE_info*DE_clust)
      #   prec <- qt(1-alpha/2, df=c-1)*sqrt(prev*(1-prev)/k_eff_ADR)
      #   
      #   return(round(prec, 3))
      # }
      
      # ### Calculate precision
      # prec_ADR_DTG <- eventReactive(input$submit, {calc_precision(m=sample_size_DTG_clinic_mod(), prev=prev_ADR_DTG, 
      #                                          c=n_clinics(), C=C(), N=N(),
      #                                          q=q_DTG(), q_VNS=1-prev_VS_DTG, ICC=ICC, DE_info=DE_info, 
      #                                          labFail=labFail, genoFail=genoFail, alpha=alpha)})
      # prec_ADR_O <- eventReactive(input$submit, {calc_precision(m=sample_size_total_clinic(), prev=prev_ADR_O, 
      #                                        c=n_clinics(), C=C(), N=N(),
      #                                        q=1, q_VNS=1-prev_VS_O, ICC=ICC, DE_info=DE_info, 
      #                                        labFail=labFail, genoFail=genoFail, alpha=alpha)})
  
  # Render output
  output$prec_ADR_DTG <- renderText({
    ifelse(input$population == 1,
      paste0("Precision for DTG-specific ADR estimate among adults receiving dolutegravir-containing regimens with viral non-suppression: ",
           a(paste0("\u00B1", prec_ADR_DTG()*100, "%"), style = "color:red")),
      paste0("Precision for DTG-specific ADR estimate among children and adolescents receiving dolutegravir-containing regimens with viral non-suppression: ",
             a(paste0("\u00B1", prec_ADR_DTG()*100, "%"), style = "color:red"))
    )
  })
  output$prec_ADR_O <- renderText({
    ifelse(input$population == 1, 
           paste0("Precision for ADR estimate among all adults receiving ART with viral non-suppression: ",
           a(paste0("\u00B1", prec_ADR_O()*100, "%"), style = "color:red")),
           paste0("Precision for ADR estimate among all children and adolescents receiving ART with viral non-suppression: ",
                  a(paste0("\u00B1", prec_ADR_O()*100, "%"), style = "color:red"))
    )
  })
  
  # Table of user-specified parameter values
  assumptions_ADR_DTG <- eventReactive(input$submit, {
    if(input$population == 1) {
      data.frame(
        Assumptions = c("Expected prevalence of dolutegravir-specific ADR for adults taking dolutegravir-containing regimens with viral non-suppression",
                        "Sample size for estimating the prevalence of viral suppression among adults taking dolutegravir-containing regimens",
                        "Viral load testing failure rate",
                        "Expected proportion of adults with viral non-suppression receiving dolutegravir-containing regimens",
                        "Genotyping testing failure rate",
                        "Number of clinics sampled",
                        "Design effect"),
        Value = as.character(c(paste0(prev_ADR_DTG*100, "%"),
                               sample_size_DTG(),
                               paste0(labFail*100, "%"),
                               paste0((1-prev_VS_DTG)*100, "%"),
                               paste0(genoFail*100, "%"),
                               n_clinics(),
                               DE)),
        stringsAsFactors = FALSE)
    } else {
      data.frame(
        Assumptions = c("Expected prevalence of dolutegravir-specific ADR for children and adolescents taking dolutegravir-containing regimens with viral non-suppression",
                        "Sample size for estimating the prevalence of viral suppression among children and adolescents taking dolutegravir-containing regimens",
                        "Viral load testing failure rate",
                        "Expected proportion of children and adolescents with viral non-suppression receiving dolutegravir-containing regimens",
                        "Genotyping testing failure rate",
                        "Number of clinics sampled",
                        "Design effect"),
        Value = as.character(c(paste0(prev_ADR_DTG*100, "%"),
                               sample_size_DTG(),
                               paste0(labFail*100, "%"),
                               paste0((1-prev_VS_DTG)*100, "%"),
                               paste0(genoFail*100, "%"),
                               n_clinics(),
                               DE)),
        stringsAsFactors = FALSE)
    }  
  })
  
  
  # Table of user-specified parameter values
  assumptions_ADR_O <- eventReactive(input$submit, {
    if(input$population == 1) {
      data.frame(
        Assumptions = c("Expected prevalence of any ADR among all adults with viral non-suppression",
                        "Sample size for estimating the prevalence of viral suppression among all adults receiving ART",
                        "Viral load testing failure rate",
                        "Expected percentage of adults receiving ART with viral non-suppression",
                        "Genotyping testing failure rate",
                        "Number of clinics sampled",
                        "Design effect"),
        Value = as.character(c(paste0(prev_ADR_O*100, "%"),
                               sample_size_total(),
                               paste0(labFail*100, "%"),
                               paste0((1-prev_VS_O)*100, "%"),
                               paste0(genoFail*100, "%"),
                               n_clinics(),
                               DE)),
        stringsAsFactors = FALSE)
    } else {
      data.frame(
        Assumptions = c("Expected prevalence of any ADR among all children and adolescents with viral non-suppression",
                        "Sample size for estimating the prevalence of viral suppression among all children and adolescents receiving ART",
                        "Viral load testing failure rate",
                        "Expected percentage of children and adolescents receiving ART with viral non-suppression",
                        "Genotyping testing failure rate",
                        "Number of clinics sampled",
                        "Design effect"),
        Value = as.character(c(paste0(prev_ADR_O*100, "%"),
                               sample_size_total(),
                               paste0(labFail*100, "%"),
                               paste0((1-prev_VS_O)*100, "%"),
                               paste0(genoFail*100, "%"),
                               n_clinics(),
                               DE)),
        stringsAsFactors = FALSE)
    }  
  })
  
  # Render table
  output$values_ADR_DTG <- renderTable({assumptions_ADR_DTG()})
  
  # Render table
  output$values_ADR_O <- renderTable({assumptions_ADR_O()})
  
  
  
}


############################
# Build Shiny app
############################

shinyApp(ui, server)

