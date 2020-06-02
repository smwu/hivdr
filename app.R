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
  
  # Set up navigation bar with multiple pages
  navbarPage("HIVDR Sample Size Calculations",
             
    ### Tab panel for DTG-specific calculations
    tabPanel("DTG-Specific",
             
             titlePanel(title = "Sample Size Calculations for DTG-Specific HIV Drug Resistance"),
             
             sidebarLayout(
               
               # Sidebar options to specify sample size calculation
               sidebarPanel(h4("Assumed Parameter Values"),
                            sliderInput("prev", "Prevalence (%)", min=0, max=30, value=5, step=0.5),
                            selectInput("precision", "Select Precision Type", c("Absolute Precision", "Relative Precision")),
                            uiOutput("prec"),
                            sliderInput("N", "Population Size", min=300, max=20000, value=10000, step=100),
                            selectInput("alpha", "Significance Level", choices = c(0.1, 0.05, 0.01), selected=0.05),
                            sliderInput("labFail", "Genotyping Failure (%)", min=0, max=50, value=30, step=5)),
                            #sliderInput("infl", "Inflation Factor (%)", min=0, max=50, value=10, step=5)),
               
               # Main panel display consisting of two tables and two plots
               mainPanel(
                 wellPanel(
                   fluidRow(splitLayout(cellWidths = c("55%", "45%"),
                                        tableOutput("values"),
                                        tableOutput("sampleSize")))
                 ),
                 br(),
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                      plotOutput("plot1", hover = "plot1_hover"),
                                      plotOutput("plot2", hover = "plot2_hover"))),
                 br(),
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                      verbatimTextOutput("info1"),
                                      verbatimTextOutput("info2"))),
                 br(),        
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                      textOutput("text_plot1"),
                                      textOutput("text_plot2"), 
                                      tags$style(type="text/css", "#text_plot1 {white-space: pre-wrap;}"),
                                      tags$style(type="text/css", "#text_plot2 {white-space: pre-wrap;}")))
               )

             )
    ),
    
    
    ### Tab panel for overall calculations
    tabPanel("Overall (All Comers)",
             
             titlePanel(title = "Sample Size Calculations for Overall HIV Drug Resistance"),
             
             sidebarLayout(
               
               # Sidebar options to specify sample size calculation
               sidebarPanel(h4("Assumed Parameter Values"),
                            sliderInput("prev_O", "Prevalence (%)", min=0, max=50, value=25, step=0.5),
                            selectInput("precision_O", "Select Precision Type", c("Relative Precision", "Absolute Precision")),
                            uiOutput("prec_O"),
                            sliderInput("N_O", "Population Size", min=300, max=20000, value=20000, step=100),
                            selectInput("alpha_O", "Significance Level", choices = c(0.1, 0.05, 0.01), selected=0.05),
                            sliderInput("labFail_O", "Genotyping Failure (%)", min=0, max=50, value=30, step=5)),
                            #sliderInput("infl_O", "Inflation Factor (%)", min=0, max=50, value=5, step=5)),
               
               # Main panel display consisting of two tables and two plots
               mainPanel(
                 wellPanel(
                   fluidRow(splitLayout(cellWidths = c("55%", "45%"),
                                        tableOutput("values_O"),
                                        tableOutput("sampleSize_O")))
                 ),
                 br(),
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                      plotOutput("plot1_O", hover="plot1_O_hover"),
                                      plotOutput("plot2_O", hover="plot2_O_hover"))),
                 br(),
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                      verbatimTextOutput("info1_O"),
                                      verbatimTextOutput("info2_O"))),
                 br(),
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                      textOutput("text_plot1_O"),
                                      textOutput("text_plot2_O"), 
                                      tags$style(type="text/css", "#text_plot1_O {white-space: pre-wrap;}"),
                                      tags$style(type="text/css", "#text_plot2_O {white-space: pre-wrap;}")))
               )
             )
    )
  ),
)


#################################
# Define Server code
#################################

server <- function(input, output) {
  
  # =========================================
  # Server code for DTG-specific calculations
  
  ### Define variables and functions
  # User-specified precision type
  output$prec <- renderUI({
    switch(input$precision, 
           "Relative Precision" = sliderInput("prec", "Relative Precision (%)", 
                                                  min=10, max=100, value=30, step=5),
           "Absolute Precision" = sliderInput("prec", "Absolute Precision (%)", 
                                                  min=0.5, max=30, value=3, step=0.25))
  })
  
  # Define reactive variables
  prev <- reactive({input$prev/100})
  prec <- reactive({input$prec/100})
  alpha <- reactive({as.numeric(input$alpha)})
  labFail <- reactive({input$labFail/100})
  #infl <- reactive({input$infl/100})
  CI <- reactive({ifelse(input$precision == "Relative Precision",
                         prec()*prev(), prec())})
  
  # Function to calculate sample sizes using FPC and Wald-type intervals
  # No inflation factor
  calcSampleSize <- function(alpha, prev, N, CI, labFail) {
    nEff <- qnorm(1-alpha/2)^2*prev*(1-prev)*N / 
      (N*CI^2 + (prev*(1-prev)*qnorm(1-alpha/2)^2))
    n <- nEff/(1-labFail)
    return(ceiling(n))
  }
  
  # Function to calculate confidence interval limits given designed sample size, 
  # using FPC and Wald-type intervals
  calcCI <- function(alpha, prev, n, N, labFail) {
    
    # obtain neff from n
    neff <- n*(1-labFail)
    
    # margin of error
    margin <- qnorm(1-alpha/2)*sqrt((prev*(1-prev)/neff) * (1-(neff/N)))
    
    # return lower and upper confidence bounds
    # return(c(prev - margin, prev + margin, margin))
    return(margin)
  }
  
  # Calculate sample size
  sampleSize <- reactive({calcSampleSize(alpha(), prev(), input$N, CI(), 
                                         labFail())})
  
  
  ### Output tables
  # Table of user-specified parameter values
  sliderValues <- reactive({
    data.frame(
      Parameter = c("Prevalence", 
                    input$precision,
                    "Population Size", 
                    "Laboratory Failure Rate",
                    "Significance Level"),
                    # "Inflation Factor"),
      Value = as.character(c(paste0(input$prev, "%"),
                             paste0(input$prec, "%"),
                             input$N,
                             paste0(input$labFail, "%"),
                             input$alpha)),
                             # paste0(input$infl, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Table of user-specified sample size and confidence interval
  sampleSizeValues <- reactive({
    data.frame(
      Variable = c("Sample Size", "Confidence Interval Width"),
      Value = as.character(c(sampleSize(), paste0("\u00B1",CI()*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render tables
  output$values <- renderTable({sliderValues()})
  output$sampleSize <- renderTable({sampleSizeValues()})
  
  
  ### Output plots
  # Plot of Sample Size vs Population Size
  output$plot1 <- renderPlot({
    pop = seq(300, 20000, by=100)
    df1 <- data.frame(
      pop = pop,
      sampSize = calcSampleSize(alpha(), prev(), pop, CI(), labFail())
    )
    g1 <- data.frame(pop = input$N, sampSize = sampleSize())
    ggplot(data = df1, aes(x = pop, y = sampSize)) +
      xlab("Population Size") + ylab("Sample Size") +
      geom_point() + ggtitle("Plot of Sample Size vs. Population Size") +
      geom_point(data=g1, color="red", size=3) +
      geom_text(x=17500, y=df1$sampSize[1], label=paste0("(", g1$pop,", ", g1$sampSize, ")"),
                color = "red", size=5) + 
      theme(axis.text=element_text(size=12))
    
  })
  
  # Plot of Estimated Prevalence with Confidence Bands
  output$plot2 <- renderPlot({
    prevs <- seq(0.01, 0.3, by=0.01)
    lower <- numeric(length(prevs))
    upper <- numeric(length(prevs))
    
    for (i in 1:length(prevs)) {
      # if (input$precision == "Relative Precision") {
      #   lower[i] <- prevs[i] - prevs[i]*prec()
      #   upper[i] <- prevs[i] + prevs[i]*prec()
      # } else {
      #   lower[i] <- prevs[i] - prec()
      #   upper[i] <- prevs[i] + prec()
      # }  
      margin <- calcCI(alpha(), prevs[i], sampleSize(), input$N, labFail())
      lower[i] <- prevs[i] - margin
      upper[i] <- prevs[i] + margin
    }
    df2 <- data.frame(
      prev = prevs,
      lower = lower,
      upper = upper
    )
    
    g2 <- data.frame(prev=prev(), lower=prev() - CI(), upper = prev() + CI())
    ggplot(data = df2, aes(x = prev, y = prev)) + geom_point() + 
      ylim(df2$prev[1] - CI(), df2$prev[length(prevs)] + CI()) +
      geom_pointrange(data = df2, ymin = df2$lower, ymax = df2$upper) +
      xlab("Prevalence") + ylab("Prevalence") +
      ggtitle("Plot of Confidence Interval for Prevalence") +
      geom_point(data=g2, color="red") +
      geom_pointrange(data=g2, color="red", ymin = g2$lower, ymax = g2$upper, size=0.7) +
      geom_text(x=0.275, y=df2$prev[1]-CI(), label=paste0("\u00B1", CI()), color="red", size=5) + 
      theme(axis.text=element_text(size=12))
  })
  
  # Text output for plot captions
  output$text_plot1 <- renderText({"This plot displays the effect of the finite population correction on the required sample size. A smaller total population size will result in a smaller required sample size. The red point marks the assumed population size and the corresponding required sample size."})
  output$text_plot2 <- renderText({"This plot displays how the confidence interval width will change if the calculated sample size is used but the actual prevalence differs from what is assumed. The red point corresponds to the assumed prevalence used to calculate the sample size."})
  output$info1 <- renderText({
    paste0("x=", round(as.numeric(input$plot1_hover$x),0), ", y=", round(as.numeric(input$plot1_hover$y),0))
  })
  output$info2 <- renderText({
    paste0("x=", round(as.numeric(input$plot2_hover$x),3), ", y=", round(as.numeric(input$plot2_hover$y),3))
  })

  # ====================================
  # Server code for overall calculations
  
  ### Define variables and functions
  # User-specified precision type
  output$prec_O <- renderUI({
    switch(input$precision_O, 
           "Relative Precision" = sliderInput("prec_O", "Relative Precision (%)", 
                                              min=10, max=100, value=35, step=5),
           "Absolute Precision" = sliderInput("prec_O", "Absolute Precision (%)", 
                                              min=0.5, max=30, value=3, step=0.25))
  })
  
  # Define reactive variables
  prev_O <- reactive({input$prev_O/100})
  prec_O <- reactive({input$prec_O/100})
  alpha_O <- reactive({as.numeric(input$alpha_O)})
  labFail_O <- reactive({input$labFail_O/100})
  # infl_O <- reactive({input$infl_O/100})
  CI_O <- reactive({ifelse(input$precision_O == "Relative Precision",
                           prec_O()*prev_O(), prec_O())})
  
  # Calculate sample sizes using FPC and Wald-type intervals
  sampleSize_O <- reactive({calcSampleSize(alpha_O(), prev_O(), input$N_O, 
                                           CI_O(), labFail_O())})
  
  ### Output tables
  # Table of user-specified parameter values
  sliderValues_O <- reactive({
    data.frame(
      Parameter = c("Prevalence", 
                    input$precision_O,
                    "Population Size", 
                    "Laboratory Failure Rate",
                    "Significance Level"),
                   #  "Inflation Factor"),
      Value = as.character(c(paste0(input$prev_O, "%"),
                             paste0(input$prec_O, "%"),
                             input$N_O,
                             paste0(input$labFail_O, "%"),
                             input$alpha_O)),
                             #paste0(input$infl_O, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Table of user-specified sample size and confidence interval
  sampleSizeValues_O <- reactive({
    data.frame(
      Variable = c("Sample Size", "Confidence Interval Width"),
      Value = as.character(c(sampleSize_O(), paste0("\u00B1",CI_O()*100, "%"))),
      stringsAsFactors = FALSE)
  })
  
  # Render tables
  output$values_O <- renderTable({sliderValues_O()})
  output$sampleSize_O <- renderTable({sampleSizeValues_O()})
  
  
  ### Output plots
  # Plot of Sample Size vs Population Size
  output$plot1_O <- renderPlot({
    pop = seq(300, 20000, by=100)
    df1 <- data.frame(
      pop = pop,
      sampSize = calcSampleSize(alpha_O(), prev_O(), pop, CI_O(), labFail_O())
    )
    g1 <- data.frame(pop = input$N_O, sampSize = sampleSize_O())
    ggplot(data = df1, aes(x = pop, y = sampSize)) +
      xlab("Population Size") + ylab("Sample Size") +
      geom_point() + ggtitle("Plot of Sample Size vs. Population Size") +
      geom_point(data=g1, color="red", size=3) +
      geom_text(x=17500, y=df1$sampSize[1], label=paste0("(", g1$pop,", ", g1$sampSize, ")"),
                color = "red", size=5) + 
      theme(axis.text=element_text(size=12))
    
  })
  
  # Plot of Estimated Prevalence with Confidence Bands
  output$plot2_O <- renderPlot({
    prevs <- seq(0.01, 0.5, by=0.01)
    lower <- numeric(length(prevs))
    upper <- numeric(length(prevs))
    
    for (i in 1:length(prevs)) {
      # if (input$precision_O == "Relative Precision") {
      #   lower[i] <- prevs[i] - prevs[i]*prec_O()
      #   upper[i] <- prevs[i] + prevs[i]*prec_O()
      # } else {
      #   lower[i] <- prevs[i] - prec_O()
      #   upper[i] <- prevs[i] + prec_O()
      # }  
      margin <- calcCI(alpha_O(), prevs[i], sampleSize_O(), input$N_O, labFail_O())
      lower[i] <- prevs[i] - margin
      upper[i] <- prevs[i] + margin
    }
    df2 <- data.frame(
      prev = prevs,
      lower = lower,
      upper = upper
    )
    
    g2 <- data.frame(prev=prev_O(), lower=prev_O() - CI_O(), upper = prev_O() + CI_O())
    ggplot(data = df2, aes(x = prev, y = prev)) + geom_point() + 
      ylim(df2$prev[1] - CI_O(), df2$prev[length(prevs)] + CI_O()) +
      geom_pointrange(data = df2, ymin = df2$lower, ymax = df2$upper) +
      xlab("Prevalence") + ylab("Prevalence") +
      ggtitle("Plot of Confidence Interval for Prevalence") +
      geom_point(data=g2, color="red") +
      geom_pointrange(data=g2, color="red", ymin = g2$lower, ymax = g2$upper, size=0.7) +
      geom_text(x=0.475, y=df2$prev[1] - CI_O(), label=paste0("\u00B1", CI_O()), color="red", size=5) + 
      theme(axis.text=element_text(size=12))
  })

  # Text output for plot captions
  output$text_plot1_O <- renderText({"This plot displays the effect of the finite population correction on the required sample size. A smaller total population size will result in a smaller required sample size. The red point marks the assumed population size and the corresponding required sample size."})
  output$text_plot2_O <- renderText({"This plot displays how the confidence interval width will change if the calculated sample size is used but the actual prevalence differs from what is assumed. The red point corresponds to the assumed prevalence used to calculate the sample size."})
  output$info1_O <- renderText({
    paste0("x=", round(as.numeric(input$plot1_O_hover$x),0), ", y=", round(as.numeric(input$plot1_O_hover$y),0))
  })
  output$info2_O <- renderText({
    paste0("x=", round(as.numeric(input$plot2_O_hover$x),3), ", y=", round(as.numeric(input$plot2_O_hover$y),3))
  })
    
}


############################
# Build Shiny app
############################

shinyApp(ui, server)


############################
# Old discarded code
############################

# textInput("name", "Enter your name", ""),
# radioButtons("gender", "Select gender", list("Male", "Female"), "")),
# sliderInput("relPrec", "Relative Precision (%)", min=10, max=100, value=50, step=5),

# ("Sample Size"), 
# textOutput("myprev"),
# textOutput("myalpha"),
# textOutput("myname"), 
# textOutput("mygender"),

# output$myprev <- renderText(paste0("Prevalence is: ", input$prev))
# output$myalpha <- renderText(paste0("Significance level is: ", input$alpha))
# output$myrelPrec <- renderText(input$relPrec)
# output$myname <- renderText(input$name)
# output$mygender <- renderText(input$gender)
# relPrec <- reactive({input$relPrec}/100)

# choicePrec <- reactive({
#   switch(input$precision, 
#          "Relative Precision" = sliderInput("prec", "Relative Precision (%)", 
#                                             min=10, max=100, value=50, step=5),
#          "Absolute Precision" = sliderInput("prec", "Absolute Precision (%)", 
#                                             min=0.5, max=30, value=1.75, step=0.25))
# })

# sampleSize <- reactive({
#     nEff <- qnorm(1-alpha()/2)^2*prev()*(1-prev())*input$N / 
#     (input$N*(relPrec()*prev())^2 + (prev()*(1-prev())*qnorm(1-alpha()/2)^2))
#   n <- nEff/(1-labFail())*(1+infl())
#   return(ceiling(n))
# })



# plotSampleSize <- ggplot(data = df(), aes(x = pop, y = sampSize)) +
#                   xlab("Population Size") + ylab("Sample Size") +
#                   geom_point() + ggtitle("Plot of Sample Size vs. Population Size") +
#                   geom_point(data=g1(), color="red") +
#                   geom_text(data=g1(), label=paste0("(", g1()[1],", ", g1()[2], ")"), vjust=1)

# # Plot of prevalence and confidence intervals
# plotPrev <- ggplot(data = df, aes(x = prev, y = prev)) + geom_point() + geom_errorbar() +
#             xlab("Prevalence") + ylab("Prevalence") +
#             ggtitle("Plot of Prevalence with Confidence Intervals") +
#             geom_point(data=g2, color="red") + geom_errorbar(data=g2, color="red") +
#             geom_text(data=g2, label=paste0("\u00B1", CI())) + ### may need reactive here


# # Plot of Sample Size vs. Population Size
# plotSampleSize <- reactive({
#   x <- seq(300, 20000, by=100)
#   y <- calcSampleSize(alpha(), prev(), x, CI(), labFail(), infl())
#   
#   # y <- ceiling((qnorm(1-alpha()/2)^2*prev()*(1-prev())*x / 
#   #                 (x*(CI())^2 + (prev()*(1-prev())*qnorm(1-alpha()/2)^2)))/
#   #                (1-labFail())*(1+infl()))
#   
#   plot(x, y, xlab="Population Size", ylab="Sample Size", cex.lab=1.2,
#        main="Plot of Sample Size vs. Population Size", cex=0.9)
#   points(input$N, y[x==input$N], col="red", type="p", pch=16, cex=1.3)
#   legend("right", legend=paste0("(",as.character(input$N),", ",
#                                 as.character(round(y[x==input$N],0)), ")"), col="red", pch=16)
# })


# Render plots
# output$plot1 <- renderPlot({plotSampleSize})
# output$plot2 <- renderPlot({plotPrev})


# # Main panel display consisting of two tables and two plots
# mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%"),
#                                tableOutput("values"),
#                                tableOutput("sampleSize"))),
#           # plotOutput("plot1"),
#           # plotOutput("plot2")
#           fluidRow(splitLayout(cellWidths = c("50%", "50%"),
#                                plotOutput("plot1"),
#                                plotOutput("plot2")))
# )

# # Main panel display consisting of two tables and two plots
# mainPanel(
#   wellPanel(
#     fluidRow(column(6, tableOutput("values")),
#              column(6, tableOutput("sampleSize")))
#   ),
#   br(),
#   fluidRow(column(6, plotOutput("plot1")),
#            column(6, plotOutput("plot2")))
# )


# # Function to calculate sample sizes using FPC and Wald-type intervals
# calcSampleSize <- function(alpha, prev, N, CI, labFail, infl) {
#   nEff <- qnorm(1-alpha/2)^2*prev*(1-prev)*N / 
#     (N*CI^2 + (prev*(1-prev)*qnorm(1-alpha/2)^2))
#   n <- nEff/(1-labFail)*(1+infl)
#   return(ceiling(n))
# }