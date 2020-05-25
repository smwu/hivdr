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
  # tabsetPanel(
  #   tabPanel(title = "DTG-Specific", ),
  #   tabPanel(title = "Overall", )
  # )
  
  # Title
  titlePanel("Sample Size Calculations"),
  
  sidebarLayout(
    
    # Sidebar options to specify sample size calculation
    sidebarPanel(sliderInput("prev", "Prevalence (%)", min=0, max=50, value=5, step=0.5),
                 selectInput("precision", "Select Precision Type", c("Absolute Precision (%)", "Relative Precision (%)")),
                 uiOutput("prec"),
                 sliderInput("N", "Population Size", min=300, max=20000, value=10000, step=100),
                 sliderInput("labFail", "Genotyping Failure (%)", min=0, max=50, value=30, step=5),
                 selectInput("alpha", "Significance Level", choices = c(0.1, 0.05, 0.01), selected=0.05),
                 sliderInput("infl", "Inflation Factor (%)", min=0, max=50, value=10, step=5)),
    
    # # Main panel display consisting of two tables and two plots
    # mainPanel(column(6, fluidRow(tableOutput("values"), plotOutput("plot1"))),
    #           column(6, fluidRow(tableOutput("sampleSize"),plotOutput("plot2")))
    # )
    
    # Main panel display consisting of two tables and two plots
    mainPanel(
      wellPanel(
        fluidRow(column(6, tableOutput("values")),
                         column(6, tableOutput("sampleSize")))
      ),
      br(),
      fluidRow(column(6, plotOutput("plot1")),
                       column(6, plotOutput("plot2")))
    )
    
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
  )
)


#################################
# Define Server code
#################################

server <- function(input, output) {
  
  # User-specified precision type
  output$prec <- renderUI({
    switch(input$precision, 
           "Relative Precision (%)" = sliderInput("prec", "Relative Precision (%)", 
                                                  min=10, max=100, value=30, step=5),
           "Absolute Precision (%)" = sliderInput("prec", "Absolute Precision (%)", 
                                                  min=0.5, max=30, value=3, step=0.25))
  })
  
  # Define reactive variables
  prev <- reactive({input$prev/100})
  prec <- reactive({input$prec/100})
  alpha <- reactive({as.numeric(input$alpha)})
  labFail <- reactive({input$labFail/100})
  infl <- reactive({input$infl/100})
  CI <- reactive({ifelse(input$precision == "Relative Precision (%)",
                         prec()*prev(), prec())})

  # # Calculate sample sizes using FPC and Wald-type intervals
  # sampleSize <- reactive({
  #   nEff <- qnorm(1-alpha()/2)^2*prev()*(1-prev())*input$N / 
  #      (input$N*CI()^2 + (prev()*(1-prev())*qnorm(1-alpha()/2)^2))
  #   n <- nEff/(1-labFail())*(1+infl())
  #   return(ceiling(n))
  # })
  
  # Calculate sample sizes using FPC and Wald-type intervals
  calcSampleSize <- function(alpha, prev, N, CI, labFail, infl) {
    nEff <- qnorm(1-alpha/2)^2*prev*(1-prev)*N / 
      (N*CI^2 + (prev*(1-prev)*qnorm(1-alpha/2)^2))
    n <- nEff/(1-labFail)*(1+infl)
    return(ceiling(n))
  }
  sampleSize <- reactive({calcSampleSize(alpha(), prev(), input$N, CI(), labFail(), infl())})
  
  
  # Table of user-specified parameter values
  sliderValues <- reactive({
    data.frame(
      Parameter = c("Prevalence (%)", 
                    input$precision,
                    "Population Size", 
                    "Laboratory Failure Rate (%)",
                    "Significance Level",
                    "Inflation Factor (%)"),
      Value = as.character(c(input$prev,
                             input$prec,
                             input$N,
                             input$labFail,
                             input$alpha,
                             input$infl)),
      stringsAsFactors = FALSE)
  })
  
  # Table of user-specified sample size and confidence interval
  sampleSizeValues <- reactive({
    data.frame(
      Variable = c("Sample Size", "Confidence Interval Half-Width (%)"),
      Value = as.character(c(sampleSize(), CI()*100)),
      stringsAsFactors = FALSE)
  })
  
  # Render tables
  output$values <- renderTable({sliderValues()})
  output$sampleSize <- renderTable({sampleSizeValues()})
  
  
  # df1 <- reactive({
  #   data.frame(
  #     pop = seq(300, 20000, by=100),
  #     sampSize = calcSampleSize(alpha(), prev(), pop, CI(), labFail(), infl())
  #   )
  # })
  # df2 <- reactive({
  #   prevs = seq(0.05, 0.95, by=0.05)
  #   CI <- ifelse(input$precision == "Relative Precision (%)", prevs*prec(), rep(prec(), length(prevs)))
  #   
  #   data.frame(
  #     prev = seq(0.05, 0.95, by=0.05),
  #     lower = prev - ifelse(input$precision == "Relative Precision (%)", prevs*prec(), rep(prec(), length(prevs))),
  #     upper = prev + ifelse(input$precision == "Relative Precision (%)", prevs*prec(), rep(prec(), length(prevs)))
  #   )
  # })
  # g1 <- reactive({subset(df1(), pop == input$N)})
  # g2 <- reactive({subset(df2(), prev == prev())})
  
  output$plot1 <- renderPlot({
    pop = seq(300, 20000, by=100)
    df1 <- data.frame(
      pop = pop,
      sampSize = calcSampleSize(alpha(), prev(), pop, CI(), labFail(), infl())
    )
    g1 <- data.frame(pop = input$N, sampSize = sampleSize())
    ggplot(data = df1, aes(x = pop, y = sampSize)) +
      xlab("Population Size") + ylab("Sample Size") +
      geom_point() + ggtitle("Plot of Sample Size vs. Population Size") +
      geom_point(data=g1, color="red", size=3) +
      geom_text(x=17500, y=df1$sampSize[1], label=paste0("(", g1$pop,", ", g1$sampSize, ")"),
                color = "red", size=5)

  })
  
  
  # CIs <- reactive({ifelse(input$precision == "Relative Precision (%)", 
  #                        prevs*prec(), rep(prec(),length(prevs)))})
  output$plot2 <- renderPlot({
    prevs <- seq(0.01, 0.5, by=0.01)
    lower <- numeric(length(prevs))
    upper <- numeric(length(prevs))

    for (i in 1:length(prevs)) {
      if (input$precision == "Relative Precision (%)") {
        lower[i] <- prevs[i] - prevs[i]*prec()
        upper[i] <- prevs[i] + prevs[i]*prec()
      } else {
        lower[i] <- prevs[i] - prec()
        upper[i] <- prevs[i] + prec()
      }  
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
      ggtitle("Plot of Prevalence with Confidence Intervals") +
      geom_point(data=g2, color="red") +
      geom_pointrange(data=g2, color="red", ymin = g2$lower, ymax = g2$upper, size=0.7) +
      geom_text(x=0.475, y=df2$prev[1], label=paste0("\u00B1", CI()), color="red", size=5)
  })
  
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
