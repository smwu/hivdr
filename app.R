#=============================================#
# Shiny App for HIVDR Sample Size Calculations
# 
# Results displayed at 
# https://smwu.shinyapps.io/shinyapp/
#=============================================#

# Load required packages
library(shiny)
library(gridExtra)

##########################
# Define UI code
##########################

ui <- fluidPage(
  
  # Title
  titlePanel("Sample Size Calculations"),
  
  sidebarLayout(
    
    # Sidebar options to specify sample size calculation
    sidebarPanel(sliderInput("prev", "Prevalence (%)", min=0, max=100, value=5, step=0.5),
                 selectInput("precision", "Select Precision Type", c("Absolute Precision (%)", "Relative Precision (%)")),
                 uiOutput("prec"),
                 sliderInput("N", "Population Size", min=300, max=20000, value=10000, step=100),
                 sliderInput("labFail", "Genotyping Failure (%)", min=0, max=50, value=30, step=5),
                 selectInput("alpha", "Significance Level", choices = c(0.1, 0.05, 0.01), selected=0.05),
                 sliderInput("infl", "Inflation Factor (%)", min=0, max=50, value=10, step=5)),
                 
                 # textInput("name", "Enter your name", ""),
                 # radioButtons("gender", "Select gender", list("Male", "Female"), "")),
                 # sliderInput("relPrec", "Relative Precision (%)", min=10, max=100, value=50, step=5),
    
    # Main panel display consisting of two tables and a plot
    mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                   tableOutput("values"), 
                                   tableOutput("sampleSize"))),
              plotOutput("myplot"))
    
              # ("Sample Size"), 
              # textOutput("myprev"),
              # textOutput("myalpha"),
              # textOutput("myname"), 
              # textOutput("mygender"),
  )
)


#################################
# Define Server code
#################################

server <- function(input, output) {
  
  # Define reactive variables
  prev <- reactive({input$prev/100})
  prec <- reactive({input$prec/100})
  alpha <- reactive({as.numeric(input$alpha)})
  labFail <- reactive({input$labFail/100})
  infl <- reactive({input$infl/100})
  CI <- reactive({ifelse(input$precision == "Relative Precision (%)",
                         prec()*prev(), prec())})
  
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
  
  # User-specified precision type
  output$prec <- renderUI({
    switch(input$precision, 
           "Relative Precision (%)" = sliderInput("prec", "Relative Precision (%)", 
                                              min=10, max=100, value=30, step=5),
           "Absolute Precision (%)" = sliderInput("prec", "Absolute Precision (%)", 
                                              min=0.5, max=30, value=3, step=0.25))
  })
  
  # Calculate sample sizes using FPC and Wald-type intervals
  sampleSize <- reactive({
    nEff <- qnorm(1-alpha()/2)^2*prev()*(1-prev())*input$N / 
       (input$N*CI()^2 + (prev()*(1-prev())*qnorm(1-alpha()/2)^2))
    n <- nEff/(1-labFail())*(1+infl())
    return(ceiling(n))
  })
  
  # sampleSize <- reactive({
  #     nEff <- qnorm(1-alpha()/2)^2*prev()*(1-prev())*input$N / 
  #     (input$N*(relPrec()*prev())^2 + (prev()*(1-prev())*qnorm(1-alpha()/2)^2))
  #   n <- nEff/(1-labFail())*(1+infl())
  #   return(ceiling(n))
  # })
  
  # Plot of Sample Size vs. Population Size
  output$myplot <- renderPlot({
    x <- seq(300, 20000, by=100)
    y <- ceiling((qnorm(1-alpha()/2)^2*prev()*(1-prev())*x / 
            (x*(CI())^2 + (prev()*(1-prev())*qnorm(1-alpha()/2)^2)))/
      (1-labFail())*(1+infl()))
    plot(x, y, xlab="Population Size", ylab="Sample Size", cex.lab=1.2,
         main="Plot of Sample Size vs. Population Size", cex=0.9)
    points(input$N, y[x==input$N], col="red", type="p", pch=16, cex=1.3)
    legend("right", legend=paste0("(",as.character(input$N),", ",
                       as.character(round(y[x==input$N],0)), ")"), col="red", pch=16)
  })
  
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
  
  # Render 'sliderValues' into a table
  output$values <- renderTable({
    sliderValues()
  })
  
  # Render 'sampleSizeValues' into a table
  output$sampleSize <- renderTable({
    sampleSizeValues()
  })
}


############################
# Build Shiny app
############################

shinyApp(ui, server)

