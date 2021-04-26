library(shiny)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Application"),
    
    sidebarPanel(
      selectInput("Distribution", "Please Select Distribution Type",
                  choices = c("Normal", "Exponential", "Lognormal")),
      sliderInput("sampleSize", "Please Select Sample Size",
                  min = 100, max = 5000, value = 1000, step = 100),
      conditionalPanel(condition = "input.Distribution == 'Normal'",
                       textInput("Mean", "Please Select the mean", 10),
                       textInput("SD", "Please Select Standard Deviation", 3)),
      conditionalPanel(condition = "input.Distribution == 'Exponential'",
                       textInput("Lambda", "Please Select Lambda/Rate:",1)),
      conditionalPanel(condition = "input.Distribution == 'Lognormal'",
                       textInput("Meanlog", "Please Select Meanlog:",0),
                       textInput("Meansd", "Please Select Meansd:",1))
    ),
    
    mainPanel(
      plotOutput("myPlot")
    )
  )
  
)

server <- function(input, output, session) {
  
  output$myPlot <- renderPlot({
    distType <- input$Distribution
    size <- input$sampleSize
    
    if(distType == "Normal"){
      Random_Variable <- rnorm(size, mean = as.numeric(input$Mean), sd = as.numeric(input$SD))
    }
    else if (distType == "Exponential"){
      Random_Variable <- rexp(size, rate = 1/as.numeric(input$Lambda))
    }
    else{
        Random_Variable <- rlnorm(size, meanlog = as.numeric(input$Meanlog), sdlog = as.numeric(input$Meansd))
    }
    
    hist(Random_Variable, col = "light blue")
  })
  
}

shinyApp(ui, server)