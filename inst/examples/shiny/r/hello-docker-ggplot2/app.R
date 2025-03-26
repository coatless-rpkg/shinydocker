library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Hello Docker"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", 
                  min = 1, max = 1000, value = 500)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(data.frame(x = rnorm(input$obs))) + aes(x = x) + 
      geom_histogram()
  })
}

shinyApp(ui = ui, server = server)
