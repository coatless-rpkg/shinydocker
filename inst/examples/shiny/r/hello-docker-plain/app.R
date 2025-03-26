library(shiny)

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
    hist(rnorm(input$obs))
  })
}

shinyApp(ui = ui, server = server)
