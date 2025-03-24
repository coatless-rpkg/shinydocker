# Create a simple Shiny app
app_dir <- tempdir()
writeLines(
  'library(shiny)
   
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
   
   shinyApp(ui = ui, server = server)',
  file.path(app_dir, "app.R")
)

# Export the app
shinydocker::export(app_dir, run = TRUE, detached = TRUE)

## Alternatively, steps can be run separately ----

# Create Docker configuration
dockerize(app_dir)

# Build Docker image
build_image(app_dir)

# Run the containerized app
run_container(app_dir, detach = TRUE)
