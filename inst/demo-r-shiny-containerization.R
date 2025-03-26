# Quick demo of containerizing a simple R Shiny app

## Create a simple R Shiny app ----
app_dir <- tempfile()
dir.create(app_dir)
writeLines(
  readLines(system.file("examples", "shiny", "r", "hello-docker-ggplot2", "app.R", package = "shinydocker")),
  file.path(app_dir, "app.R")
)

## Export the app  ----
container <- shinydocker::export(app_dir, run = TRUE, detach = TRUE)

## Stop the container ----
shinydocker::stop_container()

# Alternatively, steps can be run separately ----

## Create Docker configuration ----
dockerize(app_dir)

## Build Docker image ----
build_image(app_dir)

## Run the containerized app ----
run_container(app_dir, detach = TRUE)
