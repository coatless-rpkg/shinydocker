# Quick demo of containerizing a simple Shiny for Python app

## Setup a temporary directory that gets destructed after the session ----
app_dir <- tempfile()
dir.create(app_dir)

## Create a simple Shiny for Python app from a template ----
writeLines(
  readLines(system.file("examples", "shiny", "python", "hello-docker-plain", "app.py", package = "shinydocker")),
  file.path(app_dir, "app.py")
)

## Export the app ----
shinydocker::export(app_dir, run = TRUE, detach = TRUE)

# Stop the container
shinydocker::stop_container(app_dir)

# Restart the container:
shinydocker::run_container(app_dir, detach = TRUE)

## Alternatively, steps can be run separately ----
# Create Docker configuration
# shinydocker::dockerize(app_dir)
# Build Docker image
# shinydocker::build_image(app_dir)
# Run the containerized app
# shinydocker::run_container(app_dir, detach = TRUE)
