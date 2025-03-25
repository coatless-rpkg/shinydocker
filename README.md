
<!-- README.md is generated from README.Rmd. Please edit that file -->

> \[!IMPORTANT\]
>
> This package is currently in the prototype/experimental stage. It is
> not yet available on CRAN and may have bugs or limitations.

# shinydocker <img src="man/figures/shinydocker-animated-logo.svg" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-rpkg/shinydocker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-rpkg/shinydocker/actions/workflows/R-CMD-check.yaml)
![Prototype](https://img.shields.io/badge/Status-Prototype-orange)
![Experimental](https://img.shields.io/badge/Status-Experimental-blue)
<!-- badges: end -->

`shinydocker` is an R package that streamlines the process of
containerizing Shiny applications, supporting both R and Python Shiny
apps.

## Installation

You can install the development version of `shinydocker` from GitHub
with:

``` r
# install.packages("remotes")
remotes::install_github("coatless-rpkg/shinydocker")
```

You will also need to have Docker installed on your system. You can
download Docker from the [official
website](https://www.docker.com/products/docker-desktop). You do not
need to log in to Docker to use `shinydocker`.

## Using shinydocker

The package provides a set of functions to automate the process, as well
as options for advanced configuration.

### Exporting a Shiny App to Docker

The `export()` function allows you to convert a Shiny application into a
docker containerized application.

``` r
# Automated export: configures, builds, and optionally runs the container
export("path/to/your/shinyapp", run = TRUE)
```

For example, to convert the “Hello World” Shiny app from the `{shiny}`
package into a standalone Electron app:

``` r
# Copy "Hello World" from `{shiny}`
system.file("examples", "01_hello", package="shiny") |>
    fs::dir_copy("myapp", overwrite = TRUE)

shinydocker::export("myapp", run = TRUE)
```

### Manually Dockerizing a Shiny App

``` r
library(shinydocker)

# Path to your Shiny app
shiny_app_path <- "path/to/your/shinyapp"

# Create Docker configuration for a Shiny app
dockerize(shiny_app_path)

# Build a Docker image
build_image(shiny_app_path)

# Run the container
run_container(shiny_app_path)
```

### Diagnostic Tools

``` r
# Check Docker environment
sitrep_docker()

# Analyze app containerization readiness
sitrep_app_conversion("path/to/your/shinyapp")
```

## Advanced Configuration

### Custom Dockerfile

``` r
# Use a custom Dockerfile template
dockerize("path/to/your/shinyapp", 
          custom_dockerfile = "path/to/custom/Dockerfile")
```

### Environment Variables

``` r
# Set environment variables during containerization
dockerize("path/to/your/shinyapp", 
          env_vars = c(API_KEY = "your-secret-key"))
```

## Cleanup and Management

``` r
# Stop a specific container
stop_containers("path/to/your/shinyapp")

# Clean up Docker containers and images
cleanup_containers(remove_images = TRUE)
```

## License

AGPL (\>= 3)
