#' Create a Dockerfile for a Shiny application
#' 
#' @param output_dir Character. Directory where the Dockerfile should be created.
#' @param app_type Character. Either "r" or "python".
#' @param custom_template Character. Path to a custom Dockerfile template.
#' @param port Integer. The port to expose.
#' @param dependencies List. Application dependencies.
#' @param env_vars Named character vector. Environment variables.
#' @param ... Additional arguments.
#' 
#' @return Character. Path to the created Dockerfile.
#' 
#' @keywords internal
create_dockerfile <- function(output_dir, app_type, custom_template = NULL, 
                              port = 3838, dependencies = list(), env_vars = NULL, ...) {
  # Determine which template to use
  if (!is.null(custom_template) && file.exists(custom_template)) {
    template_content <- readLines(custom_template)
    cli::cli_alert_info("Using custom Dockerfile template: {.file {custom_template}}")
  } else {
    # Use built-in template
    template_name <- paste0("Dockerfile_", ifelse(app_type == "r", "R", "Python"))
    template_path <- system.file("templates", template_name, package = "shinydocker")
    
    if (!file.exists(template_path) || nchar(template_path) == 0) {
      cli::cli_abort(c(
        "Could not find template file: {.file {template_name}}",
        "i" = "Please ensure the package is installed correctly."
      ))
    }
    
    cli::cli_alert_info("Using built-in {ifelse(app_type == 'r', 'R', 'Python')} Dockerfile template")
    template_content <- readLines(template_path)
  }
  
  # Process template
  content <- process_dockerfile_template(template_content, app_type, port, dependencies, env_vars)
  
  # Write Dockerfile
  dockerfile_path <- file.path(output_dir, "Dockerfile")
  writeLines(content, dockerfile_path)
  
  return(dockerfile_path)
}

#' Process a Dockerfile template by replacing placeholders
#' 
#' @param template_content Character vector. Template content.
#' @param app_type Character. Either "r" or "python".
#' @param port Integer. The port to expose.
#' @param dependencies List. Application dependencies.
#' @param env_vars Named character vector. Environment variables.
#' 
#' @return Character vector with processed template.
#' 
#' @keywords internal
process_dockerfile_template <- function(template_content, app_type, port, dependencies, env_vars) {
  content <- template_content
  
  # Replace port placeholder
  content <- gsub("\\{\\{PORT\\}\\}", as.character(port), content)
  
  # Replace dependencies placeholder
  content <- gsub("\\{\\{DEPENDENCIES\\}\\}", format_dependencies(app_type, dependencies), content)
  
  # Replace env vars placeholder
  content <- gsub("\\{\\{ENV_VARS\\}\\}", format_env_vars(env_vars), content)
  
  return(content)
}

#' Format dependencies for inclusion in Dockerfile
#' 
#' @param app_type Character. Either "r" or "python".
#' @param dependencies List of dependencies.
#' 
#' @return Character string with formatted dependencies.
#' 
#' @keywords internal
format_dependencies <- function(app_type, dependencies) {
  if (length(dependencies) == 0) {
    return(paste0("# No additional ", ifelse(app_type == "r", "R", "Python"), " packages to install"))
  }
  
  if (app_type == "r") {
    return(paste0('RUN install2.r --error ', paste(dependencies, collapse = " ")))
  } else {
    return(paste0('RUN pip install ', paste(dependencies, collapse = " ")))
  }
}

#' Format environment variables for inclusion in Dockerfile
#' 
#' @param env_vars Named character vector. Environment variables.
#' 
#' @return Character string with formatted environment variables.
#' 
#' @keywords internal
format_env_vars <- function(env_vars) {
  if (is.null(env_vars) || length(env_vars) == 0) {
    return("# No environment variables set")
  }
  
  env_str <- ""
  for (env_name in names(env_vars)) {
    env_str <- paste0(env_str, "ENV ", env_name, "=", env_vars[env_name], "\n")
  }
  
  return(trimws(env_str))
}

#' Create a docker-compose.yml file for a Shiny application
#' 
#' @param output_dir Character. Directory where the file should be created.
#' @param port Integer. The port to expose.
#' @param env_vars Named character vector. Environment variables.
#' @param ... Additional arguments.
#' 
#' @return Character. Path to the created file.
#' 
#' @keywords internal
create_docker_compose <- function(output_dir, port = 3838, env_vars = NULL, ...) {
  # Create docker-compose.yml content
  service_name <- tolower(basename(output_dir))
  service_name <- gsub("[^a-z0-9]", "", service_name)  # Clean name for service
  
  # Basic compose structure
  compose_data <- list(
    version = "3",
    services = list()
  )
  
  # Format port as a list item (required by docker-compose)
  port_mapping <- paste0(port, ":3838")
  
  # Create service configuration
  service_config <- list(
    build = ".",
    ports = list(port_mapping),  # Use a list for ports
    restart = "unless-stopped"
  )
  
  # Add platform configuration if needed (for ARM64 systems)
  app_type <- detect_app_type(output_dir)
  if (is_arm64() && app_type == "r") {
    service_config$platform <- "linux/amd64"
  }
  
  # Add environment variables if provided
  if (!is.null(env_vars) && length(env_vars) > 0) {
    env_list <- as.list(env_vars)
    service_config$environment <- env_list
  }
  
  # Add service to compose data
  compose_data$services[[service_name]] <- service_config
  
  # Convert to YAML
  compose_content <- yaml::as.yaml(compose_data)
  
  # Write docker-compose.yml
  compose_path <- file.path(output_dir, "docker-compose.yml")
  writeLines(compose_content, compose_path)
  
  return(compose_path)
}

#' Create a .dockerignore file
#' 
#' @param output_dir Character. Directory where the file should be created.
#' 
#' @return Character. Path to the created file.
#' 
#' @keywords internal
create_dockerignore <- function(output_dir) {
  # Get the .dockerignore template
  template_path <- system.file("templates", "dockerignore", package = "shinydocker")
  
  if (!file.exists(template_path) || nchar(template_path) == 0) {
    cli::cli_abort(c(
      "Could not find .dockerignore template file",
      "i" = "Please ensure the package is installed correctly."
    ))
  }
  
  # Read template content
  ignore_content <- readLines(template_path)
  
  # Write .dockerignore
  ignore_path <- file.path(output_dir, ".dockerignore")
  writeLines(ignore_content, ignore_path)
  
  return(ignore_path)
}