#' Create Docker configuration for a Shiny application
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' @param output_dir Character. Path where Docker configuration should be created.
#'   If NULL, files will be created in app_dir.
#' @param app_type Character. Either "r" or "python". If NULL, it will be auto-detected.
#' @param port Integer. The port to expose for the Shiny application. Default: 3838.
#' @param dependencies Logical. Whether to automatically detect and include
#'   dependencies. Default: TRUE.
#' @param custom_dockerfile Character. Path to a custom Dockerfile template to use.
#'   If NULL, the package's built-in templates will be used.
#' @param env_vars Named character vector. Environment variables to include in
#'   the Docker configuration.
#' @param ... Additional arguments passed to internal functions.
#' 
#' @return Invisibly returns the path to the created Docker configuration.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Basic usage with an R Shiny app
#' dockerize("path/to/my/shinyapp")
#' 
#' # For a Python Shiny app
#' dockerize("path/to/my/python/shinyapp", app_type = "python")
#' 
#' # With custom port and environment variables
#' dockerize("path/to/my/shinyapp", port = 8080,
#'           env_vars = c(API_KEY = "your-secret-key"))
#' }
dockerize <- function(app_dir, output_dir = NULL, app_type = NULL, port = 3838,
                      dependencies = TRUE, custom_dockerfile = NULL,
                      env_vars = NULL, ...) {
  # Validate inputs
  check_app_directory(app_dir)
  
  if (is.null(output_dir)) {
    output_dir <- app_dir
  } else {
    output_dir <- fs::path_abs(output_dir)
    fs::dir_create(output_dir, recurse = TRUE)
  }
  
  # Detect app type if not specified
  if (is.null(app_type)) {
    app_type <- detect_app_type(app_dir)
  } else {
    app_type <- tolower(app_type)
    if (!app_type %in% c("r", "python")) {
      cli::cli_abort("App type must be either {.val r} or {.val python}")
    }
  }
  
  # Detect dependencies if required
  if (dependencies) {
    cli::cli_alert_info("Detecting {ifelse(app_type == 'r', 'R', 'Python')} package dependencies...")
    deps <- detect_dependencies(app_dir, app_type)
    if (length(deps) > 0) {
      cli::cli_alert_success("Found {length(deps)} dependencies")
    } else {
      cli::cli_alert_info("No package dependencies detected")
    }
  } else {
    deps <- list()
  }
  
  # Create Dockerfile
  dockerfile_path <- create_dockerfile(
    output_dir, 
    app_type = app_type,
    custom_template = custom_dockerfile,
    port = port,
    dependencies = deps,
    env_vars = env_vars,
    ...
  )
  
  # Create docker-compose.yml
  compose_path <- create_docker_compose(
    output_dir,
    port = port,
    env_vars = env_vars,
    ...
  )
  
  # Create .dockerignore
  ignore_path <- create_dockerignore(output_dir)
  
  cli::cli_alert_success("Docker configuration created successfully")
  cli::cli_bullets(c(
    " " = "Output directory: {.file {output_dir}}",
    " " = "Dockerfile: {.file {dockerfile_path}}",
    " " = "docker-compose.yml: {.file {compose_path}}",
    " " = ".dockerignore: {.file {ignore_path}}"
  ))
  
  invisible(output_dir)
}

#' Export a Shiny app to a Docker container
#' 
#' This function takes a directory containing a Shiny application (either R or Python) 
#' and exports it to an appropriate Docker container. It handles the entire process 
#' including creating Docker configuration, building the Docker image, and optionally 
#' running the container.
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' @param output_dir Character. Path where Docker configuration should be created.
#'   If NULL, files will be created in app_dir.
#' @param tag Character. The tag for the Docker image. If NULL, a tag will be generated
#'   from the directory name.
#' @param port Integer. The port to expose for the Shiny application. Default: 3838.
#' @param env_vars Named character vector. Environment variables to include in
#'   the Docker configuration.
#' @param run Logical. Whether to run the container after building. Default: FALSE.
#' @param detach Logical. If run=TRUE, whether to run in detached mode. Default: TRUE.
#' @param quiet Logical. If TRUE, suppress Docker command output. Default: FALSE.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return Invisibly returns a list with the paths to the created Docker files and
#'   container information if run=TRUE.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Basic usage
#' export("path/to/my/shinyapp")
#' 
#' # Export and run
#' export("path/to/my/shinyapp", run = TRUE)
#' 
#' # Custom configuration
#' export("path/to/my/shinyapp", 
#'        tag = "myorg/myapp:latest",
#'        port = 8080,
#'        env_vars = c(API_KEY = "secret-key"),
#'        run = TRUE)
#' }
export <- function(app_dir, output_dir = NULL, tag = NULL, port = 3838,
                   env_vars = NULL, run = FALSE, detach = TRUE, quiet = FALSE, ...) {
  # Validate app_dir exists
  check_app_directory(app_dir)
  
  # Validate app has required files
  check_shiny_app_files(app_dir)
  
  # Step 1: Determine app type
  app_type <- detect_app_type(app_dir)
  
  # Report detected app type
  cli::cli_alert_info("Detected {.strong {ifelse(app_type == 'r', 'R', 'Python')}} Shiny application")
  
  # Step 2: Create Docker configuration
  cli::cli_alert_info("Creating Docker configuration...")
  docker_dir <- tryCatch({
    dockerize(
      app_dir = app_dir,
      output_dir = output_dir,
      app_type = app_type,
      port = port,
      env_vars = env_vars,
      ...
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to create Docker configuration",
      "x" = "{e$message}"
    ))
  })
  
  # Step 3: Build Docker image
  cli::cli_alert_info("Building Docker image...")
  image_tag <- tryCatch({
    build_image(
      app_dir = docker_dir,
      tag = tag,
      quiet = quiet,
      ...
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to build Docker image",
      "x" = "{e$message}"
    ))
  })
  
  # Step 4: Run container if requested
  container_id <- NULL
  if (run) {
    cli::cli_alert_info("Running Docker container...")
    container_id <- tryCatch({
      run_container(
        app_dir = docker_dir,
        tag = image_tag,
        port = port,
        detach = detach,
        env_vars = env_vars,
        quiet = quiet,
        ...
      )
    }, error = function(e) {
      cli::cli_alert_warning(c(
        "Failed to run Docker container",
        "x" = "{e$message}"
      ))
      return(NULL)
    })
  } else {
    cli::cli_alert_info("To run the container, use:")
    cli::cli_code(glue::glue("run_container('{docker_dir}', port = {port}, detach = TRUE)"))
  }
  
  # Return information about export
  invisible(list(
    app_type = app_type,
    docker_dir = docker_dir,
    image_tag = image_tag,
    container_id = container_id
  ))
}

#' Clean up Docker containers and images created by shinydocker
#' 
#' This function removes Docker containers and optionally images created by the
#' shinydocker package. It can target a specific application or clean up all
#' shinydocker-created containers and images.
#' 
#' @param app_dir Character. Path to the Shiny application directory. If provided,
#'   only containers/images related to this application will be removed.
#'   If NULL, all containers/images created by shinydocker will be considered.
#' @param remove_images Logical. If TRUE, also remove Docker images after removing containers.
#'   Default: FALSE.
#' @param include_running Logical. If TRUE, stop and remove running containers.
#'   If FALSE, only stopped containers will be removed. Default: FALSE.
#' @param force Logical. If TRUE, force removal without confirmation. Default: FALSE.
#' @param prune Logical. If TRUE, run docker system prune after cleanup to remove unused data.
#'   Default: FALSE.
#' @param quiet Logical. If TRUE, suppress Docker command output. Default: FALSE.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Invisibly returns a list with counts of removed containers and images.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Clean up containers for a specific app
#' cleanup_container("path/to/my/shinyapp")
#' 
#' # Clean up all shinydocker containers and images
#' cleanup_container(remove_images = TRUE)
#' 
#' # Force cleanup of all containers including running ones
#' cleanup_container(include_running = TRUE, force = TRUE)
#' }
cleanup_container <- function(app_dir = NULL, remove_images = FALSE, 
                               include_running = FALSE, force = FALSE, prune = FALSE,
                               quiet = FALSE, ...) {
  # Check if Docker is available
  check_docker_available()
  
  # Initialize counters
  removed_containers <- 0
  removed_images <- 0
  
  # If app_dir is provided, validate and convert to absolute path
  if (!is.null(app_dir)) {
    check_app_directory(app_dir)
    
    # Generate image name based on app directory
    image_filter <- paste0("ancestor=shiny-", tolower(fs::path_file(app_dir)))
    cli::cli_alert_info("Targeting containers for app: {.file {app_dir}}")
  } else {
    # Filter for all shinydocker images - they all start with "shiny-"
    image_filter <- "ancestor=shiny-"
    cli::cli_alert_info("Targeting all shinydocker containers")
  }
  
  # Find all containers (running or not based on include_running)
  status_args <- if(include_running) c("ps", "-a") else c("ps", "--filter", "status=exited")
  
  # Find containers
  find_result <- execute_docker_command(
    "docker", 
    c(status_args, "--filter", image_filter, "--format", "{{.ID}} {{.Status}}"),
    quiet = TRUE,
    error_on_status = FALSE,
    ...
  )
  
  # Parse container data
  container_data <- trimws(find_result$stdout)
  if (container_data == "") {
    cli::cli_alert_info("No matching containers found to clean up")
    container_lines <- character(0)
  } else {
    container_lines <- strsplit(container_data, "\n")[[1]]
  }
  
  # Process found containers
  if (length(container_lines) > 0) {
    # Process and remove containers
    removed_containers <- process_containers(container_lines, include_running, force, quiet, ...)
  }
  
  # Find and remove images if requested
  if (remove_images) {
    removed_images <- remove_shinydocker_images(app_dir, force, quiet, ...)
  }
  
  # Run docker system prune if requested
  if (prune) {
    run_docker_prune(force, quiet, ...)
  }
  
  # Summary
  cli::cli_alert_success("Cleanup complete: removed {removed_containers} container(s) and {removed_images} image(s)")
  
  invisible(list(containers = removed_containers, images = removed_images))
}

#' Process and remove containers
#' 
#' @param container_lines Character vector. Container data lines.
#' @param include_running Logical. Whether to include running containers.
#' @param force Logical. Whether to force removal.
#' @param quiet Logical. Whether to suppress output.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Integer. Number of removed containers.
#' 
#' @keywords internal
process_containers <- function(container_lines, include_running, force, quiet, ...) {
  # Count running vs stopped containers
  running_containers <- grep("Up ", container_lines, value = TRUE)
  stopped_containers <- grep("Exited ", container_lines, value = TRUE)
  running_count <- length(running_containers)
  stopped_count <- length(stopped_containers)
  
  # Get container IDs
  all_container_ids <- sapply(strsplit(container_lines, " "), function(x) x[1])
  running_container_ids <- if (length(running_containers) > 0) {
    sapply(strsplit(running_containers, " "), function(x) x[1])
  } else {
    character(0)
  }
  stopped_container_ids <- if (length(stopped_containers) > 0) {
    sapply(strsplit(stopped_containers, " "), function(x) x[1])
  } else {
    character(0)
  }
  
  # Show summary of what was found
  cli::cli_alert_info("Found {length(container_lines)} container(s): {running_count} running, {stopped_count} stopped")
  
  # For non-forced operations, ask for confirmation
  if (!force) {
    message <- "Do you want to proceed with cleanup?"
    
    if (include_running && running_count > 0) {
      message <- paste0(message, "\nWARNING: This will stop and remove ", running_count, " running container(s)!")
    }
    
    if (!utils::menu(c("Yes", "No"), title = message) == 1) {
      cli::cli_alert_info("Cleanup cancelled by user")
      return(0)
    }
  }
  
  # Stop running containers if included
  if (include_running && running_count > 0) {
    stop_running_containers(running_container_ids, quiet, ...)
  }
  
  # Remove containers
  removed_count <- 0
  target_container_ids <- if (include_running) all_container_ids else stopped_container_ids
  
  if (length(target_container_ids) > 0) {
    cli::cli_alert_info("Removing {length(target_container_ids)} container(s)...")
    
    for (container_id in target_container_ids) {
      rm_result <- execute_docker_command(
        "docker", 
        c("rm", container_id),
        quiet = quiet,
        echo = !quiet,
        error_on_status = FALSE,
        ...
      )
      
      if (rm_result$status == 0) {
        removed_count <- removed_count + 1
        if (!quiet) {
          cli::cli_alert_success("Removed container {.val {container_id}}")
        }
      } else {
        cli::cli_alert_warning("Failed to remove container {.val {container_id}}")
      }
    }
  }
  
  return(removed_count)
}

#' Stop running containers
#' 
#' @param container_ids Character vector. Container IDs to stop.
#' @param quiet Logical. Whether to suppress output.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Invisibly returns TRUE.
#' 
#' @keywords internal
stop_running_containers <- function(container_ids, quiet, ...) {
  cli::cli_alert_info("Stopping {length(container_ids)} running container(s)...")
  
  for (container_id in container_ids) {
    stop_result <- execute_docker_command(
      "docker", 
      c("stop", container_id),
      quiet = quiet,
      echo = !quiet,
      error_on_status = FALSE,
      ...
    )
    
    if (stop_result$status != 0) {
      cli::cli_alert_warning("Failed to stop container {.val {container_id}}")
    } else if (!quiet) {
      cli::cli_alert_success("Stopped container {.val {container_id}}")
    }
  }
  
  invisible(TRUE)
}

#' Remove shinydocker images
#' 
#' @param app_dir Character. Path to the application directory. If NULL, all shinydocker images.
#' @param force Logical. Whether to force removal.
#' @param quiet Logical. Whether to suppress output.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Integer. Number of removed images.
#' 
#' @keywords internal
remove_shinydocker_images <- function(app_dir, force, quiet, ...) {
  # Adjust image filter for listing images
  if (!is.null(app_dir)) {
    image_name <- paste0("shiny-", tolower(fs::path_file(app_dir)))
  } else {
    image_name <- "shiny-"
  }
  
  # List images
  find_images_result <- execute_docker_command(
    "docker", 
    c("images", "--format", "{{.Repository}}:{{.Tag}} {{.ID}}", image_name),
    quiet = TRUE,
    error_on_status = FALSE,
    ...
  )
  
  image_data <- trimws(find_images_result$stdout)
  if (image_data == "") {
    cli::cli_alert_info("No matching images found to remove")
    return(0)
  }
  
  image_lines <- strsplit(image_data, "\n")[[1]]
  cli::cli_alert_info("Found {length(image_lines)} image(s) to remove")
  
  # For non-forced image removal, ask for confirmation
  if (!force) {
    if (!utils::menu(c("Yes", "No"), title = "Do you want to remove these images?") == 1) {
      cli::cli_alert_info("Image removal cancelled by user")
      return(0)
    }
  }
  
  # Remove images
  removed_count <- 0
  for (image_line in image_lines) {
    parts <- strsplit(image_line, " ")[[1]]
    image_id <- parts[length(parts)]  # Last part is the ID
    
    rmi_result <- execute_docker_command(
      "docker", 
      c("rmi", image_id),
      quiet = quiet,
      echo = !quiet,
      error_on_status = FALSE,
      ...
    )
    
    if (rmi_result$status == 0) {
      removed_count <- removed_count + 1
      if (!quiet) {
        cli::cli_alert_success("Removed image {.val {image_id}}")
      }
    } else {
      cli::cli_alert_warning("Failed to remove image {.val {image_id}}")
    }
  }
  
  return(removed_count)
}

#' Run docker system prune
#' 
#' @param force Logical. Whether to force prune.
#' @param quiet Logical. Whether to suppress output.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Invisibly returns TRUE.
#' 
#' @keywords internal
run_docker_prune <- function(force, quiet, ...) {
  cli::cli_alert_info("Running docker system prune...")
  
  prune_args <- c("system", "prune")
  if (force) {
    prune_args <- c(prune_args, "-f")
  }
  
  prune_result <- execute_docker_command(
    "docker", 
    prune_args,
    quiet = quiet,
    echo = !quiet,
    error_on_status = FALSE,
    ...
  )
  
  if (prune_result$status == 0) {
    cli::cli_alert_success("Docker system prune completed successfully")
  } else {
    cli::cli_alert_warning("Docker system prune failed")
  }
  
  invisible(TRUE)
}