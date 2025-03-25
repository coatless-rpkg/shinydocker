#' Determine if docker-compose should be used for container operations
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' @param docker_compose Logical. User preference for using docker-compose.
#' 
#' @return Logical. TRUE if docker-compose should be used.
#' 
#' @keywords internal
should_use_compose <- function(app_dir, docker_compose = TRUE) {
  compose_path <- fs::path(app_dir, "docker-compose.yml")
  return(docker_compose && is_docker_compose_available() && fs::file_exists(compose_path))
}

#' Run container using docker-compose
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' @param port Integer. The port to map.
#' @param env_vars Named character vector. Environment variables.
#' @param detach Logical. Whether to run in detached mode.
#' @param quiet Logical. Whether to suppress output.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Character. Container ID if successful.
#' 
#' @keywords internal
run_with_compose <- function(app_dir, port, env_vars = NULL, detach = FALSE, quiet = FALSE, ...) {
  # Set environment variables for docker-compose
  if (!is.null(env_vars) && length(env_vars) > 0) {
    cli::cli_alert_info("Setting environment variables...")
    for (env_name in names(env_vars)) {
      Sys.setenv(env_name = env_vars[env_name])
    }
  }
  
  # Prepare docker-compose command arguments
  compose_args <- if (detach) c("up", "-d") else c("up")
  
  # Execute command
  result <- execute_docker_command(
    "docker-compose", 
    c("-f", fs::path(app_dir, "docker-compose.yml"), compose_args),
    quiet = quiet,
    echo = !quiet,
    error_on_status = FALSE,
    ...
  )
  
  # For detached mode, try to get container ID
  container_id <- NULL
  if (detach && result$status == 0) {
    # Use docker-compose ps to get container ID
    ps_result <- tryCatch({
      execute_docker_command(
        "docker-compose", 
        c("-f", fs::path(app_dir, "docker-compose.yml"), "ps", "-q"),
        quiet = TRUE
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(ps_result) && nzchar(ps_result$stdout)) {
      container_id <- trimws(ps_result$stdout)
    }
  }
  
  return(container_id)
}

#' Run container using docker run
#' 
#' @param tag Character. The Docker image tag.
#' @param port Integer. The port to map.
#' @param app_type Character. Either "r" or "python".
#' @param env_vars Named character vector. Environment variables.
#' @param detach Logical. Whether to run in detached mode.
#' @param quiet Logical. Whether to suppress output.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Character. Container ID if successful.
#' 
#' @keywords internal
run_with_docker <- function(tag, port, app_type, env_vars = NULL, detach = FALSE, quiet = FALSE, ...) {
  # Prepare docker run arguments
  run_args <- c("run")
  
  # Add platform flag if needed
  platform_flag <- get_platform_flag(app_type)
  if (length(platform_flag) > 0) {
    run_args <- c(run_args, platform_flag)
  }
  
  # Add port mapping
  run_args <- c(run_args, "-p", paste0(port, ":3838"))
  
  # Add environment variables if provided
  if (!is.null(env_vars) && length(env_vars) > 0) {
    cli::cli_alert_info("Setting {length(env_vars)} environment variables")
    for (env_name in names(env_vars)) {
      run_args <- c(run_args, "-e", paste0(env_name, "=", env_vars[env_name]))
    }
  }
  
  # Add detach flag if requested
  if (detach) {
    run_args <- c(run_args, "-d")
  }
  
  # Add the image tag
  run_args <- c(run_args, tag)
  
  # Execute command
  result <- execute_docker_command(
    "docker", 
    run_args,
    quiet = quiet,
    echo = !quiet,
    error_on_status = FALSE,
    ...
  )
  
  # Get container ID from output (if available)
  container_id <- NULL
  if (detach && result$status == 0 && nzchar(result$stdout)) {
    container_id <- trimws(result$stdout)
  }
  
  return(container_id)
}

#' Run a Docker container with a Shiny application
#' 
#' @param app_dir Character. Path to the Shiny application directory with Docker configuration.
#' @param tag Character. The tag for the Docker image to run. If NULL, a tag will be generated
#'   from the directory name (should match what was used in build_image).
#' @param port Integer. The port to map to the container's exposed port. Default: 3838.
#' @param detach Logical. If TRUE, run container in detached mode. Default: FALSE.
#' @param env_vars Named character vector. Environment variables to pass to the container.
#' @param docker_compose Logical. If TRUE, use docker-compose for running if available. Default: TRUE.
#' @param quiet Logical. If TRUE, suppress Docker command output. Default: FALSE.
#' @param force_port Logical. If TRUE, fail if requested port is unavailable. If FALSE, try to find an alternative port. Default: FALSE.
#' @param ... Additional arguments passed to processx.
#' 
#' @return For detached mode, invisibly returns the container ID if successful.
#'         For attached mode, the function will block until the container stops.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # First create Docker configuration and build
#' dockerize("path/to/my/shinyapp")
#' build_image("path/to/my/shinyapp")
#' 
#' # Run the container
#' run_container("path/to/my/shinyapp")
#' 
#' # Run detached on a different port
#' run_container("path/to/my/shinyapp", port = 8080, detach = TRUE)
#' }
run_container <- function(app_dir, tag = NULL, port = 3838,
                          detach = FALSE, env_vars = NULL, docker_compose = TRUE, 
                          quiet = FALSE, force_port = FALSE, ...) {
  # Validate inputs
  check_app_directory(app_dir)
  check_dockerfile(app_dir)
  
  
  # Prepare tag and app type
  if (is.null(tag)) {
    tag <- generate_image_tag(app_dir)
  }
  
  # Detect app type
  app_type <- detect_app_type(app_dir)
  
  # Check port availability and get an alternative if needed
  original_port <- port
  port <- handle_port_configuration(port, force_port)
  
  # Log port information clearly
  if (port != original_port) {
    cli::cli_alert_success("Using alternative port {.val {port}} instead of {.val {original_port}}")
  }
  
  # Update docker-compose.yml with the new port if needed
  if (port != 3838 && should_use_compose(app_dir, docker_compose)) {
    update_compose_port(app_dir, port)
  }
  
  # Determine running method
  container_id <- if (should_use_compose(app_dir, docker_compose)) {
    cli::cli_alert_info("Running container with docker-compose...")
    run_with_compose(app_dir, port, env_vars, detach, quiet, ...)
  } else {
    cli::cli_alert_info("Running container with docker run...")
    run_with_docker(tag, port, app_type, env_vars, detach, quiet, ...)
  }
  
  # Display access information
  if (detach) {
    cli::cli_alert_success("Container running in detached mode")
    app_url <- generate_app_url(app_type, port)
    cli::cli_alert_info("Access the Shiny app at: {.url {app_url}}")
  }
  
  return(invisible(container_id))
}

#' Update port mapping in docker-compose.yml
#' 
#' @param app_dir Character. Path to the application directory.
#' @param port Integer. New port to use.
#' 
#' @return Invisibly returns TRUE if successful.
#' 
#' @keywords internal
update_compose_port <- function(app_dir, port) {
  compose_path <- fs::path(app_dir, "docker-compose.yml")
  
  # Read compose file
  compose_data <- yaml::read_yaml(compose_path)
  
  # Update port in the first service (assumes single service)
  service_name <- names(compose_data$services)[1]
  current_ports <- compose_data$services[[service_name]]$ports
  
  # Find and replace the port mapping
  for (i in seq_along(current_ports)) {
    if (grepl("3838$", current_ports[i])) {
      compose_data$services[[service_name]]$ports[i] <- list(paste0(port, ":3838"))
      break
    }
  }
  
  # Write updated docker-compose.yml
  yaml::write_yaml(compose_data, compose_path)
  
  cli::cli_alert_info("Updated docker-compose.yml with new port mapping: {.val {port}}:3838")
  invisible(TRUE)
}

#' Find containers associated with an application
#' 
#' @param app_dir Character. Path to the application directory.
#' @param quiet Logical. Whether to suppress messages.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Character vector. Container IDs.
#' 
#' @keywords internal
find_containers_for_app <- function(app_dir, quiet = FALSE, ...) {
  expected_image <- paste0("shiny-", tolower(fs::path_file(app_dir)))
  
  if (!quiet) {
    cli::cli_alert_info("Finding containers for image: {.val {expected_image}}")
  }
  
  find_result <- execute_docker_command(
    "docker", 
    c("ps", "-a", "--filter", paste0("ancestor=", expected_image), "--format", "{{.ID}}"),
    quiet = TRUE,
    ...
  )
  
  container_ids <- strsplit(trimws(find_result$stdout), "\\s+")[[1]]
  
  if (length(container_ids) == 0 || (length(container_ids) == 1 && container_ids[1] == "")) {
    if (!quiet) {
      cli::cli_alert_warning("No containers found for image: {.val {expected_image}}")
    }
    return(character(0))
  }
  
  return(container_ids)
}

#' Stop a Docker container
#' 
#' @param container_id Character. ID of the container to stop.
#' @param quiet Logical. Whether to suppress output.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Logical. TRUE if successful.
#' 
#' @keywords internal
stop_docker_container <- function(container_id, quiet = FALSE, ...) {
  # Run docker stop command
  result <- execute_docker_command(
    "docker", 
    c("stop", container_id),
    quiet = quiet,
    echo = !quiet,
    error_on_status = FALSE,
    ...
  )
  
  # Check if command was successful
  if (result$status != 0) {
    cli::cli_alert_warning(c(
      "Failed to stop container {.val {container_id}} (exit code {result$status}):",
      "x" = "{result$stderr}"
    ))
    return(FALSE)
  }
  
  cli::cli_alert_success("Container {.val {container_id}} stopped successfully")
  return(TRUE)
}

#' Stop running Docker containers for a Shiny application
#' 
#' This function stops a running Docker container that was started with 
#' \code{run_container()} or \code{export()}. It can stop containers by ID
#' or by finding containers associated with the specified application directory.
#' If neither app_dir nor container_id is provided, it will stop all running containers.
#' 
#' @param app_dir Character. Path to the Shiny application directory with Docker configuration.
#'   If provided, the function will attempt to find and stop containers based on the image name.
#' @param container_id Character. ID or name of the container to stop. If NULL, 
#'   the function will try to find containers based on app_dir.
#' @param docker_compose Logical. If TRUE, use docker-compose for stopping if available. Default: TRUE.
#' @param quiet Logical. If TRUE, suppress Docker command output. Default: FALSE.
#' @param force Logical. If TRUE and stopping all containers, skip confirmation prompt. Default: FALSE.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # First run a container
#' result <- export("path/to/my/shinyapp", run = TRUE)
#' 
#' # Stop by container ID
#' stop_container(container_id = result$container_id)
#' 
#' # Or stop by app directory
#' stop_container("path/to/my/shinyapp")
#' 
#' # Stop all running containers
#' stop_container()
#' }
stop_container <- function(app_dir = NULL, container_id = NULL, 
                           docker_compose = TRUE, quiet = FALSE, force = FALSE, ...) {
  
  # Check if Docker is available
  check_docker_available()
  
  # Track success to determine if cleanup note should be shown
  success <- FALSE
  
  # Case 1: Use docker-compose if requested and available
  if (!is.null(app_dir) && docker_compose && should_use_compose(app_dir, docker_compose)) {
    success <- stop_with_compose(app_dir, quiet, ...)
  }
  # Case 2: Neither app_dir nor container_id provided - stop all containers
  else if (is.null(app_dir) && is.null(container_id)) {
    success <- stop_all_containers(quiet, force, ...)
    # Note is already shown in stop_all_containers when successful
    return(invisible(success))
  }
  # Case 3: app_dir provided but no container_id
  else if (!is.null(app_dir) && is.null(container_id)) {
    success <- stop_containers_for_app(app_dir, quiet, force, ...)
  }
  # Case 4: container_id provided
  else if (!is.null(container_id)) {
    success <- stop_docker_container(container_id, quiet, ...)
  }
  # Shouldn't reach here, but for safety
  else {
    cli::cli_alert_warning("No containers specified to stop")
    return(invisible(FALSE))
  }
  
  # Show cleanup note if containers were successfully stopped
  if (success) {
    cli::cli_alert_info("To remove stopped containers, use {.code cleanup_container()} function")
  }
  
  return(invisible(success))
}

#' Stop containers using docker-compose
#' 
#' @param app_dir Character. Path to the application directory.
#' @param quiet Logical. Whether to suppress output.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Logical. TRUE if successful.
#' 
#' @keywords internal
stop_with_compose <- function(app_dir, quiet = FALSE, ...) {
  cli::cli_alert_info("Stopping container with docker-compose...")
  
  # Run docker-compose stop (not down, which would remove containers)
  result <- execute_docker_command(
    "docker-compose", 
    c("-f", fs::path(app_dir, "docker-compose.yml"), "stop"),
    quiet = quiet,
    echo = !quiet,
    error_on_status = FALSE,
    ...
  )
  
  # Check if command was successful
  if (result$status != 0) {
    cli::cli_alert_warning(c(
      "Failed to stop container with docker-compose (exit code {result$status}):",
      "x" = "{result$stderr}"
    ))
    return(FALSE)
  }
  
  cli::cli_alert_success("Container stopped successfully with docker-compose")
  return(TRUE)
}


#' Stop all running Docker containers
#' 
#' @param quiet Logical. Whether to suppress output.
#' @param force Logical. Whether to skip confirmation.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Logical. TRUE if successful.
#' 
#' @keywords internal
stop_all_containers <- function(quiet = FALSE, force = FALSE, ...) {
  # Find all running containers
  cli::cli_alert_info("Finding all running containers...")
  
  find_result <- execute_docker_command(
    "docker", 
    c("ps", "--format", "{{.ID}}"),
    quiet = TRUE,
    ...
  )
  
  # Get container IDs from output
  container_ids <- strsplit(trimws(find_result$stdout), "\\s+")[[1]]
  
  # Check if any containers were found
  if (length(container_ids) == 0 || (length(container_ids) == 1 && container_ids[1] == "")) {
    cli::cli_alert_info("No running containers found")
    return(invisible(TRUE))
  }
  
  # Get container details for display
  details_result <- execute_docker_command(
    "docker", 
    c("ps", "--format", "table {{.ID}}\t{{.Image}}\t{{.Status}}\t{{.Names}}"),
    quiet = TRUE,
    ...
  )
  
  # Display container details
  cli::cli_alert_info("Found {length(container_ids)} running container(s):")
  cli::cli_text(details_result$stdout)
  
  # Ask for confirmation unless force=TRUE
  if (!force) {
    cli::cli_alert_warning("This will stop ALL running containers!")
    response <- readline("Do you want to continue? (y/N): ")
    
    if (tolower(substr(response, 1, 1)) != "y") {
      cli::cli_alert_info("Operation cancelled")
      return(invisible(FALSE))
    }
  }
  
  # Stop all containers
  cli::cli_alert_info("Stopping all containers...")
  
  # Count successful stops
  success_count <- 0
  
  for (id in container_ids) {
    result <- stop_docker_container(id, quiet, ...)
    if (result) {
      success_count <- success_count + 1
    }
  }
  
  # Report results
  if (success_count == length(container_ids)) {
    cli::cli_alert_success("All {length(container_ids)} container(s) stopped successfully")
    # Add note about cleanup_container() after all containers are stopped
    cli::cli_alert_info("To remove stopped containers, use {.code cleanup_container()} function")
    return(invisible(TRUE))
  } else if (success_count > 0) {
    cli::cli_alert_warning("Stopped {success_count} out of {length(container_ids)} container(s)")
    # Add note about cleanup_container()
    cli::cli_alert_info("To remove stopped containers, use {.code cleanup_container()} function")
    return(invisible(TRUE))
  } else {
    cli::cli_alert_danger("Failed to stop any containers")
    return(invisible(FALSE))
  }
}

#' Stop containers for a specific application
#' 
#' @param app_dir Character. Path to the application directory.
#' @param quiet Logical. Whether to suppress output.
#' @param force Logical. Whether to skip confirmation.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Logical. TRUE if successful.
#' 
#' @keywords internal
stop_containers_for_app <- function(app_dir, quiet = FALSE, force = FALSE, ...) {
  check_app_directory(app_dir)
  
  # Find containers for this app
  container_ids <- find_containers_for_app(app_dir, quiet, ...)
  
  # Check if any containers were found
  if (length(container_ids) == 0) {
    cli::cli_alert_warning("No containers found for {.file {app_dir}}")
    return(invisible(FALSE))
  }
  
  # If multiple containers found, handle selection
  if (length(container_ids) > 1) {
    return(handle_multiple_containers(container_ids, quiet, force, ...))
  } else {
    # Just one container, stop it
    return(invisible(stop_docker_container(container_ids[1], quiet, ...)))
  }
}

#' Handle stopping multiple containers
#' 
#' @param container_ids Character vector. Container IDs.
#' @param quiet Logical. Whether to suppress output.
#' @param force Logical. Whether to skip confirmation.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Logical. TRUE if successful.
#' 
#' @keywords internal
handle_multiple_containers <- function(container_ids, quiet = FALSE, force = FALSE, ...) {
  cli::cli_alert_info("Multiple containers found:")
  
  # Get container details
  for (i in seq_along(container_ids)) {
    details_result <- execute_docker_command(
      "docker", 
      c("ps", "-a", "--filter", paste0("id=", container_ids[i]), "--format", "{{.ID}}: {{.Status}}"),
      quiet = TRUE,
      ...
    )
    cli::cli_bullets(paste0("[", i, "] ", trimws(details_result$stdout)))
  }
  
  # If force=TRUE, stop all containers
  if (force) {
    cli::cli_alert_info("Stopping all containers due to force=TRUE")
    all_success <- TRUE
    for (id in container_ids) {
      result <- stop_docker_container(id, quiet, ...)
      all_success <- all_success && result
    }
    return(invisible(all_success))
  }
  
  # Let user choose which container to stop
  selection <- readline("Enter the number of the container to stop (or 'all' to stop all): ")
  
  if (selection == "all") {
    # Stop all containers
    all_success <- TRUE
    for (id in container_ids) {
      result <- stop_docker_container(id, quiet, ...)
      all_success <- all_success && result
    }
    if (all_success) {
      cli::cli_alert_success("All containers stopped successfully")
    }
    return(invisible(all_success))
  } else {
    selection <- as.integer(selection)
    if (is.na(selection) || selection < 1 || selection > length(container_ids)) {
      cli::cli_abort("Invalid selection")
    } else {
      return(invisible(stop_docker_container(container_ids[selection], quiet, ...)))
    }
  }
}