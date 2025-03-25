#' Execute a Docker command with standardized error handling
#' 
#' @param cmd Character. The command to execute (e.g., "docker", "docker-compose").
#' @param args Character vector. Arguments for the command.
#' @param quiet Logical. If TRUE, suppress command output. Default: FALSE.
#' @param echo Logical. Whether to echo command output. Default: !quiet.
#' @param error_on_status Logical. Whether to error on non-zero exit status. Default: FALSE.
#' @param ... Additional arguments passed to processx::run.
#' 
#' @return The result of processx::run.
#' 
#' @keywords internal
execute_docker_command <- function(cmd, args, quiet = FALSE, echo = !quiet, 
                                   error_on_status = FALSE, ...) {
  # Display command if not quiet
  if (!quiet) {
    cli::cli_alert_info("Executing: {.code {cmd} {paste(args, collapse = ' ')}}")
  }
  
  # Run command with standardized error handling
  result <- tryCatch({
    processx::run(
      cmd, 
      args = args,
      echo_cmd = FALSE,
      echo = echo,
      error_on_status = error_on_status,
      ...
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Error executing {cmd} command",
      "x" = "{conditionMessage(e)}"
    ))
  })
  
  # Check command status
  if (!error_on_status && result$status != 0) {
    cli::cli_abort(c(
      "{cmd} command failed with exit code {result$status}",
      "x" = "{result$stderr}"
    ))
  }
  
  return(result)
}

#' Check if Docker is available on the system
#'
#' @return Logical. TRUE if Docker is available in the system path.
#'
#' @keywords internal
is_docker_available <- function() {
  tryCatch({
    result <- processx::run("docker", "--version", error_on_status = FALSE)
    return(result$status == 0)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Check if Docker is available and abort if not
#'
#' @return Invisibly returns TRUE if Docker is available, otherwise aborts with an error message.
#'
#' @keywords internal
check_docker_available <- function() {
  if (!is_docker_available()) {
    cli::cli_abort(c(
      "Docker is not available on your system or not in your PATH",
      "i" = "Make sure Docker is installed and available in your PATH",
      "i" = "You may need to restart your R session after installing Docker"
    ))
  }
  
  invisible(TRUE)
}

#' Check if Docker Compose is available
#'
#' @return Logical. TRUE if docker-compose command is available in the system path.
#'
#' @keywords internal
is_docker_compose_available <- function() {
  tryCatch({
    result <- processx::run("docker-compose", "--version", error_on_status = FALSE)
    return(result$status == 0)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Check if a directory exists and is valid
#' 
#' @param app_dir Character. Path to check.
#' 
#' @return Invisibly returns TRUE if valid, otherwise aborts with an error message.
#' 
#' @keywords internal
check_app_directory <- function(app_dir) {
  app_dir <- fs::path_abs(app_dir)
  
  if (!fs::dir_exists(app_dir)) {
    cli::cli_abort("Application directory does not exist: {.file {app_dir}}")
  }
  
  invisible(TRUE)
}

#' Check if a Dockerfile exists in the specified directory
#' 
#' @param app_dir Character. Path to the application directory.
#' 
#' @return Invisibly returns TRUE if exists, otherwise aborts with an error message.
#' 
#' @keywords internal
check_dockerfile <- function(app_dir) {
  dockerfile_path <- fs::path(app_dir, "Dockerfile")
  
  if (!fs::file_exists(dockerfile_path)) {
    cli::cli_abort(c(
      "Dockerfile not found in: {.file {app_dir}}",
      "i" = "Run {.code dockerize()} first to create Docker configuration."
    ))
  }
  
  invisible(TRUE)
}

#' Detect if system is running on ARM64 architecture
#'
#' @return Logical. TRUE if the system is running on ARM64 architecture.
#'
#' @keywords internal
is_arm64 <- function() {
  # Check system architecture
  arch <- Sys.info()["machine"]
  
  # Return TRUE for ARM architectures (arm64, aarch64)
  return(grepl("arm|aarch", tolower(arch)))
}

#' Get platform flag for Docker if needed
#'
#' Returns platform flag for Docker commands if running on ARM64 and if app type is R.
#' This is needed because Shiny Server isn't available natively for ARM64 yet.
#'
#' @param app_type Character. Either "r" or "python".
#'
#' @return Character vector. Platform flag for Docker commands or empty vector if not needed.
#'
#' @keywords internal
get_platform_flag <- function(app_type) {
  if (is_arm64() && tolower(app_type) == "r") {
    cli::cli_alert_warning(c(
      "ARM64 architecture detected. Using x86_64 emulation for R Shiny container.",
      "i" = "This may impact performance until Shiny Server offers native ARM64 support."
    ))
    return(c("--platform", "linux/amd64"))
  } else {
    return(character(0))
  }
}