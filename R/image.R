#' Generate a default Docker image tag based on app directory
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' 
#' @return Character. Generated tag.
#' 
#' @keywords internal
generate_image_tag <- function(app_dir) {
  app_name <- tolower(fs::path_file(app_dir))
  # Sanitize the app name to remove any characters that might cause issues
  app_name <- gsub("[^a-z0-9_.-]", "", app_name)
  return(paste0("shiny-", app_name, ":latest"))
}

#' Prepare Docker build arguments
#' 
#' @param app_dir Character. Path to application directory.
#' @param tag Character. Image tag.
#' @param build_args Named character vector. Additional build arguments.
#' @param quiet Logical. Whether to use quiet mode.
#' 
#' @return Character vector of build arguments.
#' 
#' @keywords internal
prepare_build_arguments <- function(app_dir, tag, build_args = NULL, quiet = FALSE) {
  app_type <- detect_app_type(app_dir)
  platform_flag <- get_platform_flag(app_type)
  
  # Prepare docker build arguments
  build_args_vec <- c("build", "-t", tag)
  
  # Add platform flag if needed
  if (length(platform_flag) > 0) {
    build_args_vec <- c(build_args_vec, platform_flag)
  }
  
  # Add build args if provided
  if (!is.null(build_args) && length(build_args) > 0) {
    for (arg_name in names(build_args)) {
      build_args_vec <- c(build_args_vec, "--build-arg", 
                          paste0(arg_name, "=", build_args[arg_name]))
    }
  }
  
  # Add quietness flag if requested
  if (quiet) {
    build_args_vec <- c(build_args_vec, "--quiet")
  }
  
  # Add context path (the application directory)
  build_args_vec <- c(build_args_vec, app_dir)
  
  return(build_args_vec)
}

#' Build Docker image for a Shiny application
#' 
#' @param app_dir Character. Path to the Shiny application directory with Docker configuration.
#' @param tag Character. The tag for the Docker image. If NULL, a tag will be generated
#'   from the directory name.
#' @param build_args Named character vector. Additional build arguments to pass to Docker.
#' @param quiet Logical. If TRUE, suppress Docker build output. Default: FALSE.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Invisibly returns the image ID if successful.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # First create Docker configuration
#' dockerize("path/to/my/shinyapp")
#' 
#' # Then build the image
#' build_image("path/to/my/shinyapp")
#' 
#' # With a custom tag
#' build_image("path/to/my/shinyapp", tag = "myorg/myapp:latest")
#' }
build_image <- function(app_dir, tag = NULL, build_args = NULL, quiet = FALSE, ...) {
  # Validate inputs
  check_app_directory(app_dir)
  check_dockerfile(app_dir)
  
  # Generate tag if not provided
  if (is.null(tag)) {
    tag <- generate_image_tag(app_dir)
  }
  
  # Prepare build arguments
  build_args_vec <- prepare_build_arguments(app_dir, tag, build_args, quiet)
  
  # Show command if not quiet
  if (!quiet) {
    cli::cli_alert_info("Building Docker image: {.val {tag}}")
    cli::cli_process_start("Building image")
  }
  
  # Execute build command
  result <- execute_docker_command(
    "docker", 
    build_args_vec,
    quiet = quiet,
    ...
  )
  
  # Stop process indicator if we were showing it
  if (!quiet) {
    cli::cli_process_done()
  }
  
  cli::cli_alert_success("Docker image built successfully: {.val {tag}}")
  invisible(tag)
}