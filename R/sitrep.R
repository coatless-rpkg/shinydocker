#' Report system information health
#' 
#' @return List with system information details
#' 
#' @keywords internal
health_system_info <- function() {
  system_info <- list(
    os = Sys.info()["sysname"],
    version = Sys.info()["release"],
    arch = Sys.info()["machine"]
  )
  
  # Display system information
  cli::cli_alert_info("Operating System: {.val {system_info$os}} {.val {system_info$version}}")
  cli::cli_alert_info("Architecture: {.val {system_info$arch}}")
  
  # Check if on ARM64 architecture (M1/M2 Mac or similar)
  if (is_arm64()) {
    cli::cli_alert_warning(c(
      "ARM64 architecture detected.",
      "i" = "R Shiny apps may use emulation for compatibility."
    ))
    system_info$is_arm64 <- TRUE
  } else {
    system_info$is_arm64 <- FALSE
  }
  
  return(system_info)
}

#' Report Docker installation health
#' 
#' @param timeout Numeric. Timeout in seconds for commands
#' @param ... Additional arguments passed to processx
#' 
#' @return List with Docker installation details
#' 
#' @keywords internal
health_docker_installation <- function(timeout = 30, ...) {
  result <- list(
    available = FALSE,
    version = NULL
  )
  
  # Check if Docker is available
  if (is_docker_available()) {
    result$available <- TRUE
    cli::cli_alert_success("Docker is installed")
    
    # Get Docker version
    version_result <- tryCatch({
      execute_docker_command(
        "docker", 
        c("version", "--format", "{{.Server.Version}}"),
        quiet = TRUE,
        timeout = timeout,
        ...
      )
    }, error = function(e) {
      cli::cli_alert_danger("Failed to get Docker version: {e$message}")
      return(NULL)
    })
    
    if (!is.null(version_result) && version_result$status == 0) {
      version <- trimws(version_result$stdout)
      result$version <- version
      cli::cli_alert_info("Docker version: {.val {version}}")
    } else {
      cli::cli_alert_warning("Could not determine Docker version")
    }
  } else {
    cli::cli_alert_danger("Docker is not installed or not in your PATH")
    cli::cli_alert_info("Please install Docker from {.url https://docs.docker.com/get-docker/}")
  }
  
  return(result)
}

#' Report Docker daemon health status
#' 
#' @param timeout Numeric. Timeout in seconds for commands
#' @param ... Additional arguments passed to processx
#' 
#' @return List with Docker daemon status details
#' 
#' @keywords internal
health_docker_daemon <- function(timeout = 30, ...) {
  result <- list(
    running = FALSE,
    error = NULL
  )
  
  # Check if Docker daemon is running
  info_result <- tryCatch({
    execute_docker_command(
      "docker", 
      c("info", "--format", "{{.ServerVersion}}"),
      quiet = TRUE,
      error_on_status = FALSE,
      timeout = timeout,
      ...
    )
  }, error = function(e) {
    cli::cli_alert_danger("Failed to check Docker daemon: {e$message}")
    result$error <- e$message
    return(NULL)
  })
  
  if (!is.null(info_result) && info_result$status == 0) {
    result$running <- TRUE
    cli::cli_alert_success("Docker daemon is running")
  } else {
    error_msg <- if (!is.null(info_result)) info_result$stderr else "Unknown error"
    result$error <- error_msg
    
    cli::cli_alert_danger("Docker daemon is not running")
    
    # Provide platform-specific guidance
    if (Sys.info()["sysname"] == "Windows") {
      cli::cli_alert_info("Check if Docker Desktop is running")
    } else if (Sys.info()["sysname"] == "Darwin") {
      cli::cli_alert_info("Check if Docker Desktop is running")
    } else {
      cli::cli_alert_info("Start the Docker daemon with: {.code sudo systemctl start docker}")
    }
  }
  
  return(result)
}

#' Report Docker permissions health
#' 
#' @param timeout Numeric. Timeout in seconds for commands
#' @param ... Additional arguments passed to processx
#' 
#' @return List with Docker permission details
#' 
#' @keywords internal
health_docker_permissions <- function(timeout = 30, ...) {
  result <- list(
    permission_issue = FALSE,
    in_docker_group = FALSE
  )
  
  # Check if user can run Docker without sudo
  hello_result <- tryCatch({
    execute_docker_command(
      "docker", 
      c("run", "--rm", "hello-world"),
      quiet = TRUE,
      error_on_status = FALSE,
      timeout = timeout,
      ...
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(hello_result) && hello_result$status == 0) {
    cli::cli_alert_success("User has permission to run Docker commands")
  } else {
    result$permission_issue <- TRUE
    
    # Check if user is in docker group
    groups_result <- tryCatch({
      processx::run("groups", error_on_status = FALSE, timeout = timeout)
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(groups_result) && groups_result$status == 0) {
      user_groups <- strsplit(trimws(groups_result$stdout), "\\s+")[[1]]
      result$in_docker_group <- "docker" %in% user_groups
      
      if (result$in_docker_group) {
        cli::cli_alert_info("User is in the docker group, but may need to log out and back in")
      } else {
        cli::cli_alert_warning("User is not in the docker group")
        cli::cli_alert_info("Add user to docker group: {.code sudo usermod -aG docker $USER}")
        cli::cli_alert_info("Then log out and back in for changes to take effect")
      }
    } else {
      cli::cli_alert_warning("Could not determine group membership")
    }
  }
  
  return(result)
}

#' Report Docker Compose availability health
#' 
#' @param timeout Numeric. Timeout in seconds for commands
#' @param ... Additional arguments passed to processx
#' 
#' @return List with Docker Compose details
#' 
#' @keywords internal
health_docker_compose <- function(timeout = 30, ...) {
  result <- list(
    available = FALSE,
    version = NULL
  )
  
  # Check if Docker Compose is available
  if (is_docker_compose_available()) {
    result$available <- TRUE
    cli::cli_alert_success("Docker Compose is installed")
    
    # Get Docker Compose version
    version_result <- tryCatch({
      execute_docker_command(
        "docker-compose", 
        c("version", "--short"),
        quiet = TRUE,
        timeout = timeout,
        ...
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(version_result) && version_result$status == 0) {
      version <- trimws(version_result$stdout)
      result$version <- version
      cli::cli_alert_info("Docker Compose version: {.val {version}}")
    } else {
      cli::cli_alert_warning("Could not determine Docker Compose version")
    }
  } else {
    cli::cli_alert_warning("Docker Compose is not installed or not in your PATH")
    cli::cli_alert_info("Docker Compose simplifies container management")
    cli::cli_alert_info("Install: {.url https://docs.docker.com/compose/install/}")
  }
  
  return(result)
}

#' Report Docker resources health
#' 
#' @param verbose Logical. If TRUE, provide more detailed output
#' @param timeout Numeric. Timeout in seconds for commands
#' @param ... Additional arguments passed to processx
#' 
#' @return List with Docker resource details
#' 
#' @keywords internal
health_docker_resources <- function(verbose = FALSE, timeout = 30, ...) {
  result <- list(
    disk_space_warning = FALSE,
    disk_space_info = NULL
  )
  
  # Check Docker disk space usage
  df_result <- tryCatch({
    execute_docker_command(
      "docker", 
      c("system", "df"),
      quiet = !verbose,
      timeout = timeout,
      ...
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(df_result) && df_result$status == 0) {
    result$disk_space_info <- df_result$stdout
    
    # Look for high usage indicators in the output
    if (grepl("9[0-9]\\.[0-9]%|100\\.0%", df_result$stdout)) {
      result$disk_space_warning <- TRUE
      cli::cli_alert_warning("Docker is using >90% of allocated disk space")
      cli::cli_alert_info("Run {.code docker system prune} to clean up unused data")
    } else {
      cli::cli_alert_success("Docker disk space usage is within normal ranges")
    }
    
    # Display resource info in verbose mode
    if (verbose) {
      cli::cli_h3("Detailed Resource Information")
      cli::cli_text(df_result$stdout)
    }
  } else {
    cli::cli_alert_warning("Could not check Docker disk space usage")
  }
  
  return(result)
}

#' Report port availability health
#' 
#' @param ports Integer vector. Ports to check
#' @param timeout Numeric. Timeout in seconds for commands
#' @param port_timeout Numeric. Timeout in seconds for port checking
#' @param ... Additional arguments passed to processx
#' 
#' @return List with port availability details
#' 
#' @keywords internal
health_port_availability <- function(ports, timeout = 30, port_timeout = 2, ...) {
  result <- list(
    ports_checked = ports,
    ports_available = logical(length(ports)),
    ports_unavailable = logical(length(ports))
  )
  
  # Check each port
  for (i in seq_along(ports)) {
    port <- ports[i]
    available <- is_port_available(port)
    result$ports_available[i] <- available
    result$ports_unavailable[i] <- !available
    
    if (available) {
      cli::cli_alert_success("Port {.val {port}} is available")
    } else {
      cli::cli_alert_warning("Port {.val {port}} is already in use")
      
      # Try to identify what's using the port on Unix-like systems
      if (Sys.info()["sysname"] != "Windows") {
        process_info <- tryCatch({
          processx::run(
            "lsof", 
            c("-i", paste0(":", port)),
            error_on_status = FALSE,
            timeout = timeout,
            ...
          )
        }, error = function(e) {
          return(NULL)
        })
        
        if (!is.null(process_info) && process_info$status == 0 && nzchar(process_info$stdout)) {
          cli::cli_text("Process using port {.val {port}}:")
          cli::cli_text(process_info$stdout)
        }
      }
    }
  }
  
  # Summary of port availability
  unavailable_ports <- ports[result$ports_unavailable]
  if (length(unavailable_ports) > 0) {
    cli::cli_alert_info("Consider using alternative ports: {paste(unavailable_ports + 1, collapse = ', ')}")
  }
  
  return(result)
}

#' Get Docker environment issues and recommendations
#' 
#' @param results List. Results from Docker environment checks
#' 
#' @return List with issues and recommendations
#' 
#' @keywords internal
get_docker_issues <- function(results) {
  issues <- character(0)
  recommendations <- character(0)
  
  # Check Docker installation
  if (!results$docker_available) {
    issues <- c(issues, "Docker is not installed or not in your PATH")
    recommendations <- c(recommendations, "Install Docker from https://docs.docker.com/get-docker/")
    # Early return as other checks depend on Docker
    return(list(issues = issues, recommendations = recommendations))
  }
  
  # Check Docker daemon
  if (!results$docker_info$running) {
    issues <- c(issues, "Docker daemon is not running")
    
    if (Sys.info()["sysname"] == "Windows" || Sys.info()["sysname"] == "Darwin") {
      recommendations <- c(recommendations, "Start Docker Desktop application")
    } else {
      recommendations <- c(recommendations, "Start Docker daemon with: sudo systemctl start docker")
    }
  }
  
  # Check permissions (Unix only)
  if (Sys.info()["sysname"] != "Windows" && 
      results$docker_info$running && 
      results$docker_info$permission_issue) {
    issues <- c(issues, "User does not have permission to run Docker commands")
    
    if (!results$docker_info$in_docker_group) {
      recommendations <- c(recommendations, 
                           "Add user to docker group: sudo usermod -aG docker $USER and log out/in")
    } else {
      recommendations <- c(recommendations,
                           "Log out and log back in for docker group membership to take effect")
    }
  }
  
  # Check Docker Compose
  if (!results$docker_compose_available) {
    issues <- c(issues, "Docker Compose is not installed")
    recommendations <- c(recommendations, 
                         "Install Docker Compose from https://docs.docker.com/compose/install/")
  }
  
  # Check disk space
  if (results$docker_info$running && results$docker_info$disk_space_warning) {
    issues <- c(issues, "Docker is using >90% of allocated disk space")
    recommendations <- c(recommendations, "Run 'docker system prune' to clean up unused data")
  }
  
  # Check port availability
  unavailable_ports <- results$port_diagnostics$ports_checked[results$port_diagnostics$ports_unavailable]
  if (length(unavailable_ports) > 0) {
    issues <- c(issues, paste("Ports in use:", paste(unavailable_ports, collapse = ", ")))
    alt_ports <- unavailable_ports + 1
    recommendations <- c(recommendations, 
                         paste("Use alternative ports:", paste(alt_ports, collapse = ", ")))
  }
  
  # Check ARM64 architecture
  if (results$system_info$is_arm64) {
    issues <- c(issues, "ARM64 architecture may cause compatibility issues with R Shiny Server")
    recommendations <- c(recommendations, 
                         "Be aware that R Shiny apps will use emulation which may affect performance")
  }
  
  return(list(issues = issues, recommendations = recommendations))
}

#' Report Shiny app structure health
#' 
#' @param app_dir Character. Path to the Shiny application directory
#' @param verbose Logical. If TRUE, provide more detailed output
#' 
#' @return List with Shiny app details
#' 
#' @keywords internal
health_shiny_app <- function(app_dir, verbose = FALSE) {
  result <- list(
    valid = FALSE,
    type = NULL,
    files = list()
  )
  
  # Check if directory exists
  if (!fs::dir_exists(app_dir)) {
    cli::cli_alert_danger("Directory does not exist: {.file {app_dir}}")
    return(result)
  }
  
  # List all files in directory
  all_files <- fs::dir_ls(app_dir, recurse = TRUE, type = "file")
  r_files <- grep("\\.R$|\\.r$", all_files, value = TRUE)
  py_files <- grep("\\.py$", all_files, value = TRUE)
  
  # Store file lists
  result$files$r <- basename(r_files)
  result$files$python <- basename(py_files)
  result$files$total_r <- length(r_files)
  result$files$total_py <- length(py_files)
  
  # Check for Shiny app files
  r_app_files <- c("app.R", "ui.R", "server.R")
  py_app_files <- c("app.py", "ui.py", "server.py")
  
  has_r_app <- any(basename(r_files) %in% r_app_files)
  has_py_app <- any(basename(py_files) %in% py_app_files)
  
  # Determine app type
  if (has_r_app) {
    result$type <- "r"
    result$valid <- TRUE
    cli::cli_alert_success("Valid R Shiny application detected")
    
    found_files <- basename(r_files)[basename(r_files) %in% r_app_files]
    cli::cli_alert_info("Found Shiny files: {.file {paste(found_files, collapse = ', ')}}")
  } else if (has_py_app) {
    result$type <- "python"
    result$valid <- TRUE
    cli::cli_alert_success("Valid Python Shiny application detected")
    
    found_files <- basename(py_files)[basename(py_files) %in% py_app_files]
    cli::cli_alert_info("Found Shiny files: {.file {paste(found_files, collapse = ', ')}}")
  } else {
    cli::cli_alert_danger("No valid Shiny application structure detected")
    cli::cli_alert_info("Expected files for R apps: {.file app.R}, or {.file ui.R} and {.file server.R}")
    cli::cli_alert_info("Expected files for Python apps: {.file app.py}, or {.file ui.py} and {.file server.py}")
  }
  
  # Show file counts in verbose mode
  if (verbose) {
    cli::cli_alert_info("Found {result$files$total_r} R files and {result$files$total_py} Python files")
    
    if (result$files$total_r > 0 && verbose) {
      cli::cli_h3("R Files")
      cli::cli_ul(result$files$r)
    }
    
    if (result$files$total_py > 0 && verbose) {
      cli::cli_h3("Python Files")
      cli::cli_ul(result$files$python)
    }
  }
  
  return(result)
}

#' Report dependencies health in a Shiny app
#' 
#' @param app_dir Character. Path to the Shiny application directory
#' @param app_type Character. Either "r" or "python"
#' @param verbose Logical. If TRUE, provide more detailed output
#' 
#' @return List with dependency details
#' 
#' @keywords internal
health_dependencies <- function(app_dir, app_type, verbose = FALSE) {
  result <- list(
    dependencies = character(0),
    count = 0
  )
  
  # Detect dependencies
  cli::cli_alert_info("Detecting {ifelse(app_type == 'r', 'R', 'Python')} package dependencies...")
  deps <- detect_dependencies(app_dir, app_type)
  
  if (length(deps) > 0) {
    result$dependencies <- deps
    result$count <- length(deps)
    cli::cli_alert_success("Found {length(deps)} dependencies")
    
    if (verbose) {
      cli::cli_h3("Dependencies")
      cli::cli_ol(deps)
    }
  } else {
    cli::cli_alert_info("No package dependencies detected beyond base Shiny")
  }
  
  return(result)
}

#' Report Docker configuration health in a Shiny app directory
#' 
#' @param app_dir Character. Path to the Shiny application directory
#' @param app_type Character. Either "r" or "python"
#' @param verbose Logical. If TRUE, provide more detailed output
#' @param timeout Numeric. Timeout in seconds for commands
#' @param ... Additional arguments passed to processx
#' 
#' @return List with Docker configuration details
#' 
#' @keywords internal
health_docker_config <- function(app_dir, app_type, verbose = FALSE, timeout = 30, ...) {
  result <- list(
    has_dockerfile = FALSE,
    has_compose = FALSE,
    has_dockerignore = FALSE
  )
  
  # Check for Dockerfile
  dockerfile_path <- fs::path(app_dir, "Dockerfile")
  if (fs::file_exists(dockerfile_path)) {
    result$has_dockerfile <- TRUE
    cli::cli_alert_success("Dockerfile exists: {.file {dockerfile_path}}")
    
    # Analyze Dockerfile in verbose mode
    if (verbose) {
      dockerfile_content <- readLines(dockerfile_path)
      base_image_line <- grep("^FROM", dockerfile_content, value = TRUE)
      if (length(base_image_line) > 0) {
        cli::cli_alert_info("Base image: {.val {base_image_line}}")
      }
    }
  } else {
    cli::cli_alert_warning("Dockerfile not found")
    cli::cli_alert_info("Create Docker configuration with {.code dockerize()}")
  }
  
  # Check for docker-compose.yml
  compose_path <- fs::path(app_dir, "docker-compose.yml")
  if (fs::file_exists(compose_path)) {
    result$has_compose <- TRUE
    cli::cli_alert_success("docker-compose.yml exists: {.file {compose_path}}")
    
    # Parse compose file in verbose mode
    if (verbose) {
      tryCatch({
        compose_data <- yaml::read_yaml(compose_path)
        service_name <- names(compose_data$services)[1]
        if (!is.null(service_name)) {
          port_mapping <- compose_data$services[[service_name]]$ports
          if (!is.null(port_mapping)) {
            cli::cli_alert_info("Port mapping: {.val {paste(port_mapping, collapse = ', ')}}")
          }
        }
      }, error = function(e) {
        cli::cli_alert_warning("Could not parse docker-compose.yml")
      })
    }
  } else {
    cli::cli_alert_warning("docker-compose.yml not found")
  }
  
  # Check for .dockerignore
  ignore_path <- fs::path(app_dir, ".dockerignore")
  if (fs::file_exists(ignore_path)) {
    result$has_dockerignore <- TRUE
    cli::cli_alert_success(".dockerignore exists: {.file {ignore_path}}")
  } else {
    cli::cli_alert_warning(".dockerignore not found")
  }
  
  return(result)
}

#' Report Docker image health for a Shiny app
#' 
#' @param app_dir Character. Path to the Shiny application directory
#' @param verbose Logical. If TRUE, provide more detailed output
#' @param timeout Numeric. Timeout in seconds for commands
#' @param ... Additional arguments passed to processx
#' 
#' @return List with Docker image details
#' 
#' @keywords internal
health_docker_image <- function(app_dir, verbose = FALSE, timeout = 30, ...) {
  result <- list(
    exists = FALSE,
    tag = NULL,
    id = NULL,
    size = NULL
  )
  
  # Generate expected image tag
  expected_image <- paste0("shiny-", tolower(fs::path_file(app_dir)))
  result$tag <- expected_image
  
  # Look for image
  image_result <- tryCatch({
    execute_docker_command(
      "docker", 
      c("images", expected_image, "--format", "{{.ID}} {{.Size}}"),
      quiet = TRUE,
      error_on_status = FALSE,
      timeout = timeout,
      ...
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(image_result) && image_result$status == 0 && nzchar(image_result$stdout)) {
    parts <- strsplit(trimws(image_result$stdout), "\\s+")[[1]]
    result$exists <- TRUE
    result$id <- parts[1]
    if (length(parts) > 1) result$size <- parts[2]
    
    cli::cli_alert_success("Docker image exists: {.val {expected_image}}")
    cli::cli_alert_info("Image ID: {.val {result$id}}, Size: {.val {result$size}}")
  } else {
    cli::cli_alert_warning("Docker image not found: {.val {expected_image}}")
    cli::cli_alert_info("Build image with {.code build_image()}")
  }
  
  return(result)
}

#' Report Docker containers health for a Shiny app
#' 
#' @param app_dir Character. Path to the Shiny application directory
#' @param image_name Character. Name of the Docker image
#' @param verbose Logical. If TRUE, provide more detailed output
#' @param timeout Numeric. Timeout in seconds for commands
#' @param ... Additional arguments passed to processx
#' 
#' @return List with Docker container details
#' 
#' @keywords internal
health_docker_containers <- function(app_dir, image_name, verbose = FALSE, timeout = 30, ...) {
  result <- list(
    running = character(0),
    stopped = character(0),
    total_running = 0,
    total_stopped = 0
  )
  
  # Look for containers based on this image
  container_result <- tryCatch({
    execute_docker_command(
      "docker", 
      c("ps", "-a", "--filter", paste0("ancestor=", image_name), 
        "--format", "{{.ID}} {{.Status}} {{.Ports}}"),
      quiet = TRUE,
      error_on_status = FALSE,
      timeout = timeout,
      ...
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(container_result) && container_result$status == 0 && nzchar(container_result$stdout)) {
    # Process container data
    container_lines <- strsplit(trimws(container_result$stdout), "\n")[[1]]
    
    # Process each container
    for (line in container_lines) {
      parts <- strsplit(line, "\\s+", 2)[[1]]
      id <- parts[1]
      status_info <- parts[2]
      
      # Determine if running or stopped
      if (grepl("^Up", status_info)) {
        result$running <- c(result$running, id)
      } else {
        result$stopped <- c(result$stopped, id)
      }
    }
    
    result$total_running <- length(result$running)
    result$total_stopped <- length(result$stopped)
    
    # Report findings
    if (result$total_running > 0) {
      cli::cli_alert_success("Found {result$total_running} running container(s)")
      if (verbose) {
        for (id in result$running) {
          # Get container details
          details_result <- execute_docker_command(
            "docker", 
            c("ps", "--filter", paste0("id=", id), "--format", "{{.Ports}}"),
            quiet = TRUE,
            ...
          )
          ports <- trimws(details_result$stdout)
          cli::cli_bullets(c(id, " - Ports: ", ports))
        }
      }
    } else {
      cli::cli_alert_info("No running containers found for this app")
    }
    
    if (result$total_stopped > 0) {
      cli::cli_alert_info("Found {result$total_stopped} stopped container(s)")
      if (verbose) {
        cli::cli_ul(result$stopped)
      }
    }
  } else {
    cli::cli_alert_info("No containers found for this app")
  }
  
  return(result)
}

#' Get issues and recommendations for Shiny app containerization
#' 
#' @param results List. Results from Shiny app checks
#' 
#' @return List with issues and recommendations
#' 
#' @keywords internal
get_app_issues <- function(results) {
  issues <- character(0)
  recommendations <- character(0)
  
  # Check Docker availability
  if (!results$docker_available) {
    issues <- c(issues, "Docker is not available")
    recommendations <- c(recommendations, "Install Docker and ensure it's in your PATH")
    recommendations <- c(recommendations, "Run sitrep_docker() for a complete Docker environment check")
  }
  
  # Check app validity
  if (!results$app_diagnostics$valid) {
    issues <- c(issues, "No valid Shiny application structure detected")
    recommendations <- c(recommendations, 
                         "Ensure your directory contains proper Shiny app files (app.R or ui.R/server.R for R)")
    recommendations <- c(recommendations, 
                         "For Python Shiny apps, ensure you have app.py or ui.py/server.py")
  } else {
    # Docker configuration issues - prioritize Dockerfile
    if (!results$docker_config$has_dockerfile) {
      issues <- c(issues, "Docker configuration missing")
      recommendations <- c(recommendations, 
                           paste0("Run dockerize('", results$app_diagnostics$type, 
                                  "') to create all required Docker configuration files"))
    }
    
    # Docker image issues
    if (results$docker_config$has_dockerfile && 
        (is.null(results$images) || !isTRUE(results$images$exists))) {
      issues <- c(issues, "Docker image not found")
      recommendations <- c(recommendations, 
                           "Build the Docker image with build_image()")
    }
  }
  
  return(list(issues = issues, recommendations = recommendations))
}

#' Situation report for Docker environment
#' 
#' This function provides a comprehensive report of the Docker environment including
#' installation status, version information, daemon status, permissions, resource usage,
#' and port availability. It helps identify issues with Docker setup that might affect
#' containerizing Shiny applications.
#' 
#' @param check_ports Logical. If TRUE, check common port conflicts. Default: TRUE.
#' @param verbose Logical. If TRUE, provide more detailed output. Default: FALSE.
#' @param timeout Numeric. Timeout in seconds for Docker commands. Default: 30.
#' @param port_timeout Numeric. Timeout in seconds for port checking. Default: 2.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Invisibly returns a list with Docker environment diagnostic results.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Basic Docker environment report
#' sitrep_docker()
#' 
#' # Detailed Docker environment report
#' sitrep_docker(verbose = TRUE)
#' 
#' # Skip port checking
#' sitrep_docker(check_ports = FALSE)
#' 
#' # Use shorter timeout for port checking
#' sitrep_docker(port_timeout = 1)
#' }
sitrep_docker <- function(check_ports = TRUE, verbose = FALSE, timeout = 30, 
                          port_timeout = 2, ...) {
  # Initialize results list
  results <- list(
    docker_available = FALSE,
    docker_compose_available = FALSE,
    docker_version = NULL,
    docker_info = NULL,
    port_diagnostics = list(),
    system_info = list()
  )
  
  # Start diagnostic output
  cli::cli_h1("Docker Environment Report")
  
  # Get system information
  cli::cli_h2("System Information")
  results$system_info <- health_system_info()
  
  # Check Docker availability
  cli::cli_h2("Docker Installation")
  docker_installation <- health_docker_installation(timeout, ...)
  results$docker_available <- docker_installation$available
  results$docker_version <- docker_installation$version
  
  if (!docker_installation$available) {
    # No point continuing most checks if Docker isn't available
    return(invisible(results))
  }
  
  # Check if Docker daemon is running
  cli::cli_h2("Docker Daemon Status")
  results$docker_info <- health_docker_daemon(timeout, ...)
  
  # Check Docker permissions (for non-Windows systems)
  if (results$docker_info$running && Sys.info()["sysname"] != "Windows") {
    cli::cli_h2("Docker Permissions")
    permission_results <- health_docker_permissions(timeout, ...)
    results$docker_info <- c(results$docker_info, permission_results)
  }
  
  # Check Docker Compose availability
  cli::cli_h2("Docker Compose")
  compose_results <- health_docker_compose(timeout, ...)
  results$docker_compose_available <- compose_results$available
  results$docker_compose_version <- compose_results$version
  
  # Check Docker disk space
  if (results$docker_info$running) {
    cli::cli_h2("Docker Resources")
    resource_results <- health_docker_resources(verbose, timeout, ...)
    results$docker_info <- c(results$docker_info, resource_results)
  }
  
  # Check common ports
  if (check_ports) {
    cli::cli_h2("Port Availability")
    results$port_diagnostics <- health_port_availability(c(3838, 8080, 80, 443), timeout, port_timeout, ...)
  }
  
  # Summarize diagnostics
  cli::cli_h2("Docker Environment Summary")
  
  # Get issues and recommendations
  diagnosis <- get_docker_issues(results)
  issues <- diagnosis$issues
  recommendations <- diagnosis$recommendations
  
  # Display summary
  if (length(issues) == 0) {
    cli::cli_alert_success("Docker environment ready for Shiny containerization")
  } else {
    cli::cli_alert_warning("Found {length(issues)} issue(s):")
    cli::cli_ol(issues)
    
    cli::cli_alert_info("Recommendations:")
    cli::cli_ol(recommendations)
  }
  
  # Return results
  results$issues <- issues
  results$recommendations <- recommendations
  
  invisible(results)
}

#' Situation report for Shiny app containerization
#' 
#' This function analyzes a Shiny application directory and checks its readiness
#' for containerization. It examines Shiny app structure, Docker configuration files,
#' dependency detection, image building status, and running containers.
#' 
#' @param app_dir Character. Path to the Shiny application directory to analyze.
#' @param check_dependencies Logical. If TRUE, detect and display app dependencies. Default: TRUE.
#' @param verbose Logical. If TRUE, provide more detailed output. Default: FALSE.
#' @param timeout Numeric. Timeout in seconds for commands. Default: 30.
#' @param ... Additional arguments passed to processx.
#' 
#' @return Invisibly returns a list with Shiny app containerization diagnostic results.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Basic Shiny app containerization report
#' sitrep_app_conversion("path/to/my/shinyapp")
#' 
#' # Detailed report with dependency analysis
#' sitrep_app_conversion("path/to/my/shinyapp", verbose = TRUE)
#' 
#' # Skip dependency detection
#' sitrep_app_conversion("path/to/my/shinyapp", check_dependencies = FALSE)
#' }
sitrep_app_conversion <- function(app_dir, check_dependencies = TRUE, 
                                  verbose = FALSE, timeout = 30, ...) {
  # Initialize results list
  results <- list(
    app_diagnostics = list(),
    docker_config = list(),
    images = list(exists = FALSE),
    containers = list()
  )
  
  # Validate app_dir exists
  app_dir <- fs::path_abs(app_dir)
  if (!fs::dir_exists(app_dir)) {
    cli::cli_abort("Application directory does not exist: {.file {app_dir}}")
  }
  
  # Start diagnostic output
  cli::cli_h1("Shiny App Containerization Report")
  cli::cli_alert_info("Analyzing Shiny application at: {.file {app_dir}}")
  
  # Check if Docker is available
  docker_available <- is_docker_available()
  results$docker_available <- docker_available
  
  if (!docker_available) {
    cli::cli_alert_danger("Docker is not available or not in your PATH")
    cli::cli_alert_info("Please install Docker and ensure it's in your PATH")
    cli::cli_alert_info("Run {.code sitrep_docker()} for a complete Docker environment check")
  }
  
  # Check if it's a valid Shiny app
  cli::cli_h2("Shiny Application Structure")
  app_check_results <- health_shiny_app(app_dir, verbose)
  results$app_diagnostics <- app_check_results
  
  # Continue only if it's a valid Shiny app
  if (app_check_results$valid) {
    # Check for dependencies
    if (check_dependencies) {
      cli::cli_h2("Application Dependencies")
      dependency_results <- health_dependencies(app_dir, app_check_results$type, verbose)
      results$dependencies <- dependency_results$dependencies
    }
    
    # Check Docker configuration
    cli::cli_h2("Docker Configuration")
    docker_config_results <- health_docker_config(app_dir, app_check_results$type, verbose, timeout, ...)
    results$docker_config <- docker_config_results
    
    # Docker image and container checks
    if (docker_available && docker_config_results$has_dockerfile) {
      # Check for Docker images
      cli::cli_h2("Docker Image Status")
      expected_image_name <- paste0("shiny-", tolower(fs::path_file(app_dir)))
      image_results <- health_docker_image(app_dir, verbose, timeout, ...)
      results$images <- image_results
      
      # Check for containers if image exists
      if (image_results$exists) {
        cli::cli_h2("Container Status")
        container_results <- health_docker_containers(app_dir, expected_image_name, verbose, timeout, ...)
        results$containers <- container_results
      }
    }
  }
  
  # Summarize diagnostics
  cli::cli_h2("Containerization Summary")
  
  # Get all issues and recommendations
  diagnosis <- get_app_issues(results)
  issues <- diagnosis$issues
  recommendations <- diagnosis$recommendations
  
  # Display summary
  if (length(issues) == 0) {
    cli::cli_alert_success("App is ready for containerization")
  } else {
    cli::cli_alert_warning("Found {length(issues)} issue(s):")
    cli::cli_ol(issues)
    
    cli::cli_alert_info("Recommendations:")
    cli::cli_ol(recommendations)
  }
  
  # Return results
  results$issues <- issues
  results$recommendations <- recommendations
  
  invisible(results)
}
