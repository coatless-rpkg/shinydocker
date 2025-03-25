#' Detect the type of Shiny application (R or Python)
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' 
#' @return Character. Either "r" or "python".
#' 
#' @keywords internal
detect_app_type <- function(app_dir) {
  # Look for R files
  r_files <- list.files(app_dir, pattern = "\\.R$|\\.r$", recursive = TRUE)
  
  # Look for Python files
  py_files <- list.files(app_dir, pattern = "\\.py$", recursive = TRUE)
  
  # Check for Python's app.py or server.py
  has_py_app <- any(grepl("app\\.py$|server\\.py$", py_files))
  
  # Check for R's app.R, server.R, or ui.R
  has_r_app <- any(grepl("app\\.R$|server\\.R$|ui\\.R$", r_files, ignore.case = TRUE))
  
  if (has_py_app && !has_r_app) {
    return("python")
  } else if (has_r_app || length(r_files) > 0) {
    return("r")
  } else if (length(py_files) > 0) {
    return("python")
  } else {
    # If we can't determine, default to R
    warning("Could not determine app type, defaulting to R. Specify app_type manually if needed.")
    return("r")
  }
}

#' Check if directory contains Shiny app files
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' 
#' @return Logical. TRUE if the directory contains valid Shiny app files.
#' 
#' @keywords internal
has_shiny_app_files <- function(app_dir) {
  # Check for R Shiny app files
  has_r_app <- file.exists(file.path(app_dir, "app.R")) ||
    (file.exists(file.path(app_dir, "ui.R")) && 
       file.exists(file.path(app_dir, "server.R")))
  
  # Check for Python Shiny app files
  has_py_app <- file.exists(file.path(app_dir, "app.py")) ||
    (file.exists(file.path(app_dir, "ui.py")) && 
       file.exists(file.path(app_dir, "server.py")))
  
  return(has_r_app || has_py_app)
}

#' Check if directory contains valid Shiny app files and abort if not
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' 
#' @return Invisibly returns TRUE if valid, otherwise aborts with an error message.
#' 
#' @keywords internal
check_shiny_app_files <- function(app_dir) {
  if (!has_shiny_app_files(app_dir)) {
    cli::cli_abort(c(
      "Directory does not appear to contain a valid Shiny application.",
      "i" = "Expected files for R apps: {.file app.R}, or {.file ui.R} and {.file server.R}",
      "i" = "Expected files for Python apps: {.file app.py}, or {.file ui.py} and {.file server.py}"
    ))
  }
  
  invisible(TRUE)
}

#' Detect dependencies for a Shiny application
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' @param app_type Character. Either "r" or "python".
#' 
#' @return Character vector with detected dependencies.
#' 
#' @keywords internal
detect_dependencies <- function(app_dir, app_type) {
  if (app_type == "r") {
    return(detect_r_dependencies(app_dir))
  } else if (app_type == "python") {
    return(detect_python_dependencies(app_dir))
  }
}
#' Extract package names from library() and require() calls
#' 
#' @param r_code Character vector. R code to analyze.
#' 
#' @return Character vector of package names.
#' 
#' @keywords internal
extract_library_packages <- function(r_code) {
  lib_pattern <- "library\\(([^,)]+)\\)|require\\(([^,)]+)\\)"
  lib_matches <- gregexpr(lib_pattern, r_code, perl = TRUE)
  
  packages <- character(0)
  for (i in seq_along(r_code)) {
    if (lib_matches[[i]][1] != -1) {
      matches <- regmatches(r_code[i], lib_matches[[i]])
      for (match in matches) {
        # Extract the package name from library()/require() call
        pkg_name <- gsub("library\\(([^,)]+)\\)|require\\(([^,)]+)\\)", "\\1\\2", match, perl = TRUE)
        # Remove quotes if present
        pkg_name <- gsub("^['\"]|['\"]$", "", pkg_name)
        packages <- c(packages, pkg_name)
      }
    }
  }
  
  return(packages)
}

#' Extract package names from namespace calls (package::function)
#' 
#' @param r_code Character vector. R code to analyze.
#' 
#' @return Character vector of package names.
#' 
#' @keywords internal
extract_namespace_packages <- function(r_code) {
  pkg_pattern <- "([[:alnum:].]+)::"
  pkg_matches <- gregexpr(pkg_pattern, r_code, perl = TRUE)
  
  packages <- character(0)
  for (i in seq_along(r_code)) {
    if (pkg_matches[[i]][1] != -1) {
      matches <- regmatches(r_code[i], pkg_matches[[i]])
      for (match in matches) {
        # Extract the package name from package:: call
        pkg_name <- gsub("([[:alnum:].]+)::", "\\1", match, perl = TRUE)
        packages <- c(packages, pkg_name)
      }
    }
  }
  
  return(packages)
}

#' Detect R package dependencies
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' 
#' @return Character vector of detected R package dependencies.
#' 
#' @keywords internal
detect_r_dependencies <- function(app_dir) {
  # Look for R files
  r_files <- list.files(app_dir, pattern = "\\.R$|\\.r$", full.names = TRUE, recursive = TRUE)
  
  if (length(r_files) == 0) {
    warning("No R files found in the application directory.")
    return(c("shiny"))
  }
  
  # Read all R files
  r_code <- unlist(lapply(r_files, readLines))
  
  # Extract packages from different patterns
  library_packages <- extract_library_packages(r_code)
  namespace_packages <- extract_namespace_packages(r_code)
  
  # Combine and ensure shiny is included
  packages <- unique(c("shiny", library_packages, namespace_packages))
  return(sort(packages))
}

#' Detect Python package dependencies
#' 
#' @param app_dir Character. Path to the Shiny application directory.
#' 
#' @return Character vector of detected Python package dependencies.
#' 
#' @keywords internal
detect_python_dependencies <- function(app_dir) {
  # Look for requirements.txt
  req_file <- file.path(app_dir, "requirements.txt")
  
  if (file.exists(req_file)) {
    # Read requirements.txt
    packages <- readLines(req_file)
    # Remove comments and empty lines
    packages <- packages[!grepl("^\\s*#", packages) & nzchar(trimws(packages))]
    # Remove version specifications
    packages <- gsub("([^<>=~!]+)[<>=~!].*", "\\1", packages)
    # Trim whitespace
    packages <- trimws(packages)
    return(packages)
  }
  
  # If no requirements.txt, look for import statements in Python files
  py_files <- list.files(app_dir, pattern = "\\.py$", full.names = TRUE, recursive = TRUE)
  
  if (length(py_files) == 0) {
    warning("No Python files found in the application directory.")
    return(c("shiny"))
  }
  
  # Read all Python files
  py_code <- unlist(lapply(py_files, readLines))
  
  # Look for import statements
  import_pattern <- "^\\s*import\\s+([^\\s.]+)|^\\s*from\\s+([^\\s.]+)"
  import_matches <- gregexpr(import_pattern, py_code, perl = TRUE)
  
  # Extract package names
  packages <- character(0)
  for (i in seq_along(py_code)) {
    if (import_matches[[i]][1] != -1) {
      matches <- regmatches(py_code[i], import_matches[[i]])
      for (match in matches) {
        # Extract the package name from import or from statement
        pkg_name <- gsub("^\\s*import\\s+([^\\s.]+)|^\\s*from\\s+([^\\s.]+)", "\\1\\2", match, perl = TRUE)
        packages <- c(packages, pkg_name)
      }
    }
  }
  
  # Remove duplicates and sort
  packages <- sort(unique(c("shiny", packages)))
  return(packages)
}