#' Check if a port is available on localhost
#' 
#' @param port Integer. The port to check.
#' 
#' @return Logical. TRUE if the port is available, FALSE if already in use.
#' 
#' @keywords internal
is_port_available <- function(port) {
  tryCatch({
    serverSocket <- get("serverSocket", envir = baseenv(), 
                        mode = "function", inherits = FALSE)
    # Try to create a server socket on the port
    con <- serverSocket(port)
    close(con)
    TRUE  # Success means port is available
  }, error = function(e) {
    FALSE  # Error means port is in use
  })
}

#' Find an available port starting from a given port
#' 
#' @param start_port Integer. The port to start checking from.
#' @param max_tries Integer. Maximum number of ports to check. Default: 100.
#' 
#' @return Integer. An available port or NULL if no port found.
#' 
#' @keywords internal
find_available_port <- function(start_port, max_tries = 100) {
  for (offset in 0:(max_tries - 1)) {
    port <- start_port + offset
    if (is_port_available(port)) {
      return(port)
    }
  }
  return(NULL) # No available port found
}

#' Handle port configuration and availability
#' 
#' @param port Integer. Requested port.
#' @param force_port Logical. Whether to enforce the requested port.
#' 
#' @return Integer. The port to use, either the requested one or an alternative.
#' 
#' @keywords internal
handle_port_configuration <- function(port, force_port = FALSE) {
  if (!is_port_available(port)) {
    if (force_port) {
      cli::cli_abort(c(
        "Port {.val {port}} is already in use",
        "i" = "Specify a different port or set force_port=FALSE to auto-select an available port."
      ))
    }
    
    cli::cli_alert_warning("Port {.val {port}} is already in use")
    
    # Try to find an alternative port with more attempts
    alternative_port <- find_available_port(port + 1, max_tries = 100)
    
    if (is.null(alternative_port)) {
      cli::cli_abort(c(
        "Port {.val {port}} is already in use and no alternative ports available in range {.val {port+1}} to {.val {port+100}}",
        "i" = "Please specify a different port range or close applications using these ports."
      ))
    }
    
    cli::cli_alert_info("Using alternative port {.val {alternative_port}} instead")
    return(alternative_port)
  }
  
  return(port)
}

#' Generate app URL based on app type and port
#' 
#' @param app_type Character. Either "r" or "python".
#' @param port Integer. Port number.
#' 
#' @return Character. URL to access the app.
#' 
#' @keywords internal
generate_app_url <- function(app_type, port) {
  # For R apps, Shiny Server serves the app at /app/
  # For Python apps, the app is served at the root
  if (app_type == "r") {
    return(paste0("http://localhost:", port, "/app/"))
  } else {
    return(paste0("http://localhost:", port))
  }
}