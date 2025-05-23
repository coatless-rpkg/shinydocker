% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sitrep.R
\name{health_docker_image}
\alias{health_docker_image}
\title{Report Docker image health for a Shiny app}
\usage{
health_docker_image(app_dir, verbose = FALSE, timeout = 30, ...)
}
\arguments{
\item{app_dir}{Character. Path to the Shiny application directory}

\item{verbose}{Logical. If TRUE, provide more detailed output}

\item{timeout}{Numeric. Timeout in seconds for commands}

\item{...}{Additional arguments passed to processx}
}
\value{
List with Docker image details
}
\description{
Report Docker image health for a Shiny app
}
\keyword{internal}
