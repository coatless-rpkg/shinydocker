% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/container.R
\name{run_with_docker}
\alias{run_with_docker}
\title{Run container using docker run}
\usage{
run_with_docker(
  tag,
  port,
  app_type,
  env_vars = NULL,
  detach = FALSE,
  quiet = FALSE,
  ...
)
}
\arguments{
\item{tag}{Character. The Docker image tag.}

\item{port}{Integer. The port to map.}

\item{app_type}{Character. Either "r" or "python".}

\item{env_vars}{Named character vector. Environment variables.}

\item{detach}{Logical. Whether to run in detached mode.}

\item{quiet}{Logical. Whether to suppress output.}

\item{...}{Additional arguments passed to processx.}
}
\value{
Character. Container ID if successful.
}
\description{
Run container using docker run
}
\keyword{internal}
