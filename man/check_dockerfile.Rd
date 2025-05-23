% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docker-core.R
\name{check_dockerfile}
\alias{check_dockerfile}
\title{Check if a Dockerfile exists in the specified directory}
\usage{
check_dockerfile(app_dir)
}
\arguments{
\item{app_dir}{Character. Path to the application directory.}
}
\value{
Invisibly returns TRUE if exists, otherwise aborts with an error message.
}
\description{
Check if a Dockerfile exists in the specified directory
}
\keyword{internal}
