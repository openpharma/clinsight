% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_test_clinsight.R
\name{test_clinsight}
\alias{test_clinsight}
\title{Test ClinSight}
\usage{
test_clinsight(
  clinsight_data = clinsightful_data,
  meta_data = metadata,
  clinsight_config = "test"
)
}
\arguments{
\item{clinsight_data}{A data frame with (ClinSight compatible) study data. If
not provided, the example study data in the ClinSight package will be used.}

\item{meta_data}{A list of data frames with (ClinSight compatible) metadata.
If not provided, the example metadata in the ClinSight package will be used}

\item{clinsight_config}{A character vector with the ClinSight configuration
to use. The standard settings should be sufficient for most testing cases.}
}
\value{
Runs the ClinSight Shiny application.
}
\description{
Provides a wrapper around \code{\link[=run_app]{run_app()}}. Unlike the function \code{\link[=run_app]{run_app()}}, this
function stores data in a temporary folder and cleans up after itself after
use. Useful during development, when tweaking new study data and metadata for
use with ClinSight. Note that, when using this function, any database files
needed for ClinSight will be renewed and any file path specified in the
\code{config.yml} file will be ignored.
}
\details{
This function is only meant for testing and development purposes. For use in
production, instead use the function \code{\link[=run_app]{run_app()}} directly.
}
