% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data_customize_events.R
\name{add_events_to_data}
\alias{add_events_to_data}
\title{Merge event data from study data with metadata}
\usage{
add_events_to_data(data, events)
}
\arguments{
\item{data}{A data frame with study data. Should contain  columns \code{event_id}
and \code{vis_num} column.}

\item{events}{A data frame with events metadata, created with
\code{\link[=get_metadata]{get_metadata()}}.}
}
\value{
A data frame with clean event data, with event_label column as a
factor with correct event_label levels.
}
\description{
Collects all available events. Defines the correct order of events, and
creates event labels if needed.
}
\keyword{internal}
