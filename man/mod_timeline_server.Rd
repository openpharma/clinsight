% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_timeline.R
\name{mod_timeline_server}
\alias{mod_timeline_server}
\title{Interactive timeline - Shiny module Server}
\usage{
mod_timeline_server(id, form_review_data, timeline_data, active_subject)
}
\arguments{
\item{id}{Character string, used to connect the module UI with the module
Server.}

\item{form_review_data}{A reactive value containing the review data of the
respective form.}

\item{timeline_data}{A reactive with a data frame containing the timeline
data. Used to create the timeline figure. Created with
\code{\link[=get_timeline_data]{get_timeline_data()}}.}

\item{active_subject}{A reactive value containing the active subject ID.}
}
\description{
A shiny module. Creates an interactive timeline. Used to provide a quick
overview of adverse events, severe adverse events, and study activities such
as Investigational Product administration. Helpful to judge whether for
example an event is related to an Investigational Product.
}
\seealso{
\code{\link[=mod_timeline_ui]{mod_timeline_ui()}}, \code{\link[=mod_common_forms_ui]{mod_common_forms_ui()}},
\code{\link[=mod_common_forms_server]{mod_common_forms_server()}}
}
