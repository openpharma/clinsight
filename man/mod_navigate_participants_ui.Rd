% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_navigate_participants.R
\name{mod_navigate_participants_ui}
\alias{mod_navigate_participants_ui}
\title{Navigate participants - Shiny module UI}
\usage{
mod_navigate_participants_ui(id)
}
\arguments{
\item{id}{Character string, used to connect the module UI with the module Server.}
}
\description{
A \code{shiny} module. Used to show participant information in a
\code{\link[bslib:value_box]{bslib::value_box()}}. By clicking on the \code{\link[bslib:value_box]{bslib::value_box()}}, additional
participant information will be shown, as well as a selection menu to select
a different subject. Once the subject is changed, the active \code{subject_id} will
be changed in the application.
}
\seealso{
\code{\link[=mod_navigate_participants_server]{mod_navigate_participants_server()}} for the server function.
}
