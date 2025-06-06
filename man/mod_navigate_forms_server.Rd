% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_navigate_forms.R
\name{mod_navigate_forms_server}
\alias{mod_navigate_forms_server}
\title{Navigate forms - Shiny module Server}
\usage{
mod_navigate_forms_server(id, navinfo, forms_to_review, all_forms)
}
\arguments{
\item{id}{Character string, used to connect the module UI with the module Server.}

\item{navinfo}{Reactive values created with \code{\link[shiny:reactiveValues]{shiny::reactiveValues()}}. Used
to send back information about the page change to the server.}

\item{forms_to_review}{Reactive value. Contains a character vector with the forms
that need review for the active/selected ID. Will be used to change the font
face of the form name to bold if review of that form is needed.}

\item{all_forms}{A data frame containing two columns: a column named "form" with
all the form to navigate through in the correct order, and a column named
"main_tab", showing the name of the main_tab in which the form can be found.}
}
\description{
Shiny server module to navigate between forms. Navigation is done with a
\code{back} and \code{forward} button in the UI module.
}
\seealso{
\code{\link[=mod_navigate_forms_ui]{mod_navigate_forms_ui()}}
}
