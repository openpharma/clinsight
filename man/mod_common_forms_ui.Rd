% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_common_forms.R
\name{mod_common_forms_ui}
\alias{mod_common_forms_ui}
\title{Common forms - Shiny module UI}
\usage{
mod_common_forms_ui(id, form)
}
\arguments{
\item{id}{Character string, used to connect the module UI with the module
Server.}

\item{form}{A character string with the name of the form to display.}
}
\description{
Shiny module. Used to display common form data in the dedicated tab.
}
\details{
The module displays tables of common form data in a wide format. Included
common forms are currently: \verb{Adverse events}, \verb{Medical History},
\code{Medication}, and \verb{Conc. Procedures}. The applicable forms can be flexibly
changed in the metadata. The tables shown are overview tables in wide format,
similar to the ones in \code{\link[=mod_study_forms_server]{mod_study_forms_server()}}. When the common form
\verb{Adverse events} is selected, the module will show an additional table with
Severe Adverse Events above the table with Adverse Events. In addition, it
will show a timeline by calling module
\code{\link[=mod_timeline_ui]{mod_timeline_ui()}}/\code{\link[=mod_timeline_server]{mod_timeline_server()}}. The timeline shows study events
(such as drug administrations) and study visits together with Adverse Events,
so that temporal relationships between these events can be quickly revealed.
The \verb{common forms} module is used in the main server to create all applicable
common form pages.
}
\seealso{
\code{\link[=mod_common_forms_server]{mod_common_forms_server()}}
}
