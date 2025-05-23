% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_common_forms.R
\name{mod_common_forms_server}
\alias{mod_common_forms_server}
\title{Common forms - Shiny module Server}
\usage{
mod_common_forms_server(
  id,
  form,
  form_data,
  form_review_data,
  form_items,
  active_subject,
  id_item = c("subject_id", "event_name", "item_group", "form_repeat", "item_name"),
  table_names = NULL,
  timeline_data
)
}
\arguments{
\item{id}{Character string, used to connect the module UI with the module
Server.}

\item{form}{A character string with the name of the form to display.}

\item{form_data}{A reactive value containing the study data of the respective
form.}

\item{form_review_data}{A reactive value containing the review data of the
respective form.}

\item{form_items}{A named character vector with the names of the expected
variables defined in the applicable \code{form}. Used in the UI to create a
filter with drop-down menu, to select the desired variables in a figure.
Used in the module Server to make sure that all expected columns are always
created, even if some variables are implicitly missing (which might occur
if there are not yet any values available for a specific variable). Also,
implicitly missing variables might give errors if part of the script relies
on the variables' presence. See also the parameter \code{expected_columns} in
\code{\link[=create_table.default]{create_table.default()}}.}

\item{active_subject}{A reactive value containing the active subject ID.}

\item{id_item}{Character vector containing the column names of the columns
that can uniquely identify one item/row.}

\item{table_names}{An optional character vector. If provided, will be used
within \code{\link[=datatable_custom]{datatable_custom()}}, to improve the column names in the final
interactive tables.}

\item{timeline_data}{A reactive with a data frame containing the timeline
data. Used to create the timeline figure. Created with
\code{\link[=get_timeline_data]{get_timeline_data()}}.}
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
\code{\link[=mod_common_forms_ui]{mod_common_forms_ui()}}, \code{\link[=mod_timeline_ui]{mod_timeline_ui()}},
\code{\link[=mod_timeline_server]{mod_timeline_server()}}, \code{\link[=mod_review_form_tbl_ui]{mod_review_form_tbl_ui()}},
\code{\link[=mod_review_form_tbl_server]{mod_review_form_tbl_server()}}
}
