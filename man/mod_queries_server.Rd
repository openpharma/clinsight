% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_queries.R
\name{mod_queries_server}
\alias{mod_queries_server}
\title{Queries - Shiny module Server}
\usage{
mod_queries_server(id, r, navinfo, all_forms, db_path, table_names)
}
\arguments{
\item{id}{Character string, used to connect the module UI with the module
Server.}

\item{r}{Common reactive values. Used to access the data frame \code{query_data},
and the reviewers' \code{user_name}. User name is needed for creating a
follow-up to an existing query.}

\item{navinfo}{Common reactive values. Used to send back the request for the
change in form/subject id back to the server. Contains the character strings
\code{active_form}, \code{active_tab}, and \code{trigger_page_change}. The module will create
these variables if they are not yet available in \code{navinfo}.}

\item{all_forms}{A data frame containing two columns: a column named "form" with
all the form to navigate through in the correct order, and a column named
"main_tab", showing the name of the main_tab in which the form can be found.}

\item{db_path}{A character string with the path to the database.}

\item{table_names}{An optional character vector. If provided, will be used
within \code{\link[=datatable_custom]{datatable_custom()}}, to improve the column names in the final
interactive tables.}
}
\description{
A Shiny module. Used to display all raised queries.
}
\details{
The module displays all raised queries in a table. Upon clicking on a query,
the query follow-up messages will be shown. In addition, it will be possible
to write a follow-up message. The logic for writing the follow-up message is
extracted in a different module, named \code{\link[=mod_query_follow_up_server]{mod_query_follow_up_server()}}. New
queries are created in the module \code{\link[=mod_query_add_server]{mod_query_add_server()}}. The button to
initiate this module can be found in the main sidebar
\code{\link[=mod_main_sidebar_server]{mod_main_sidebar_server()}}.
}
\seealso{
\code{\link[=mod_queries_ui]{mod_queries_ui()}}, \code{\link[=mod_go_to_form_ui]{mod_go_to_form_ui()}},
\code{\link[=mod_go_to_form_server]{mod_go_to_form_server()}}, \code{\link[=mod_query_follow_up_ui]{mod_query_follow_up_ui()}},
\code{\link[=mod_query_follow_up_server]{mod_query_follow_up_server()}}, \code{\link[=mod_query_add_ui]{mod_query_add_ui()}},
\code{\link[=mod_query_add_server]{mod_query_add_server()}}.
}
