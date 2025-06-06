% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data_helpers.R
\name{get_base_value}
\alias{get_base_value}
\title{Get base value}
\usage{
get_base_value(
  data,
  var = c(item_name = "Weight"),
  event = c(event_name = "Screening"),
  value_column = "item_value",
  id_column = "subject_id"
)
}
\arguments{
\item{data}{data frame in long format.}

\item{var}{A named character vector with the variable name to use.
The name of the character vector should match the column in the data frame with the variable names.}

\item{event}{A names character vector with the event that identifies the baseline value.
The name of the character vector should match the column in the data frame with the event names.}

\item{value_column}{A string containing the column name with the item values.}

\item{id_column}{The columns identifying a unique participant (subject_id).}
}
\value{
as data frame with an additional column named "base_\code{varname}".
}
\description{
Adds base value to a long-format data frame.
}
\examples{
 library(dplyr)
 library(tidyr)
 # create a long time series data frame:
 df <- as.data.frame(EuStockMarkets) |> 
  mutate(time = time(EuStockMarkets)) |>
  pivot_longer(-time)
 output <- get_base_value(df, c("name" = "DAX"), event = c("time" = df$time[1]),
                          value_column = "value", id_column = NULL)

}
