% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_utils.R
\name{get_unique_vars}
\alias{get_unique_vars}
\title{Get unique vars}
\usage{
get_unique_vars(data, var)
}
\arguments{
\item{data}{Either a list of data frames or a data frame.}

\item{var}{Column names to extract the unique variables from}
}
\value{
A data frame with unique values.
}
\description{
Get all unique values from variables that are available within a data frame
or a list of dataframes.
}
\examples{
\dontrun{
 list_with_data <- list(
  data.frame(
    "Sites" = c("site_1", "site_3", "site_4"),
    "ID" = c("subj_1", "subj_5", "subj_8")
    ),
 data.frame(
    "Sites" = c("site_1", "site_3", "site_9"),
    "ID" = c("subj_7", "subj_9", NA_character_)
    )
 )

 get_unique_vars(list_with_data, c("Sites", "ID"))
}

}
\keyword{internal}
