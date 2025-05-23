% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_appdata.R
\name{get_raw_csv_data}
\alias{get_raw_csv_data}
\title{Get raw data from CSV files}
\usage{
get_raw_csv_data(
  data_path = Sys.getenv("RAW_DATA_PATH"),
  synch_time = time_stamp(),
  exclude = c("README.csv$", "Pending_forms.csv$", "MEDRA.csv$", "WHODrug.csv$",
    "_Queries.csv$"),
  delim = ",",
  skip = 1
)
}
\arguments{
\item{data_path}{Path to the folder that contains the CSV files with the
application data.}

\item{synch_time}{Time at which the data was extracted from the EDC system.
Defaults to the current date time. Important to set this correctly, since
it will be shown in the application. By default, a warning will be given in
the application if the synchronization time is more than one day old.}

\item{exclude}{character vector with regular expressions that identify csv
files that should be excluded from the study data. Useful to exclude files
with different data structures, or files with metadata.}

\item{delim}{Delimiter to use to read in files.}

\item{skip}{Number of rows to skip when reading in files.}
}
\value{
A data frame with raw application data.
}
\description{
Get raw data from CSV files
}
