% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_utils.R
\name{cols_to_char}
\alias{cols_to_char}
\title{Convert non-numeric columns to character}
\usage{
cols_to_char(data)
}
\arguments{
\item{data}{A data frame with columns that need to be converted.}
}
\value{
A data frame with all non-numeric columns converted to character.
}
\description{
Convert non-numeric columns to character
}
\examples{
\dontrun{
df <- cols_to_char(iris)
class(df$Species)
}
}
\keyword{internal}
