% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_figure_helpers.R
\name{custom_plot_theme}
\alias{custom_plot_theme}
\title{Custom plot theme}
\usage{
custom_plot_theme()
}
\value{
A list with ggplot2 theme layers
}
\description{
A custom \code{ggplot2} theme to be used in the application.
}
\examples{
\dontrun{
library(ggplot2)
ggplot(mtcars, aes(mpg, cyl)) + 
  geom_point() + 
  custom_plot_theme()
}
}
\keyword{internal}
