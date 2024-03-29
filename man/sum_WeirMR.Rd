% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum_WeirMR.R
\name{sum_WeirMR}
\alias{sum_WeirMR}
\title{Mark-Recapture Estimates}
\usage{
sum_WeirMR(
  weir_data,
  carcass_data = NULL,
  species = c("Chinook", "Steelhead"),
  alpha = 0.05,
  ...
)
}
\arguments{
\item{weir_data}{cleaned FINS weir/trapping query}

\item{carcass_data}{cleaned CDMS carcass dataset}

\item{species}{Chinook or Steelhead}

\item{alpha}{type I error rate.  Default is set at 0.05 to produce 95\%
confidence intervals.}
}
\description{
Summarize mark-recapture data from a FINS database trapping query and CDMS spawning ground survey carcass dataset. Steelhead mark-recapture summaries only require the trapping data while Chinook also needs carcass data.
}
\examples{
get_WeirMR(weir_data)
}
\author{
Ryan N. Kinzer
}
