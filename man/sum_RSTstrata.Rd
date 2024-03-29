% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum_RSTstrata.R
\name{sum_RSTstrata}
\alias{sum_RSTstrata}
\title{sum_RSTstrata:}
\usage{
sum_RSTstrata(
  data,
  season = c("Spring", "Summer", "Fall"),
  species = c("Chinook", "Steelhead"),
  strata_dates = NULL
)
}
\arguments{
\item{data}{Prepared P4 RST data from get_RSTData() %>% clean_RSTData() and filtered for a single RST.}

\item{season}{Season desired for strata determination.}

\item{strata_dates}{Vector of dates (YYYY-MM-DD)input by user to establish
strata.  If NULL, function will automatically apply one-week strata.}
}
\description{
Takes prepared RST data and creates a plot to help determine
strata through a visualization, returning the strata dates, and returns a
dataframe to be fed to gauss estimates.
}
\examples{
p4_raw <- get_P4Data(EventSite = 'IMNTRP', MigrationYear = 2020)
p4_clean <- clean_P4Data(p4_raw)
strata <- sum_RSTstrata(p4_clean, 'Spring', 'Chinook', NULL)
}
\author{
Tyler T. Stright
}
