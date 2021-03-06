% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mis-indicators.R
\name{setup_mis}
\alias{setup_mis}
\title{Setup of the MIS data}
\usage{
setup_mis(download_data)
}
\arguments{
\item{download_data}{logical; should the data be downloaded via the Eurostat
API? If not, data will be retrieved from a local file. If the file does
not exist, the function exits with an error.}
}
\description{
Prepares the main and auxiliary indicators of the EU Macroeconomic
 Imbalance Scoreboard (MIS). Downloads data from Eurostat and saves them
 as a labelled `.rds` file. For labeling, the function `Hmisc::label()` is
 used.
}
\details{
Sometimes, for the same indicators, multiple values are reported by
 Eurostat. The values correspond to different publications of the
 Statistical Annex of the imbalance procedure. This function searches for
 the resulting duplicates and uses only the most recent value published.
}
