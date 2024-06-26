% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProcessAbunAnom.R
\name{processAbunAnom}
\alias{processAbunAnom}
\title{process Abundance Anomaly}
\usage{
processAbunAnom(AbAnomOutput)
}
\arguments{
\item{AbAnomOutput}{output from `getAbunAnom()`}
}
\value{
Abundance anomaly output processed into long form
}
\description{
`processAbunAnom()` processes the output returned from `getAbundanceAnomaly()` into a long form suitable for plotting in 'ggplot'
}
\examples{
AbAnOut = getAbundanceAnomaly(sample_collections,
                             interval = "Biweek",
                             target_year = 2020,
                             species_list = c("Cx tarsalis", "Cx pipiens"),
                             species_separate = TRUE)
AbAnOut_L = processAbunAnom(AbAnOut)
}
