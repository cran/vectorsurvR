% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAbundance.R
\name{getAbundance}
\alias{getAbundance}
\title{Calculate abundance}
\usage{
getAbundance(
  collections,
  interval,
  agency = NULL,
  species = NULL,
  trap = NULL,
  sex = "female",
  trapnight_min = 1,
  trapnight_max = NULL,
  separate_by = NULL
)
}
\arguments{
\item{collections}{Collections data retrieved from getArthroCollections()}

\item{interval}{Calculation interval for abundance, accepts "CollectionDate",“Week”, “Biweek”, or “Month.}

\item{agency}{An optional vector for filtering agency by character code}

\item{species}{An optional vector for filtering species. Species_display_name is the accepted notation.To see a list of species present in your data run unique(collections$species_display_name). If species is unspecified, the default NULL will return data for all species in data.}

\item{trap}{An optional vector for filtering trap type by acronym. Trap_acronym is the is the accepted notation. Run unique(collections$trap_acronym) to see trap types present in your data. If trap is unspecified, the default NULL will return data for all trap types.}

\item{sex}{An optional vector for filtering sex type. Accepts 'male', 'female',or 'other'. If sex is unspecified, the default NULL will return data for female sex.}

\item{trapnight_min}{Minimum trap night restriction for calculation. Default is 1.}

\item{trapnight_max}{Maximum trap night restriction for calculation. Default is no restriction.}

\item{separate_by}{Separate/group the calculation by 'trap','species', 'agency', 'county', or 'spatial_feature'. Default NULL does not separate.}
}
\value{
A dataframe of abundance calculations.
}
\description{
Calculates abundance
}
\examples{
getAbundance(sample_collections,
             interval = 'Week',
             species = list('Cx pipiens'),
             trap = list('GRVD', 'CO2'),
             sex = list("female"),
             trapnight_min = 1,
             trapnight_max = 5,
             separate_by  = "species")
}
