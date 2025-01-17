% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAbundance.R
\name{getAbundance}
\alias{getAbundance}
\title{Calculate abundance}
\usage{
getAbundance(
  collections,
  interval,
  species = NULL,
  trap = NULL,
  sex = "female",
  separate_by = NULL
)
}
\arguments{
\item{collections}{Collections data retrieved from getArthroCollections()}

\item{interval}{Calculation interval for abundance, accepts “collection_date”,“Biweek”,“Week”, and “Month.}

\item{species}{An optional vector for filtering species. Species_display_name is the accepted notation.To see a list of species present in your data run unique(collections$species_display_name). If species is unspecified, the default NULL will return data for all species in data.}

\item{trap}{An optional vector for filtering trap type by acronym. Trap_acronym is the is the accepted notation. Run unique(collections$trap_acronym) to see trap types present in your data. If trap is unspecified, the default NULL will return data for all trap types.}

\item{sex}{An optional vector for filtering sex type. Accepts 'male', 'female',or 'other'. If sex is unspecified, the default NULL will return data for female sex.}

\item{separate_by}{Separate/group the calculation by 'trap','species' or 'agency'. Default NULL does not separate.}
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
             separate_by  = "species")
}
