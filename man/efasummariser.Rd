% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efasummariser.R
\name{efasummariser}
\alias{efasummariser}
\title{efasummariser}
\usage{
efasummariser(threshold = 0, ...)
}
\arguments{
\item{threshold}{The threshold cutoff for showing factor loadings.}

\item{...}{Dataframe of EFA loadings and communalities from all input EFAs}
}
\value{
A dataframe of EFA loadings and communalities with loadings lower than the threshold hidden
}
\description{
This function produces a dataframe with loadings and communalities for EFA produced using the psych package.
It can handle multiple EFA results from different samples, binding them together.
This function should be used either with a single EFA or with EFAs on the same variables but with different samples.
}
\examples{
setosa <- psych::fa(iris \%>\%
  filter(Species == "setosa") \%>\%
  select_if(is.numeric),
fm = "gls"
)

versicolor <- psych::fa(iris \%>\%
  filter(Species == "versicolor") \%>\%
  select_if(is.numeric),
fm = "gls"
)

efa_loadings(threshold = 0.3, setosa, versicolor)
}
