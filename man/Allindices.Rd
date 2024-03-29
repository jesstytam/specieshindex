% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specieshindex.R
\name{Allindices}
\alias{Allindices}
\title{Index summary}
\usage{
Allindices(data, genus, species)
}
\arguments{
\item{data}{The dataframe generated from \code{\link{Fetch}}.}

\item{genus}{Genus classification from the binomial name.}

\item{species}{Species classification from the binomial name.}
}
\value{
A datarame of all of the indices in the package.
}
\description{
This function returns a dataframe of the summary of all of the indices.
}
\examples{
\dontrun{
Woylie <- Fetch(db = "scopus",
                search = "tak",
                genus = "Bettongia",
                species = "penicillata")
}
Allindices(Woylie,
           genus = "Bettongia",
           species = "penicillata")

}
