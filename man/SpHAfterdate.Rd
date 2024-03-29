% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specieshindex.R
\name{SpHAfterdate}
\alias{SpHAfterdate}
\title{Species h-index with a given time frame}
\usage{
SpHAfterdate(data, date)
}
\arguments{
\item{data}{The dataframe generated from \code{\link{Fetch}}.}

\item{date}{The lower limit of the timeframe.}
}
\value{
H-index of the given time period.
}
\description{
This function calculates the h-index using a given date up till the newest record.
}
\examples{
\dontrun{
Woylie <- Fetch(db = "scopus",
                search = "tak",
                genus = "Bettongia",
                species = "penicillata")
}
SpHAfterdate(Woylie, "2000-01-01")

}
