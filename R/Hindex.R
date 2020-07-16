#' This function counts the total number of search results.
#' It counts only the publications with the binomial name in their title.
#' 
#' @title Search count - title only
#'
#' @param genus 
#' @param species 
#' @param APIkey 
#' @param datatype 
#'
#' @return
#' @export
#'
#' @examples
searchCountT <- function(genus, species, APIkey, datatype = "application/xml") {
  library(httr)
  library(XML)
  theURL <- GET("http://api.elsevier.com/content/search/scopus",
                query = list(apiKey = paste0(APIkey),
                             query = paste0("TITLE(\"",genus," ",species,"\") AND DOCTYPE(ar OR re)"),
                             httpAccept = "application/xml")) 
  stop_for_status(theURL) 
  theData <- content(theURL, as = "text") 
  newData <- xmlParse(theURL)
  resultCount <- as.numeric(xpathSApply(newData,"//opensearch:totalResults", xmlValue)) 
  return(resultCount)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title, abstract and keywords.
#'
#' @title Search count - title, abstract and keywords
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param APIkey Scopus API key needed to access and download data from their database.
#' @param datatype Formats the URL to be sent to the API. The default is "application/xml".
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export 
#'
#' @examples
#' searchCount("Bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' searchCount("bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
searchCountTAK <- function(genus, species, APIkey, datatype = "application/xml") {
  library(httr)
  library(XML)
  theURL <- GET("http://api.elsevier.com/content/search/scopus",
                query = list(apiKey = paste0(APIkey),
                             query = paste0("TITLE-ABS-KEY(\"",genus," ",species,"\") AND DOCTYPE(ar OR re)"),
                             httpAccept = "application/xml")) 
  stop_for_status(theURL) 
  theData <- content(theURL, as = "text") 
  newData <- xmlParse(theURL) 
  resultCount <- as.numeric(xpathSApply(newData,"//opensearch:totalResults", xmlValue)) 
  return(resultCount)
}



#' Extract citation data from Scopus.
#'
#' @title Extract content
#'
#' @param search.string Search string with Boolean operators or Scopus advanced search.
#' @param datatype Formats the URL to be sent to the API. The default is "application/xml".
#'
#' @return A list of entries of the search from Scopus.
#' @export 
#'
#' @examples
#' extractcontent("TITLE-ABS-KEY("bettongia penicillata") AND DOCTYPE(ar OR re)")
#' 
extractcontent<- function(search.string, datatype = "application/xml") {
  library(httr)
  library(XML)
  key <- "442b9048417ef20cf680a0ae26ee4d86" 
  theURL <- GET("http://api.elsevier.com/content/search/scopus",
                query = list(apiKey = key,
                             query = paste(search.string),
                             httpAccept = "application/xml")) 
  stop_for_status(theURL) 
  theData <- content(theURL, as = "text") 
  return(theData)
}



#' Extract XML list into a dataframe.
#'
#' @title Extract XML
#'
#' @param theFile The file to be converted.
#'
#' @return A converted dataframe generated from \code{\link{extractcontent}}.
#' @export 
#'
#' @examples
#' extractXML(SpeciesXML)
#' 
extractXML <- function(theFile) {
  library(XML)
  newData <- XML::xmlParse(theFile) 
  records <- XML::getNodeSet(newData, "//cto:entry", namespaces = "cto") 
  scopusID <- lapply(records, XML::xpathSApply, "./cto:eid", XML::xmlValue, namespaces = "cto") 
  scopusID[sapply(scopusID, is.list)] <- NA
  scopusID <- unlist(scopusID)
  doi <- lapply(records, XML::xpathSApply, "./prism:doi", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/"))
  doi[sapply(doi, is.list)] <- NA
  doi <- unlist(doi)
  pmid <- lapply(records, XML::xpathSApply, "./cto:pubmed-id", XML::xmlValue, namespaces = "cto") 
  pmid[sapply(pmid, is.list)] <- NA 
  pmid <- unlist(pmid) 
  authLast <- lapply(records, XML::xpathSApply, ".//cto:surname", XML::xmlValue, namespaces = "cto") 
  authLast[sapply(authLast, is.list)] <- NA
  authInit <- lapply(records, XML::xpathSApply, ".//cto:initials", XML::xmlValue, namespaces = "cto")
  authInit[sapply(authInit, is.list)] <- NA
  authors <- mapply(paste, authLast, authInit, collapse = "|")
  authors <- sapply(strsplit(authors, "|", fixed = TRUE), unique)
  authors <- sapply(authors, paste, collapse = "|")
  affiliations <- lapply(records, XML::xpathSApply, ".//cto:affilname", XML::xmlValue, namespaces = "cto")
  affiliations[sapply(affiliations, is.list)] <- NA
  affiliations <- sapply(affiliations, paste, collapse = "|")
  affiliations <- sapply(strsplit(affiliations, "|", fixed = TRUE), unique) 
  affiliations <- sapply(affiliations, paste, collapse = "|")
  countries <- lapply(records, XML::xpathSApply, ".//cto:affiliation-country", XML::xmlValue, namespaces = "cto")
  countries[sapply(countries, is.list)] <- NA
  countries <- sapply(countries, paste, collapse = "|")
  countries <- sapply(strsplit(countries, "|", fixed = TRUE), unique) 
  countries <- sapply(countries, paste, collapse = "|") 
  year <- lapply(records, XML::xpathSApply, "./prism:coverDate", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/"))
  year[sapply(year, is.list)] <- NA
  year <- unlist(year)
  year <- gsub("\\-..", "", year) 
  articletitle <- lapply(records, XML::xpathSApply, "./dc:title", XML::xmlValue, namespaces = c(dc = "http://purl.org/dc/elements/1.1/"))
  articletitle[sapply(articletitle, is.list)] <- NA
  articletitle <- unlist(articletitle)
  journal <- lapply(records, XML::xpathSApply, "./prism:publicationName", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")) 
  journal[sapply(journal, is.list)] <- NA
  journal <- unlist(journal)
  volume <- lapply(records, XML::xpathSApply, "./prism:volume", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/"))
  volume[sapply(volume, is.list)] <- NA
  volume <- unlist(volume)
  issue <- lapply(records, XML::xpathSApply, "./prism:issueIdentifier", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")) 
  issue[sapply(issue, is.list)] <- NA
  issue <- unlist(issue)
  pages <- lapply(records, XML::xpathSApply, "./prism:pageRange", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")) 
  pages[sapply(pages, is.list)] <- NA
  pages <- unlist(pages)
  abstract <- lapply(records, XML::xpathSApply, "./dc:description", XML::xmlValue, namespaces = c(dc = "http://purl.org/dc/elements/1.1/"))
  abstract[sapply(abstract, is.list)] <- NA
  abstract <- unlist(abstract)
  keywords <- lapply(records, XML::xpathSApply, "./cto:authkeywords", XML::xmlValue, namespaces = "cto")
  keywords[sapply(keywords, is.list)] <- NA
  keywords <- unlist(keywords)
  keywords <- gsub(" | ", "|", keywords, fixed = TRUE)
  ptype <- lapply(records, XML::xpathSApply, "./cto:subtypeDescription", XML::xmlValue, namespaces = "cto")
  ptype[sapply(ptype, is.list)] <- NA
  ptype <- unlist(ptype)
  timescited <- lapply(records, XML::xpathSApply, "./cto:citedby-count", XML::xmlValue, namespaces = "cto")
  timescited[sapply(timescited, is.list)] <- NA
  timescited <- unlist(timescited)
  theDF <- data.frame(scopusID, doi, pmid, authors, affiliations, countries, year, articletitle, journal, volume, issue, pages, keywords, abstract, ptype, timescited, stringsAsFactors = FALSE)
  return(theDF)
}



#' This function fetches citation information from Scopus using genus and species name found in the title of the publications.
#' Duplicates are removed after fetching the data.
#'
#' @title Fetch data - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param APIkey Scopus API key needed to access and download data from their database.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @export 
#'
#' @examples
#' FetchSpT("Bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT("bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
FetchSpT <- function(genus, species, APIkey) {
  library(rscopus)
  library(rlang)
  library(taxize)
  library(dplyr)
  if (is_missing(APIkey)) {
    stop("You need to register for an API key on Scopus.") 
  }
  findname <- gnr_resolve(names = c(genus, species)) 
  if (findname$user_supplied_name %in% findname$matched_name) {
    print(paste("Species found on the Encyclopedia of Life, proceeding data extraction."))
  } else {
    stop("Species not found on the Encyclopedia of Life, please check your spelling.") 
  }
  count <- searchCountT(genus, species, APIkey) 
  print(paste(count, "records found."))
  if (count > 1000) {
    print(paste("More than 1000 records found, this will take a while unless you have a nice computer."))
  }
  step_size <- 1000 
  start_record <- 0
  datalist = list()
  looprepeat <- ceiling(count/step_size)
  for (i in 1:looprepeat) { 
    print(paste("starting iteration: ", i, " Note: iteration size is ", step_size, " records, which runs of 200 records inside each iteration."))
    print(paste("Fetching records now."))
    search <- scopus_search(query = paste0("TITLE(\"",genus," ",species,"\") AND DOCTYPE(ar OR re)"),
                            api_key = paste0(APIkey),
                            verbose = TRUE,
                            max_count = step_size,
                            start = start_record,
                            wait_time = 1)
    start_record <- as.numeric(summary(search)[1,1])
    searchdf <- entries_to_citation_df(search$entries)
    datalist[[i]] <- searchdf
    print(paste("Retrieved", start_record, "records."))
  }
  searchcombine <- do.call(rbind, datalist)
  returned <- dim(searchcombine)[1]
  print(paste(returned, "records retrived in total."))
  duplicates <- dim(searchcombine[duplicated(searchcombine$title),])[1] 
  print(paste(duplicates, "duplicates found."))
  if (duplicates>0) { 
    print(paste("Removing duplicated records."))
    searchcombine <- searchcombine[!duplicated(searchcombine$title), ] 
  }
  retrieved <- dim(searchcombine)[1] 
  print(paste(retrieved, "unique records successfully fetched."))
  return(searchcombine)
}



#' This function fetches citation information from Scopus using genus and species name found in the title, abstract and keywords of the publications.
#' Duplicates are removed after fetching the data.
#'
#' @title Fetch data - title, abstract and keywords
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param APIkey Scopus API key needed to access and download data from their database.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @export 
#'
#' @examples
#' FetchSpT("Bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT("bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
FetchSpTAK <- function(genus, species, APIkey) {
  library(rscopus)
  library(rlang)
  library(taxize)
  library(dplyr)
  if (is_missing(APIkey)) {
    stop("You need to register for an API key on Scopus.") 
  }
  findname <- gnr_resolve(names = c(genus, species)) 
  if (findname$user_supplied_name %in% findname$matched_name) {
    print(paste("Species found on the Encyclopedia of Life, proceeding data extraction."))
  } else {
    stop("Species not found on the Encyclopedia of Life, please check your spelling.") 
  }
  count <- searchCountTAK(genus, species, APIkey) 
  print(paste(count, "records found."))
  if (count > 1000) {
    print(paste("More than 1000 records found, this will take a while unless you have a nice computer."))
  }
  step_size <- 1000 
  start_record <- 0
  datalist = list()
  looprepeat <- ceiling(count/step_size) 
  for (i in 1:looprepeat) { 
    print(paste("starting iteration: ", i, " Note: iteration size is ", step_size, " records, which runs of 200 records inside each iteration."))
    print(paste("Fetching records now."))
    search <- scopus_search(query = paste0("TITLE-ABS-KEY(\"",genus," ",species,"\") AND DOCTYPE(ar OR re)"),
                            api_key = paste0(APIkey),
                            verbose = TRUE,
                            max_count = step_size,
                            start = start_record,
                            wait_time = 1)
    start_record <- as.numeric(summary(search)[1,1])
    searchdf <- entries_to_citation_df(search$entries)
    datalist[[i]] <- searchdf
    print(paste("Retrieved", start_record, "records."))
  }
  searchcombine <- do.call(rbind, datalist) 
  returned <- dim(searchcombine)[1]
  print(paste(returned, "records retrived in total."))
  duplicates <- dim(searchcombine[duplicated(searchcombine$title),])[1] 
  print(paste(duplicates, "duplicates found."))
  if (duplicates>0) { 
    print(paste("Removing duplicated records."))
    searchcombine <- searchcombine[!duplicated(searchcombine$title), ] 
  }
  retrieved <- dim(searchcombine)[1]
  print(paste(retrieved, "unique records successfully fetched."))
  return(searchcombine)
}



#' This function calculates the total number of publications.
#'
#' @title Total publications
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return An integer of the total number of publications.
#' @export
#'
#' @examples
#' TotalPub(SpeciesData)
#' 
TotalPub <- function(data) {
  total <- nrow(data) 
  return(total)
}



#' This function calculates the total number of citations.
#'
#' @title Total citations
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return A numerical value of the total number of citations.
#' @export
#'
#' @examples
#' TotalCite(SpeciesData)
#' 
TotalCite <- function(data) {
  data$citations <- as.numeric(data$citations) 
  total <- sum(data$citations)
  return(total)
}



#' This function calculates the total number of journals.
#'
#' @title Total journals
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return An integer of the total number of journals.
#' @export
#'
#' @examples
#' TotalJournals(SpeciesData)
#' 
TotalJournals <- function(data) {
  filter <- unique(data$journal)
  total <- length(filter)
  return(total)
}



#' This function calculates the total number of articles.
#'
#' @title Total Article
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return An integer of the total number of articles.
#' @export
#'
#' @examples
#' TotalArt(SpeciesData)
#' 
TotalArt <- function(data) {
  Article <- sum(data$description == "Article")
  return(Article)
}



#' This function calculates the total number of reviews.
#'
#' @title Total reviews
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return An integer of the total number of reviews.
#' @export
#'
#' @examples
#' TotalRev(SpeciesData)
#' 
TotalRev <- function(data) {
  Review <- sum(data$description == "Review") 
  return(Review)
}



#' This function calculates the percentage ratio of article:rerview.
#'
#' @title Article:Review ratio
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return A character value of the percentage ratio of the number of articles and reviews.
#' @export
#'
#' @examples
#' ARRatio(SpeciesData)
#' 
ARRatio <- function(data) {
  Article <- sum(data$description == "Article") 
  Review <- sum(data$description == "Review") 
  ArticleRatio <- Article/(Article+Review)*100 
  ReviewRatio <- Review/(Article+Review)*100 
  Ratio <- paste(ArticleRatio, ":", ReviewRatio)
  return(Ratio)
}



#' This function calculates the h-index of a species.
#'
#' @title Species h-index
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return H-index.
#' @export
#'
#' @examples
#' SpHindex(SpeciesData)
#' 
SpHindex <- function(data) {
  data$citations <- as.numeric(data$citations) 
  sorteddf <- sort(data$citations, decreasing = TRUE) 
  Hindex <- 0  
  for(i in 1:length(sorteddf)) {
    if (sorteddf[i] > Hindex) {
      Hindex <- Hindex + 1
    }
  }
  return(Hindex)
}



#' The number of years since the first publication in relation to the species.
#'
#' @title Years since first publication
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return Number of years.
#' @export
#'
#' @examples
#' YearsPublishing(SpeciesData)
#' 
YearsPublishing <- function(data) {
  data$year <- as.numeric(substr(data$cover_date, 1, 4))
  as.numeric(substr(Sys.Date(), 1, 4)) - min(data$year) 
  years_publishing <- as.numeric(substr(Sys.Date(), 1, 4)) - min(data$year) 
  return(years_publishing)
}



#' This function calculates the m-index of  species.
#' M-index uses the h-index and divides it by the number of years of activity.
#'
#' @title Species m-index
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return M-index.
#' @export
#'
#' @examples
#' SpMindex(SpeciesData)
#' 
SpMindex <- function(data) {
  data$citations <- as.numeric(data$citations) 
  sorteddf <- sort(data$citations, decreasing = TRUE) 
  Hindex <- 0   
  for(i in 1:length(sorteddf)) {
    if (sorteddf[i] > Hindex) {
      Hindex <- Hindex + 1
    }
  }
  data$year <- as.numeric(substr(data$cover_date, 1, 4))
  years_publishing <- as.numeric(substr(Sys.Date(), 1, 4)) - min(data$year) 
  Mindex <- Hindex/years_publishing
  return(Mindex)
}



#' This function calculates the i10 index of a species.
#' i10 index counts all of the publications with 10 or more citations.
#'
#' @title Species i10 index
#' 
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return i10 index.
#' @export
#'
#' @examples
#' Spi10(SpeciesData)
#' 
Spi10 <- function(data) {
  data$citations <- as.numeric(data$citations)
  sorteddf <- sort(data$citations, decreasing = TRUE) 
  i10 <- sum(sorteddf>=10) 
  return(i10)
}



#' This function calculates the h-index of a species in the past 5 years.
#' 
#' @title Species h5 index
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return H5 index.
#' @export
#'
#' @examples
#' SpH5(SpeciesData)
#' 
SpH5 <- function(data) { 
  current_date <- as.numeric(substr(Sys.Date(), 1, 4)) 
  d <- as.POSIXlt(Sys.Date())
  d$year <- d$year-5
  as.Date(d)
  return(HAfterdate(data, d))
}



#' This function calculates the h-index using a given date up till the newest record.
#' 
#' @title Species h-index with a given time frame
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#' @param date The lower limit of the timeframe.
#'
#' @return H-index of the given time period.
#' @export 
#'
#' @examples
#' HAfterdate(SpeciesData, "2000-01-01")
#' 
SpHAfterdate <- function(data, date) {
  library(dplyr)
  data$cover_date <- as.Date(data$cover_date, format = "%Y-%m-%d") 
  subsetdata <- filter(data, cover_date > as.Date(date) )
  HAfterdate <- Hindex(subsetdata)
  return(HAfterdate)
}



#' This function returns a dataframe of the summary of all of the indices.
#' 
#' @title Index summary
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return A datarame of all of the indices in the package.
#' @export
#'
#' @examples
#' Allindices(SpeciesData)
#' 
Allindices <- function(data) {
  combine <- data.frame(TotalPub(data), TotalCite(data), TotalJournals(data), TotalArt(data),
                        TotalRev(data), YearsPublishing(data), SpHindex(data), SpMindex(data), Spi10(data),
                        SpH5(data))
  colnames(combine) <- c("publications", "citations", "journals", "articles",
                         "reviews", "years_publishing", "h", "m", "i10",
                         "h5")
  cat("", TotalPub(data), "publications", "\n",
      TotalCite(data), "citations", "\n",
      TotalJournals(data), "journals", "\n",
      TotalArt(data), "articles", "\n",
      TotalRev(data), "reviews", "\n",
      YearsPublishing(data), "years of publishing", "\n",
      "h:", SpHindex(data), "\n",
      "m:", SpMindex(data), "\n",
      "i10:", Spi10(data), "\n",
      "h5:", SpH5(data))
  return(combine)
}
