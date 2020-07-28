#' This function counts the total number of search results.
#' It counts only the publications with the binomial name in their title.
#' A check will be conducted via \code{taxize} to validify the genus and species names.
#' 
#' @title Search count - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param APIkey Scopus API key needed to access and download data from their database.
#' @param datatype Formats the URL to be sent to the API. The default is "application/xml".
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#'
#' @references 
#' Scott Chamberlain, Eduard Szocs (2013). "taxize - taxonomic search and retrieval in R." F1000Research. http://f1000research.com/articles/2-191/v2.
#' 
#' @examples
#' \dontrun{
#' CountSpT("Bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#'  CountSpT("bettongia", "penicillata", "
#' }
CountSpT <- function(genus, species, APIkey, datatype = "application/xml") {
  library(httr)
  library(XML)
  library(rlang)
  library(taxize)
  if (is_missing(APIkey)) {
    stop("You need to register for an API key on Scopus.") #stop running if API key missing
  }
  findname <- gnr_resolve(names = c(genus, species)) #check if the species exist
  if (findname$submitted_name %in% findname$matched_name) {
    print(paste("Species found on the Encyclopedia of Life."))
  } else {
    stop("Species not found on the Encyclopedia of Life, please check your spelling.") #stop running if species does not exist
  } 
  theURL <- GET("http://api.elsevier.com/content/search/scopus",
                query = list(apiKey = paste0(APIkey),
                             query = paste0("TITLE(\"",genus," ",species,"\") AND DOCTYPE(ar OR re)"),
                             httpAccept = "application/xml")) #format the URL to be sent to the API
  stop_for_status(theURL) #pass any HTTP errors to the R console
  theData <- content(theURL, as = "text") #extract the content of the response
  newData <- xmlParse(theURL) #parse the data to extract values
  resultCount <- as.numeric(xpathSApply(newData,"//opensearch:totalResults", xmlValue)) #get the total number of search results for the string
  return(resultCount)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title, abstract and keywords.
#' A check will be conducted via \code{taxize} to validify the genus and species names.
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
#' @references 
#' Scott Chamberlain, Eduard Szocs (2013). "taxize - taxonomic search and retrieval in R." F1000Research. http://f1000research.com/articles/2-191/v2.
#'
#' @examples
#' \dontrun{
#' CountSpTAK("Bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK("bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' }
CountSpTAK <- function(genus, species, APIkey, datatype = "application/xml") {
  if (is_missing(APIkey)) {
    stop("You need to register for an API key on Scopus.") #stop running if API key missing
  }
  findname <- gnr_resolve(names = c(genus, species)) #check if the species exist
  if (findname$submitted_name %in% findname$matched_name) {
    print(paste("Species found on the Encyclopedia of Life."))
  } else {
    stop("Species not found on the Encyclopedia of Life, please check your spelling.") #stop running if species does not exist
  } 
  theURL <- GET("http://api.elsevier.com/content/search/scopus",
                query = list(apiKey = paste0(APIkey),
                             query = paste0("TITLE-ABS-KEY(\"",genus," ",species,"\") AND DOCTYPE(ar OR re)"),
                             httpAccept = "application/xml")) #format the URL to be sent to the API
  stop_for_status(theURL) #pass any HTTP errors to the R console
  theData <- content(theURL, as = "text") #extract the content of the response
  newData <- xmlParse(theURL) #parse the data to extract values
  resultCount <- as.numeric(xpathSApply(newData,"//opensearch:totalResults", xmlValue)) #get the total number of search results for the string
  return(resultCount)
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
#' \dontrun{
#' FetchSpT("Bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT("bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' }
FetchSpT <- function(genus, species, APIkey) {
  library(rscopus)
  library(rlang)
  library(dplyr)
  if (is_missing(APIkey)) {
    stop("You need to register for an API key on Scopus.") #stop running if API key missing
  }
  count <- CountSpT(genus, species, APIkey) #check the number of records
  print(paste(count, "records found."))
  if (count > 2000) {
    print(paste("More than 2000 records found, this will take a while unless you have a nice computer."))
  }
  step_size <- 1000 #the number of records to retrieve in each loop
  start_record <- 0
  datalist = data.frame()
  looprepeat <- ceiling(count/step_size)-1 #the number of loop times, rounded up to the nearest integer
  #loop starts
  for (i in 0:4) { 
    print(paste("starting iteration: ", i, " Note: iteration size is ", step_size, " records, which runs of 200 records inside each iteration."))
    print(paste("Fetching records now."))
    search <- scopus_search(query = paste0("TITLE(\"",genus," ",species,"\") AND DOCTYPE(ar OR re)"),
                            api_key = paste0(APIkey),
                            verbose = TRUE,
                            max_count = step_size,
                            start = step_size*i,
                            wait_time = 3)
    start_record <- as.numeric(summary(search)[1,1]) #move the pointer of starting record for each iteration to a new value
    searchdf <- entries_to_citation_df(search$entries)
    list <- data.frame(searchdf)
    datalist <- rbind(datalist, list)
    print(paste("Retrieved", start_record, "records."))
  }
  #loop ends
  returned <- dim(datalist)[1]
  print(paste(returned, "records retrived in total."))
  duplicates <- dim(datalist[duplicated(datalist$title),])[1] #check for duplicates
  print(paste(duplicates, "duplicates found."))
  if (duplicates>0) { #remove duplicates if they are present
    print(paste("Removing duplicated records."))
    datalist <- datalist[!duplicated(datalist$title), ] 
  }
  #showing final list of records
  retrieved <- dim(datalist)[1] #check the number
  print(paste(retrieved, "unique records successfully fetched."))
  return(datalist)
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
#' \dontrun{
#' FetchSpT("Bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT("bettongia", "penicillata", "442b9048417ef20cf680a0ae26ee4d86")
#' }
FetchSpTAK <- function(genus, species, APIkey) {
  library(rscopus)
  library(rlang)
  library(dplyr)
  if (is_missing(APIkey)) {
    stop("You need to register for an API key on Scopus.") #stop running if API key missing
  }
  count <- CountSpTAK(genus, species, APIkey) #check the number of records
  print(paste(count, "records found."))
  if (count > 2000) {
    print(paste("More than 2000 records found, this will take a while unless you have a nice computer."))
  }
  step_size <- 1000 #the number of records to retrieve in each loop
  start_record <- 0
  datalist = data.frame()
  looprepeat <- ceiling(count/step_size)-1 #the number of loop times, rounded up to the nearest integer
  #loop starts
  for (i in 0:4) { 
    print(paste("starting iteration: ", i, " Note: iteration size is ", step_size, " records, which runs of 200 records inside each iteration."))
    print(paste("Fetching records now."))
    search <- scopus_search(query = paste0("TITLE-ABS-KEY(\"",genus," ",species,"\") AND DOCTYPE(ar OR re)"),
                            api_key = paste0(APIkey),
                            verbose = TRUE,
                            max_count = step_size,
                            start = step_size*i,
                            wait_time = 3)
    start_record <- as.numeric(summary(search)[1,1]) #move the pointer of starting record for each iteration to a new value
    searchdf <- entries_to_citation_df(search$entries)
    list <- data.frame(searchdf)
    datalist <- rbind(datalist, list)
    print(paste("Retrieved", start_record, "records."))
  }
  #loop ends
  returned <- dim(datalist)[1]
  print(paste(returned, "records retrived in total."))
  duplicates <- dim(datalist[duplicated(datalist$title),])[1] #check for duplicates
  print(paste(duplicates, "duplicates found."))
  if (duplicates>0) { #remove duplicates if they are present
    print(paste("Removing duplicated records."))
    datalist <- datalist[!duplicated(datalist$title), ] 
  }
  #showing final list of records
  retrieved <- dim(datalist)[1] #check the number
  print(paste(retrieved, "unique records successfully fetched."))
  return(datalist)
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
#' data(Woylie)
#' TotalPub(Woylie)
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
#' data(Woylie)
#' TotalCite(Woylie)
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
#' data(Woylie)
#' TotalJournals(Woylie)
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
#' data(Woylie)
#' TotalArt(Woylie)
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
#' data(Woylie)
#' TotalRev(Woylie)
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
#' data(Woylie)
#' ARRatio(Woylie)
#' 
ARRatio <- function(data) {
  Article <- sum(data$description == "Article") 
  Review <- sum(data$description == "Review") 
  ArticleRatio <- signif(Article/(Article+Review)*100, digits = 4)
  ReviewRatio <- signif(Review/(Article+Review)*100, digits = 4)
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
#' @references 
#' Bertoli-Barsotti, L., & Lando, T. (2015). On a formula for the h-index. Journal of Informetrics, 9(4), 762-776.
#' Hirsch, J. (2005). An index to quantify an individual's scientific research output. Proceedings of the National Academy of Sciences of the United States of America, 102(46), 16569-16572.
#'
#' @examples
#' data(Woylie)
#' SpHindex(Woylie)
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
#' data(Woylie)
#' YearsPublishing(Woylie)
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
#' @references 
#' Hirsch, J. (2005). An index to quantify an individual's scientific research output. Proceedings of the National Academy of Sciences of the United States of America, 102(46), 16569-16572.
#'
#' @examples
#' data(Woylie)
#' SpMindex(Woylie)
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
#' data(Woylie)
#' Spi10(Woylie)
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
#' data(Woylie)
#' SpH5(Woylie)
#' 
SpH5 <- function(data) { 
  current_date <- as.numeric(substr(Sys.Date(), 1, 4)) 
  d <- as.POSIXlt(Sys.Date())
  d$year <- d$year-5
  as.Date(d)
  return(SpHAfterdate(data, d))
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
#' data(Woylie)
#' SpHAfterdate(Woylie, "2000-01-01")
#' 
SpHAfterdate <- function(data, date) {
  #library(dplyr)
  data$cover_date <- as.Date(data$cover_date, format = "%Y-%m-%d") 
  subsetdata <- filter(data, cover_date > as.Date(date) )
  HAfterdate <- SpHindex(subsetdata)
  return(HAfterdate)
}



#' This function calculates the h-index by year.
#' 
#' @title H-index by year
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @returnA A dataframe of h-index by year.
#' @export
#'
#' @examples
#' data(Woylie)
#' SpHYear(Woylie)
#' 
SpHYear <- function(data) {
  #library(dplyr)
  data$year <- as.numeric(substr(data$cover_date, 1, 4))
  yeargroup <- data %>% 
    group_by(data$year)
  splitdf <- split(yeargroup, data$year) 
  splitH <- lapply(splitdf, function(splitdf){SpHindex(splitdf)}) 
  H <- setNames(stack(splitH)[2:1], c('year','h')) 
  H$year <- as.integer(levels(H$year))[H$year]
  fulldates <- seq(min(H$year), max(H$year)) 
  fulldates <- data.frame(year = fulldates) 
  completeH <- merge(fulldates, H, by = "year", all.x = TRUE)
  completeH[is.na(completeH)] <- 0 
  return(completeH)
}



#' This function calculates the cumulative h-index overtime by year.
#' 
#' @title H-index growth
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return A dataframe of the cumulative h-index.
#' @export
#'
#' @examples
#' data(Woylie)
#' SpHGrowth(Woylie)
#' 
SpHGrowth <- function(data) {
  HbyYear <- SpHYear(data)
  HbyYear$h <- cumsum(HbyYear [,2])
  return(HbyYear)
}



#' This function returns a dataframe of the summary of all of the indices.
#' 
#' @title Index summary
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#'
#' @return A datarame of all of the indices in the package.
#' @export
#'
#' @examples
#' data(Woylie)
#' Allindices(Woylie, genus = "genus_name", species = "species_name")
#' 
Allindices <- function(data, genus, species) {
  combine <- data.frame(paste0(genus, "_", species), TotalPub(data), TotalCite(data),
                        TotalJournals(data),TotalArt(data),TotalRev(data), YearsPublishing(data),
                        SpHindex(data), SpMindex(data), Spi10(data), SpH5(data))
  colnames(combine) <- c("species", "publications", "citations", "journals", "articles",
                         "reviews", "years_publishing", "h", "m", "i10", "h5")
  cat("", genus, species, "\n",
      TotalPub(data), "publications", "\n",
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