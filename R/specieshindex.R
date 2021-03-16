#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title only.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count expanded - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param APIkey Scopus API key needed to access and download data from their database.
#' @param datatype Formats the URL to be sent to the API. The default is "application/xml".
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#'
#' @examples
#' \dontrun{
#' CountSpT("Bettongia", "penicillata", APIkey = "myAPI")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT("bettongia", "penicillata", APIkey = "myAPI")
#' }
#' \dontrun{
#' CountSpT("Bettongia", "penicillata", "conserv*", "myAPI")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT("bettongia", "penicillata", "conserv*", "myAPI")
#' }
CountSpT <- function(genus, species, synonyms, additionalkeywords, APIkey, datatype = "application/xml") {
  if (missing(APIkey)) {
    stop("You need to register for an API key on Scopus.") #stop running if API key missing
  }
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  ) 
  theURL <- httr::GET("http://api.elsevier.com/content/search/scopus",
                      query = list(apiKey = paste0(APIkey),
                                   query = create_query_string_T(genus, species, synonyms, additionalkeywords),
                                   httpAccept = "application/xml")) #format the URL to be sent to the API
  httr::stop_for_status(theURL) #pass any HTTP errors to the R console
  theData <- httr::content(theURL, as = "text") #extract the content of the response
  newData <- XML::xmlParse(theURL) #parse the data to extract values
  resultCount <- as.numeric(XML::xpathSApply(newData,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
  return(resultCount)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title, abstract, and keywords.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count expanded - title, abstract, and keywords
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param APIkey Scopus API key needed to access and download data from their database.
#' @param datatype Formats the URL to be sent to the API. The default is "application/xml".
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#'
#' @examples
#' \dontrun{
#' CountSpTAK("Bettongia", "penicillata", APIkey = "myAPI")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK("bettongia", "penicillata", APIkey = "myAPI")
#' }
#' \dontrun{
#' CountSpTAK("Bettongia", "penicillata", "conserv*", "myAPI")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK("bettongia", "penicillata", "conserv*", "myAPI")
#' }
CountSpTAK <- function(genus, species, synonyms, additionalkeywords, APIkey, datatype = "application/xml") {
  if (missing(APIkey)) {
    stop("You need to register for an API key on Scopus.") #stop running if API key missing
  }
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  ) 
  theURL <- httr::GET("http://api.elsevier.com/content/search/scopus",
                      query = list(apiKey = paste0(APIkey),
                                   query = create_query_string_TAK(genus, species, synonyms, additionalkeywords),
                                   httpAccept = "application/xml")) #format the URL to be sent to the API
  httr::stop_for_status(theURL) #pass any HTTP errors to the R console
  theData <- httr::content(theURL, as = "text") #extract the content of the response
  newData <- XML::xmlParse(theURL) #parse the data to extract values
  resultCount <- as.numeric(XML::xpathSApply(newData,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
  return(resultCount)
}



#' This function fetches citation information from Scopus using genus and species name found in the title of the publications.
#' Duplicates are removed after fetching the data.
#'
#' @title Fetch data - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param language Language of the paper; default is 0, enter 1 to retrieve the variable.
#' @param APIkey Scopus API key needed to access and download data from their database.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @export 
#'
#' @examples
#' \dontrun{
#' FetchSpT("Bettongia", "penicillata", APIkey = "myAPI")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT("bettongia", "penicillata", APIkey = "myAPI")
#' }
#' \dontrun{
#' FetchSpT("Bettongia", "penicillata", "conserv*", "myAPI")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT("bettongia", "penicillata", "conserv*", "myAPI")
#' }
FetchSpT <- function(genus, species, synonyms, additionalkeywords, language = 0, APIkey) {
  count <- CountSpT(genus, species, synonyms, additionalkeywords, APIkey) #check the number of records
  print(paste(count, "records found."))
  if (count < 1) {
    noCitations <- data.frame(citations = 0)
    return(noCitations)
  }
  if (language == 1) {
    lang <- read.csv(file = "data/languages.csv", header = T)[-c(1)]
    datalist <- data.frame()
    for (j in 1:length(lang$language)) {
      theURL <- httr::GET("http://api.elsevier.com/content/search/scopus",
                          query = list(apiKey = paste0(APIkey),
                                       query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords),
                                                      " AND LANGUAGE(", lang$language[j], ")"),
                                       httpAccept = "application/xml")) #format the URL to be sent to the API
      httr::stop_for_status(theURL) #pass any HTTP errors to the R console
      theData <- httr::content(theURL, as = "text") #extract the content of the response
      newData <- XML::xmlParse(theURL) #parse the data to extract values
      resultCount <- as.numeric(XML::xpathSApply(newData,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
      if (resultCount > 0) {
        #search begins
        search2020 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 2019 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf = data.frame()
        for (i in 2019:1990) {
          searchloop <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR = ", i, " AND LANGUAGE(", lang$language[j], ")")),
                                               api_key = paste0(APIkey),
                                               verbose = TRUE,
                                               wait_time = 3)
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf, searchlooplist)
        }
        search1985 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1984 AND PUBYEAR < 1990 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1979 AND PUBYEAR < 1985 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1974 AND PUBYEAR < 1980 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1969 AND PUBYEAR < 1975 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR < 1970 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        langlist <- dplyr::bind_rows(search2020df, searchloopdf, search1985df, search1980df, search1975df, search1970df, search_olddf)
        langlist$language <- lang$language[j]
        #search ends
        datalist <- dplyr::bind_rows(datalist, langlist)
      }
    }
    datalist <- datalist[!is.na(datalist$title), ] #remove NA papers
  } else {
    #loop if count is under 5000
    if (count <= 5000) {
      step_size <- 1000 #the number of records to retrieve in each loop
      start_record <- 0
      datalist = data.frame()
      looprepeat <- ceiling(count/step_size)-1 #the number of loop times, rounded up to the nearest integer
      #loop starts
      for (i in 0:looprepeat) { 
        print(paste("starting iteration: ", i, " Note: iteration size is ", step_size, " records, which runs of 200 records inside each iteration."))
        print(paste("Fetching records now."))
        search <- rscopus::scopus_search(query = create_query_string_T(genus, species, synonyms, additionalkeywords),
                                         api_key = paste0(APIkey),
                                         verbose = TRUE,
                                         max_count = step_size,
                                         start = step_size*i,
                                         wait_time = 3)
        start_record <- as.numeric(summary(search)[1,1]) #move the pointer of starting record for each iteration to a new value
        searchdf <- rscopus::entries_to_citation_df(search$entries)
        list <- data.frame(searchdf)
        datalist <- rbind(datalist, list)
        #loop ends
      }} else {
        #search begins
        search2020 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 2019")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf = data.frame()
        for (i in 2019:1990) {
          searchloop <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR = ", i)),
                                               api_key = paste0(APIkey),
                                               verbose = TRUE,
                                               wait_time = 3)
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- rbind(searchloopdf, searchlooplist)
        }
        search1985 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1984 AND PUBYEAR < 1990")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1979 AND PUBYEAR < 1985")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1974 AND PUBYEAR < 1980")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1969 AND PUBYEAR < 1975")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- rscopus::scopus_search(query = paste0(create_query_string_T(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR < 1970")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)  
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        datalist <- rbind(search2020df, searchloopdf, search1985df, search1980df, search1975df, search1970df, search_olddf)
        #search ends  
      }
  }
  returned <- dim(datalist)[1]
  print(paste(returned, "records retrived in total."))
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
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param language Language of the paper; default is 0, enter 1 to retrieve the variable.
#' @param APIkey Scopus API key needed to access and download data from their database.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @export 
#'
#' @examples
#' \dontrun{
#' FetchSpTAK("Bettongia", "penicillata", APIkey = "myAPI")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpTAK("bettongia", "penicillata", APIkey = "myAPI")
#' }
#' \dontrun{
#' FetchSpTAK("Bettongia", "penicillata", "conserv*", "myAPI")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpTAK("bettongia", "penicillata", "conserv*", "myAPI")
#' }
FetchSpTAK <- function(genus, species, synonyms, additionalkeywords, language = 0, APIkey) {
  count <- CountSpTAK(genus, species, synonyms, additionalkeywords, APIkey) #check the number of records
  print(paste(count, "records found."))
  if (count < 1) {
    noCitations <- data.frame(citations = 0)
    return(noCitations)
  }
  if (language == 1) {
    lang <- read.csv(file = "data/languages.csv", header = T)[-c(1)]
    datalist <- data.frame()
    for (j in 1:length(lang$language)) {
      theURL <- httr::GET("http://api.elsevier.com/content/search/scopus",
                          query = list(apiKey = paste0(APIkey),
                                       query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords),
                                                      " AND LANGUAGE(", lang$language[j], ")"),
                                       httpAccept = "application/xml")) #format the URL to be sent to the API
      httr::stop_for_status(theURL) #pass any HTTP errors to the R console
      theData <- httr::content(theURL, as = "text") #extract the content of the response
      newData <- XML::xmlParse(theURL) #parse the data to extract values
      resultCount <- as.numeric(XML::xpathSApply(newData,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
      if (resultCount > 0) {
        #search begins
        search2020 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 2019 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf = data.frame()
        for (i in 2019:1990) {
          searchloop <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR = ", i, " AND LANGUAGE(", lang$language[j], ")")),
                                               api_key = paste0(APIkey),
                                               verbose = TRUE,
                                               wait_time = 3)
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf, searchlooplist)
        }
        search1985 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1984 AND PUBYEAR < 1990 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1979 AND PUBYEAR < 1985 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1974 AND PUBYEAR < 1980 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1969 AND PUBYEAR < 1975 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR < 1970 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        langlist <- dplyr::bind_rows(search2020df, searchloopdf, search1985df, search1980df, search1975df, search1970df, search_olddf)
        langlist$language <- lang$language[j]
        #search ends
        datalist <- dplyr::bind_rows(datalist, langlist)
      }
    }
    datalist <- datalist[!is.na(datalist$title), ] #remove NA papers
  } else {
    #loop if count is under 5000
    if (count <= 5000) {
      step_size <- 1000 #the number of records to retrieve in each loop
      start_record <- 0
      datalist = data.frame()
      looprepeat <- ceiling(count/step_size)-1 #the number of loop times, rounded up to the nearest integer
      #loop starts
      for (i in 0:looprepeat) { 
        print(paste("starting iteration: ", i, " Note: iteration size is ", step_size, " records, which runs of 200 records inside each iteration."))
        print(paste("Fetching records now."))
        search <- rscopus::scopus_search(query = create_query_string_TAK(genus, species, synonyms, additionalkeywords),
                                         api_key = paste0(APIkey),
                                         verbose = TRUE,
                                         max_count = step_size,
                                         start = step_size*i,
                                         wait_time = 3)
        start_record <- as.numeric(summary(search)[1,1]) #move the pointer of starting record for each iteration to a new value
        searchdf <- rscopus::entries_to_citation_df(search$entries)
        list <- data.frame(searchdf)
        datalist <- rbind(datalist, list)
        #loop ends
      }} else {
        #search begins
        search2020 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 2019")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf = data.frame()
        for (i in 2019:1990) {
          searchloop <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR = ", i)),
                                               api_key = paste0(APIkey),
                                               verbose = TRUE,
                                               wait_time = 3)
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- rbind(searchloopdf, searchlooplist)
        }
        search1985 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1984 AND PUBYEAR < 1990")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1979 AND PUBYEAR < 1985")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1974 AND PUBYEAR < 1980")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1969 AND PUBYEAR < 1975")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- rscopus::scopus_search(query = paste0(create_query_string_TAK(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR < 1970")),
                                             api_key = paste0(APIkey),
                                             verbose = TRUE,
                                             wait_time = 3)  
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        datalist <- rbind(search2020df, searchloopdf, search1985df, search1980df, search1975df, search1970df, search_olddf)
        #search ends  
      }
  }
  returned <- dim(datalist)[1]
  print(paste(returned, "records retrived in total."))
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
#' Bertoli-Barsotti, L. & Lando, T. (2015). On a formula for the h-index. \emph{Journal of Informetrics, 9}(4), 762-776. \cr
#' Hirsch, J. (2005). An index to quantify an individual's scientific research output. \emph{Proceedings of the National Academy of Sciences of the United States of America, 102}(46), 16569-16572.
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
#' University of Pittsburgh (2019). \emph{Research Impact and Metrics: Author metrics.} Retrieved from \url{https://pitt.libguides.com/bibliometricIndicators/AuthorMetrics}.
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
  Mindex <- round(Hindex/years_publishing, digits = 3)
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
#' @references 
#' Cornell University (2019). \emph{i10-index.} Retrieved from \url{https://guides.library.cornell.edu/c.php?g=32272&p=203393}.
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
#' @references 
#' Suzuki, H. (2012). \emph{Google Scholar Metrics for Publications.} Retrieved from \url{https://scholar.googleblog.com/2012/04/google-scholar-metrics-for-publications.html}.
#'
#' @examples
#' data(Woylie)
#' SpH5(Woylie)
#' 
SpH5 <- function(data) { 
  current_date <- as.numeric(substr(Sys.Date(), 1, 4)) 
  d <- as.POSIXlt(Sys.Date())
  d$year <- d$year-5
  if (d < 1) {
    return(as.numeric("0"))
  }
  as.Date(d)
  h5 <- SpHAfterdate(data, d)
  return(h5)
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
  data$cover_date <- as.Date(data$cover_date, format = "%Y-%m-%d") 
  subsetdata <- dplyr::filter(data, cover_date > as.Date(date))
  if (dplyr::count(subsetdata) < 1) { 
    return(as.numeric("0"))
  }
  HAfterdate <- SpHindex(subsetdata)
  return(HAfterdate)
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
  if (all.equal(0, data$citations) == TRUE) {
    zeroIndex <- data.frame(genus_species = paste0(genus, "_", species),
                            species = paste0(species),
                            genus = paste0(genus),
                            publications = 0, citations = 0, journals = 0, articles = 0, reviews = 0, years_publishing = NA,
                            h = 0, m = 0, i10 = 0, h5 = 0)
    return(zeroIndex)
  } else {
    combine <- data.frame(paste0(genus, "_", species), paste0(species), paste0(genus), TotalPub(data), TotalCite(data),
                        TotalJournals(data),TotalArt(data),TotalRev(data), YearsPublishing(data),
                        SpHindex(data), SpMindex(data), Spi10(data), SpH5(data))
  combine[is.na(combine)] <- 0 #replace NA values with 0
  colnames(combine) <- c("genus_species", "species", "genus","publications", "citations", "journals", "articles", "reviews",
                         "years_publishing", "h", "m", "i10", "h5")
  return(combine)
  }
  cat("\n", genus, species, "\n",
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
}



#' Creates a query string to make functions with a query cleaner.
#' Title only.
#' 
#' @title Query string
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_T <- function(genus, species, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE("', genus, ' ', species, '")'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE("', genus, ' ', species, '"', ' AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TITLE("', genus, ' ', species, '"', ' OR ', synonyms[1])
    if (length(synonyms)==1) {
      return(paste0(temp_string, ')'))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0(temp_string, ' OR ', synonyms[i])
      }
      return(paste0(temp_string, ')'))
    }
  }
  if (!missing(additionalkeywords)&!missing(synonyms)) {
    return(paste0('TITLE(("', genus, ' ', species, '"', ' OR ', synonyms, ') AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string to make functions with a query cleaner.
#' Title, abstract, and keywords.
#' 
#' @title Query string
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_TAK <- function(genus, species, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY("', genus, ' ', species, '")'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY("', genus, ' ', species, '"', ' AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TITLE-ABS-KEY("', genus, ' ', species, '"', ' OR ', synonyms[1])
    if (length(synonyms)==1) {
      return(paste0(temp_string, ')'))
    }
    else {
      for (i in 2:length(synonyms)){
         temp_string <- paste0(temp_string, ' OR ', synonyms[i])
      }
      return(paste0(temp_string, ')'))
    }
  }
  if (!missing(additionalkeywords)&!missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY(("', genus, ' ', species, '"', ' OR ', synonyms, ') AND ', additionalkeywords, ')'))
  } 
}