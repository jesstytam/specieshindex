#' This function counts the total number of search results from Scopus, Web of Science, or BASE.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count of literature
#'
#' @param db Literature database. Scopus ("scopus"), Web of Science ("wos"), or Base ("base").
#' @param search Search fields. Title only ("t") or title, abstract, or keywords ("tak").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names; optional.
#' @param additionalkeywords Optional search terms.
#'
#' @return Search count of the genus or species with the given \code{genus} and/or \code{species}.
#' @export
#'
#' @examples
#' \dontrun{
#' Count(db = "scopus",
#'       search = "t",
#'       genus = "Osphranter",
#'       species = "rufus")
#' }
#' \dontrun{
#' Count(db = "scopus",
#'       search = "t",
#'       genus = "Osphranter",
#'       species = "rufus",
#'       synonyms = "Macropus rufus",
#'       additionalkeywords = "conserv*")
#' }
Count <- function(db = c("scopus", "wos", "base"),
                  search = c("t", "tak"),
                  genus,
                  species = NULL,
                  synonyms,
                  additionalkeywords) {
  
  db <- match.arg(db)
  search <- match.arg(search)
  
  if (missing(db)) {
    stop('Pick a database by setting db = "scopus" / "wos" / "base".')
  }
  if (missing(search)) {
    stop('Pick the search field(s) by setting search = "t" for title only
         or search = "tak" for title, absract, and keywords')
  }
  if (missing(genus)) {
    stop('Genus is missing from your query.')
  }
  if (db == "scopus" & search == "t") {
    countsp <- Count_scopus(search = "t",
                            genus,
                            species,
                            synonyms,
                            additionalkeywords)
  } else if (db == "scopus" & search == "tak") {
    countsp <- Count_scopus(search = "tak",
                            genus, 
                            species,
                            synonyms,
                            additionalkeywords)
  } else if (db == "wos" & search == "t") {
    countsp <- Count_wos(search = "t",
                         genus,
                         species,
                         synonyms,
                         additionalkeywords)
  } else if (db == "wos" & search == "tak") {
    countsp <- Count_wos(search = "tak",
                         genus,
                         species,
                         synonyms,
                         additionalkeywords)
  } else if (db == "base" & search == "t") {
    countsp <- Count_base(search = "t",
                          genus,
                          species,
                          synonyms,
                          additionalkeywords)
  } else if (db == "base" & search == "tak") {
    countsp <- Count_base(search = "tak",
                          genus,
                          species,
                          synonyms,
                          additionalkeywords)
  } 
  return(countsp)
}



#' This function fetches citation information from Scopus, Web of Science, or BASE.
#' Duplicates are to be removed by the user after fetching the data.
#' 
#' @title Fetch citation records
#'
#' @param db Literature database. Scopus ("scopus"), Web of Science ("wos"), or Base ("base").
#' @param search Search fields. Title only ("t") or title, abstract, or keywords ("tak").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names; optional.
#' @param additionalkeywords Optional search terms.
#' @param language Language of the paper; default is 0, enter 1 to retrieve the variable. Scopus only.
#'
#' @return A dataframe of the genus' or species' citation records with the given \code{genus} and/or \code{species}.
#' @export
#'
#' @examples
#' \dontrun{
#' Fetch(db = "scopus",
#'       search = "t",
#'       genus = "Osphranter", species = "rufus")
#' }
#' \dontrun{
#' Fetch(db = "scopus",
#'       search = "t",
#'       genus = "Osphranter",
#'       species = "rufus",
#'       synonyms = "Macropus rufus",
#'       additionalkeywords = "conserv*")
#' }
Fetch <- function(db = c("scopus", "wos", "base"),
                  search = c("t", "tak"),
                  genus,
                  species = NULL,
                  synonyms,
                  additionalkeywords,
                  language = 0) {
  
  db <- match.arg(db)
  search <- match.arg(search)
  
  if (missing(db)) {
    stop('Pick a database by setting db = "scopus" / "wos" / "base".')
  }
  if (missing(search)) {
    stop('Pick the search field(s) by setting search = "t" for title only
         or search = "tak" for title, absract, and keywords')
  }
  if (missing(genus)) {
    stop('Genus is missing from your query.')
  }
  if (db == "scopus" & search == "t") {
    fetchsp <- FetchT_scopus(genus,
                             species,
                             synonyms,
                             additionalkeywords,
                             language)
  } else if (db == "scopus" & search == "tak") {
    fetchsp <- FetchTAK_scopus(genus,
                               species,
                               synonyms,
                               additionalkeywords)
  } else if (db == "wos" & search == "t") {
    fetchsp <- FetchT_wos(genus,
                          species,
                          synonyms,
                          additionalkeywords)
  } else if (db == "wos" & search == "tak") {
    fetchsp <- FetchTAK_wos(genus,
                            species,
                            synonyms,
                            additionalkeywords)
  } else if (db == "base") {
    stop("Data extraction is not available for BASE")
  } 
  return(fetchsp)
}



#' This function counts the total number of search results.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from Scopus 
#'
#' @param search Search fields. Title only ("t") or title, abstract, or keywords ("tak").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param datatype Formats the URL to be sent to the API. The default is "application/xml".
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#' 
#' @examples
#' \dontrun{
#' Count_scopus(search = "t",
#'              genus = "Bettongia",
#'              species = "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' Count_scopus(search = "t",
#'              genus = "Bettongia",
#'              species = "penicillata")
#' }
#' \dontrun{
#' Count_scopus(search = "t",
#'              genus = "Bettongia",
#'              species = "penicillata",
#'              additionalkeywords = "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' Count_scopus(search = "t",
#'              genus = "bettongia",
#'              species = "penicillata",
#'              additionalkeywords = "conserv*")
#' }
#' 
#' @noRd
#' 
Count_scopus <- function(search = c("t", "tak"),
                         genus,
                         species = NULL,
                         synonyms,
                         additionalkeywords,
                         datatype = "application/xml") {
  
  search <- match.arg(search)
  
  sp_check(genus,
           species = paste0(species))
  if (search == "t") {
    response <- httr::GET("http://api.elsevier.com/content/search/scopus",
                          query = list(apiKey = apikey,
                                       query = create_query_string_T_scopus(genus,
                                                                            species = paste0(species),
                                                                            synonyms,
                                                                            additionalkeywords),
                                       httpAccept = "application/xml")) #format the URL to be sent to the API
  } else if (search == "tak") {
    response <- httr::GET("http://api.elsevier.com/content/search/scopus",
                          query = list(apiKey = apikey,
                                       query = create_query_string_TAK_scopus(genus,
                                                                              species = paste0(species),
                                                                              synonyms,
                                                                              additionalkeywords),
                                       httpAccept = "application/xml")) #format the URL to be sent to the API
  } else {
    stop('Set search = "t" for title-only searches, or "tak" for searches in the title, abstract, or keywords.')
  }
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response) #parse the data to extract values
  resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
  return(resultCount)
}



#' This function fetches citation information from Scopus using genus and species name found in the title of the publications.
#' Duplicates are to be removed by the user after fetching the data.
#'
#' @title Fetch data from Scopus - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param language Language of the paper; default is 0, enter 1 to retrieve the variable.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @importFrom utils read.csv
#' 
#'
#' @examples
#' \dontrun{
#' FetchT_scopus(genus = "Bettongia",
#'               species = "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchT_scopus(genus = "bettongia",
#'               species = "penicillata")
#' }
#' \dontrun{
#' FetchT_scopus(genus = "Bettongia",
#'               species = "penicillata",
#'               additionalkeywords = "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchT_scopus(genus = "bettongia",
#'               species = "penicillata",
#'               additionalkeywords = "conserv*")
#' }
#' 
#' @noRd
#' 
FetchT_scopus <- function(genus,
                          species = NULL,
                          synonyms,
                          additionalkeywords,
                          language = 0,
                          ...) {
  count <- Count_scopus(search = "t",
                        genus,
                        species = paste0(species),
                        synonyms,
                        additionalkeywords) #check the number of records
  print(paste(count, "records found."))
  if (count < 1) {
    noCitations <- data.frame(citations = 0)
    return(noCitations)
  }
  if (language == 1) {
    lang <- read.csv(file = "data/languages.csv", header = T)[-c(1)]
    datalist <- data.frame()
    for (j in 1:length(lang$language)) {
      response <- httr::GET("http://api.elsevier.com/content/search/scopus",
                            query = list(apiKey = apikey,
                                         query = paste0(create_query_string_T_scopus(genus,
                                                                                     species = paste0(species),
                                                                                     synonyms,
                                                                                     additionalkeywords),
                                                        " AND LANGUAGE(", lang$language[j], ")"),
                                         httpAccept = "application/xml")) #format the URL to be sent to the API
      httr::stop_for_status(response) #pass any HTTP errors to the R console
      response_data <- XML::xmlParse(response) #parse the data to extract values
      resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
      if (resultCount > 0) {
        #search begins
        search2020 <- scopus_request_t('" AND PUBYEAR > 2019 AND LANGUAGE(", lang$language[j], ")"')
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf <- data.frame()
        for (i in 2019:1990) {
          searchloop <- scopus_request_t('" AND PUBYEAR = ", i, " AND LANGUAGE(", lang$language[j], ")"')
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf,
                                           searchlooplist)
        }
        search1985 <- scopus_request_t('" AND PUBYEAR > 1984 AND PUBYEAR < 1990 AND LANGUAGE(", lang$language[j], ")"')
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- scopus_request_t('" AND PUBYEAR > 1979 AND PUBYEAR < 1985 AND LANGUAGE(", lang$language[j], ")"')
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- scopus_request_t('" AND PUBYEAR > 1974 AND PUBYEAR < 1980 AND LANGUAGE(", lang$language[j], ")"')
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- scopus_request_t('" AND PUBYEAR > 1969 AND PUBYEAR < 1975 AND LANGUAGE(", lang$language[j], ")"')
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- scopus_request_t('" AND PUBYEAR < 1970 AND LANGUAGE(", lang$language[j], ")"')
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        langlist <- dplyr::bind_rows(search2020df,
                                     searchloopdf,
                                     search1985df,
                                     search1980df,
                                     search1975df,
                                     search1970df,
                                     search_olddf)
        langlist$language <- lang$language[j]
        #search ends
        datalist <- dplyr::bind_rows(datalist,
                                     langlist)
      }
    }
    datalist <- datalist[!is.na(datalist$title), ] #remove NA papers
  } else {
    #loop if count is under 5000
    if (count <= 5000) {
      step_size <- 1000 #the number of records to retrieve in each loop
      start_record <- 0
      datalist <- data.frame()
      looprepeat <- ceiling(count/step_size)-1 #the number of loop times, rounded up to the nearest integer
      #loop starts
      for (i in 0:looprepeat) { 
        print(paste("starting iteration: ", i, " Note: iteration size is ", step_size, " records, which runs of 200 records inside each iteration."))
        print(paste("Fetching records now."))
        search <- rscopus::scopus_search(query = create_query_string_T_scopus(genus,
                                                                              species = paste0(species),
                                                                              synonyms,
                                                                              additionalkeywords),
                                         api_key = apikey,
                                         verbose = TRUE,
                                         max_count = step_size,
                                         start = step_size*i,
                                         wait_time = 3)
        start_record <- as.numeric(summary(search)[1,1]) #move the pointer of starting record for each iteration to a new value
        searchdf <- rscopus::entries_to_citation_df(search$entries)
        list <- data.frame(searchdf)
        datalist <- dplyr::bind_rows(datalist,
                                     list)
        #loop ends
      }} else {
        #search begins
        search2020 <- scopus_request_t('" AND PUBYEAR > 2019"')
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf <- data.frame()
        for (i in 2019:1990) {
          searchloop <- scopus_request_t('" AND PUBYEAR = ", i')
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf, searchlooplist)
        }
        search1985 <- scopus_request_t('" AND PUBYEAR > 1984 AND PUBYEAR < 1990"')
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- scopus_request_t('" AND PUBYEAR > 1979 AND PUBYEAR < 1985"')
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- scopus_request_t('" AND PUBYEAR > 1974 AND PUBYEAR < 1980"')
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- scopus_request_t('" AND PUBYEAR > 1969 AND PUBYEAR < 1975"')
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- scopus_request_t('" AND PUBYEAR < 1970"')
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        datalist <- dplyr::bind_rows(search2020df,
                                     searchloopdf,
                                     search1985df,
                                     search1980df, 
                                     search1975df,
                                     search1970df,
                                     search_olddf)
        #search ends  
      }
  }
  returned <- dim(datalist)[1]
  print(paste(returned, "records retrived in total."))
  return(datalist)
}



#' This function fetches citation information from Scopus using genus and species name found in the title, abstract and keywords of the publications.
#' Duplicates are to be removed by the user after fetching the data.
#'
#' @title Fetch data from Scopus - title, abstract and keywords
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param language Language of the paper; default is 0, enter 1 to retrieve the variable.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @importFrom utils read.csv
#' 
#'
#' @examples
#' \dontrun{
#' FetchTAK_scopus(genus = "Bettongia",
#'               species = "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchTAK_scopus(genus = "bettongia",
#'               species = "penicillata")
#' }
#' \dontrun{
#' FetchTAK_scopus(genus = "Bettongia",
#'                 species = "penicillata",
#'                 additionalkeywords = "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchTAK_scopus(genus = "bettongia",
#'                 species = "penicillata",
#'                 additionalkeywords = "conserv*")
#' }
#' 
#' @noRd
#' 
FetchTAK_scopus <- function(genus,
                            species = NULL,
                            synonyms,
                            additionalkeywords,
                            language = 0,
                            ...) {
  count <- Count_scopus(search = "tak",
                        genus,
                        species = paste0(species),
                        synonyms,
                        additionalkeywords) #check the number of records
  print(paste(count, "records found."))
  if (count < 1) {
    noCitations <- data.frame(citations = 0)
    return(noCitations)
  }
  if (language == 1) {
    lang <- read.csv(file = "data/languages.csv", header = T)[-c(1)]
    datalist <- data.frame()
    for (j in 1:length(lang$language)) {
      response <- httr::GET("http://api.elsevier.com/content/search/scopus",
                            query = list(apiKey = apikey,
                                         query = paste0(create_query_string_TAK_scopus(genus,
                                                                                       species = paste0(species),
                                                                                       synonyms,
                                                                                       additionalkeywords),
                                                        " AND LANGUAGE(", lang$language[j], ")"),
                                         httpAccept = "application/xml")) #format the URL to be sent to the API
      httr::stop_for_status(response) #pass any HTTP errors to the R console
      response_data <- XML::xmlParse(response) #parse the data to extract values
      resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
      if (resultCount > 0) {
        #search begins
        search2020 <- scopus_request_tak('" AND PUBYEAR > 2019 AND LANGUAGE(", lang$language[j], ")"')
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf <- data.frame()
        for (i in 2019:1990) {
          searchloop <- scopus_request_tak('" AND PUBYEAR = ", i, " AND LANGUAGE(", lang$language[j], ")"')
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf,
                                           searchlooplist)
        }
        search1985 <- scopus_request_tak('" AND PUBYEAR > 1984 AND PUBYEAR < 1990 AND LANGUAGE(", lang$language[j], ")"')
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- scopus_request_tak('" AND PUBYEAR > 1979 AND PUBYEAR < 1985 AND LANGUAGE(", lang$language[j], ")"')
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- scopus_request_tak('" AND PUBYEAR > 1974 AND PUBYEAR < 1980 AND LANGUAGE(", lang$language[j], ")"')
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- scopus_request_tak('" AND PUBYEAR > 1969 AND PUBYEAR < 1975 AND LANGUAGE(", lang$language[j], ")"')
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- scopus_request_tak('" AND PUBYEAR < 1970 AND LANGUAGE(", lang$language[j], ")"')
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        langlist <- dplyr::bind_rows(search2020df,
                                     searchloopdf,
                                     search1985df,
                                     search1980df,
                                     search1975df,
                                     search1970df,
                                     search_olddf)
        langlist$language <- lang$language[j]
        #search ends
        datalist <- dplyr::bind_rows(datalist,
                                     langlist)
      }
    }
    datalist <- datalist[!is.na(datalist$title), ] #remove NA papers
  } else {
    #loop if count is under 5000
    if (count <= 5000) {
      step_size <- 1000 #the number of records to retrieve in each loop
      start_record <- 0
      datalist <- data.frame()
      looprepeat <- ceiling(count/step_size)-1 #the number of loop times, rounded up to the nearest integer
      #loop starts
      for (i in 0:looprepeat) { 
        print(paste("starting iteration: ", i, " Note: iteration size is ", step_size, " records, which runs of 200 records inside each iteration."))
        print(paste("Fetching records now."))
        search <- rscopus::scopus_search(query = create_query_string_TAK_scopus(genus,
                                                                                species = paste0(species),
                                                                                synonyms, 
                                                                                additionalkeywords),
                                         api_key = apikey,
                                         verbose = TRUE,
                                         max_count = step_size,
                                         start = step_size*i,
                                         wait_time = 3)
        start_record <- as.numeric(summary(search)[1,1]) #move the pointer of starting record for each iteration to a new value
        searchdf <- rscopus::entries_to_citation_df(search$entries)
        list <- data.frame(searchdf)
        datalist <- dplyr::bind_rows(datalist,
                                     list)
        #loop ends
      }} else {
        #search begins
        search2020 <- scopus_request_tak('" AND PUBYEAR > 2019"')
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf <- data.frame()
        for (i in 2019:1990) {
          searchloop <- scopus_request_tak('" AND PUBYEAR = ", i')
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf, searchlooplist)
        }
        search1985 <- scopus_request_tak('" AND PUBYEAR > 1984 AND PUBYEAR < 1990"')
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- scopus_request_tak('" AND PUBYEAR > 1979 AND PUBYEAR < 1985"')
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- scopus_request_tak('" AND PUBYEAR > 1974 AND PUBYEAR < 1980"')
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- scopus_request_tak('" AND PUBYEAR > 1969 AND PUBYEAR < 1975"')
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- scopus_request_tak('" AND PUBYEAR < 1970"')
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        datalist <- dplyr::bind_rows(search2020df,
                                     searchloopdf,
                                     search1985df,
                                     search1980df,
                                     search1975df,
                                     search1970df,
                                     search_olddf)
        #search ends  
      }
  }
  returned <- dim(datalist)[1]
  print(paste(returned, "records retrived in total."))
  return(datalist)
}



#' This function counts the total number of search results.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from Web of Science
#'
#' @param search Search fields. Title only ("t") or title, abstract, or keywords ("tak").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' 
#' @examples
#' \dontrun{
#' Count_wos(search = "t",
#'           genus = "Bettongia",
#'           species = "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' Count_wos(search = "t",
#'           genus = "bettongia",
#'           species = "penicillata")
#' }
#' \dontrun{
#' Count_wos(search = "t",
#'           genus = "Bettongia",
#'           species = "penicillata",
#'           additionalkeywords = "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' Count_wos(search = "t",
#'           genus = "bettongia",
#'           species = "penicillata",
#'           additionalkeywords = "conserv*")
#' }
#' 
#' @noRd
#' 
Count_wos <- function(search,
                      genus,
                      species = NULL,
                      synonyms,
                      additionalkeywords) {
  sp_check(genus,
           species = paste0(species))
  if (search == "t") {
    count <- wosr::query_wos(query = create_query_string_T_wos(genus,
                                                               species = paste0(species),
                                                               synonyms,
                                                               additionalkeywords),
                             sid = sid) 
  } else if (search == "tak") {
    count <- wosr::query_wos(query = create_query_string_TAK_wos(genus,
                                                                 species = paste0(species),
                                                                 synonyms,
                                                                 additionalkeywords),
                             sid = sid) 
  } else {
    stop('Set search = "t" for title-only searches, or "tak" for searches in the title, abstract, or keywords.')
  }
  return(count)
}



#' This function fetches citation information from Web of Science using genus and species name found in the title of the publications.
#' Duplicates are to be removed by the user after fetching the data.
#'
#' @title Fetch data from Web of Science - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @importFrom data.table rbindlist setDT .SD
#' @importFrom stats na.omit
#' 
#' @examples
#' \dontrun{
#' FetchT_wos(genus = "Bettongia",
#'            species = "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchT_wos(genus = "bettongia",
#'            species = "penicillata")
#' }
#' \dontrun{
#' FetchT_wos(genus = "Bettongia",
#'            species = "penicillata",
#'            additionalkeywords = "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchT_wos(genus = "bettongia",
#'            species = "penicillata",
#'            additionalkeywords = "conserv*")
#' }
#' 
#' @noRd
#' 
FetchT_wos <- function(genus,
                       species = NULL,
                       synonyms,
                       additionalkeywords) {
  count <- Count_wos(search = "t",
                     genus,
                     species = paste0(species),
                     synonyms,
                     additionalkeywords) #check the number of records
  print(paste(count, "records found."))
  if (count < 1) {
    noCitations <- data.frame(citations = 0)
    return(noCitations)
  }
  query <- wosr::pull_wos(query = create_query_string_T_wos(genus,
                                                            species = paste0(species), 
                                                            synonyms,
                                                            additionalkeywords),
                          sid = sid) 
  results <- data.table::rbindlist(query, fill = TRUE)
  results <- data.table::setDT(results)[, lapply(data.table::.SD, function(x) toString(na.omit(x))), by = ut]
  #renaming columns
  names(results)[names(results) == "tot_cites"] <- "citations"
  names(results)[names(results) == "doc_type"] <- "description"
  names(results)[names(results) == "date"] <- "cover_date"
  #showing final list of records
  returned <- nrow(results)
  print(paste(returned, "records retrived in total."))
  return(results)
}



#' This function fetches citation information from Web of Science using genus and species name found in the title, abstract and author keywords of the publications.
#' Duplicates are to be removed by the user after fetching the data.
#'
#' @title Fetch data from Web of Science - title, abstract and author keywords.
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @importFrom data.table rbindlist setDT .SD
#' @importFrom stats na.omit
#' 
#' @examples
#' \dontrun{
#' FetchTAK_wos(genus = "Bettongia",
#'              species = "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchTAK_wos(genus = "bettongia",
#'              species = "penicillata")
#' }
#' \dontrun{
#' FetchTAK_wos(genus = "Bettongia",
#'              species = "penicillata",
#'              additionalkeywords = "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchTAK_wos(genus = "bettongia",
#'              species = "penicillata",
#'              additionalkeywords = "conserv*")
#' }
#' 
#' @noRd
#' 
FetchTAK_wos <- function(genus,
                         species = NULL,
                         synonyms,
                         additionalkeywords) {
  count <- Count_wos(search = "tak",
                     genus,
                     species = paste0(species),
                     synonyms,
                     additionalkeywords) #check the number of records
  print(paste(count, "records found."))
  if (count < 1) {
    noCitations <- data.frame(citations = 0)
    return(noCitations)
  }
  query <- wosr::pull_wos(query = create_query_string_TAK_wos(genus,
                                                              species = paste0(species), 
                                                              synonyms,
                                                              additionalkeywords),
                          sid = sid) 
  results <- data.table::rbindlist(query, fill = TRUE)
  results <- data.table::setDT(results)[, lapply(data.table::.SD, function(x) toString(na.omit(x))), by = ut]
  #renaming columns
  names(results)[names(results) == "tot_cites"] <- "citations"
  names(results)[names(results) == "doc_type"] <- "description"
  names(results)[names(results) == "date"] <- "cover_date"
  #showing final list of records
  returned <- nrow(results)
  print(paste(returned, "records retrived in total."))
  return(results)
}



#' This function counts the total number of search results.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from BASE
#'
#' @param search Search fields. Title only ("t") or title, abstract, or keywords ("tak").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#'
#' @examples
#' \dontrun{
#' Count_base(search = "t",
#'            genus = "Bettongia",
#'            species = "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' Count_base(search = "t",
#'            genus ="bettongia",
#'            species = "penicillata")
#' }
#' \dontrun{
#' Count_base(search = "t",
#'            genus = "Bettongia",
#'            species = "penicillata",
#'            additionalkeywords = "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' Count_base(search = "t",
#'            genus = "bettongia",
#'            species = "penicillata",
#'            additionalkeywords = "conserv*")
#' }
#' 
#' @noRd
#' 
Count_base <- function(search,
                       genus,
                       species = NULL,
                       synonyms,
                       additionalkeywords) {
  sp_check(genus,
           species = paste0(species))
  if (search == "t") {
    response <- httr::GET("https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi",
                          query = list(func = "PerformSearch",
                                       query = create_query_string_T_base(genus,
                                                                          species = paste0(species),
                                                                          synonyms,
                                                                          additionalkeywords)))
  } else if (search == "tak") {
    response <- httr::GET("https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi",
                          query = list(func = "PerformSearch",
                                       query = create_query_string_TAK_base(genus,
                                                                            species = paste0(species),
                                                                            synonyms,
                                                                            additionalkeywords)))
  } else {
    stop('Set search = "t" for title-only searches, or "tak" for searches in the title, abstract, or keywords.')
  }
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response)
  resultCount <- as.numeric(XML::xpathSApply(response_data, "//response/result/@numFound"))
  return(resultCount)
}



#' This function calculates the total number of publications.
#'
#' @title Total publications
#'
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return An integer of the total number of publications.
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
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
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return A numerical value of the total number of citations.
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
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
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return An integer of the total number of journals.
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
#' TotalJournals(Woylie)
#' 
TotalJournals <- function(data) {
  filter <- unique(data$journal)
  total <- length(filter)
  return(total)
}



#' This function calculates the total number of items for each document type.
#' 
#' @title Source type
#'
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return A dataframe with each document and their counts.
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
#' SourceType(Woylie)
#' 
SourceType <- function(data) {
  source_type <- data.frame(table(data$description))
  source_type_transpose <- as.data.frame(t(source_type))
  names(source_type_transpose) <- as.matrix(source_type_transpose[1,])
  source_type_transpose <- source_type_transpose[-1,]
  row.names(source_type_transpose) <- NULL
  return(source_type_transpose)
}



#' The number of years since the first publication in relation to the species.
#'
#' @title Years since first publication
#'
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return Number of years.
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
#' YearsPublishing(Woylie)
#' 
YearsPublishing <- function(data) {
  data$year <- as.numeric(substr(data$cover_date, 1, 4))
  as.numeric(substr(Sys.Date(), 1, 4)) - min(data$year) 
  years_publishing <- as.numeric(substr(Sys.Date(), 1, 4)) - min(data$year, na.rm = TRUE) 
  return(years_publishing)
}



#' This function calculates the h-index of a species.
#'
#' @title Species h-index
#'
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return H-index.
#' @export
#'
#' @references 
#' Bertoli-Barsotti, L. & Lando, T. (2015). On a formula for the h-index. \emph{Journal of Informetrics, 9}(4), 762-776. \cr
#' Hirsch, J. (2005). An index to quantify an individual's scientific research output. \emph{Proceedings of the National Academy of Sciences of the United States of America, 102}(46), 16569-16572.
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
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
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return M-index.
#' @export
#'
#' @references 
#' University of Pittsburgh (2019). \emph{Research Impact and Metrics: Author metrics.} Retrieved from \url{https://pitt.libguides.com/bibliometricIndicators/AuthorMetrics}.
#' 
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
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
  years_publishing <- as.numeric(substr(Sys.Date(), 1, 4)) - min(data$year, na.rm = TRUE) 
  Mindex <- round(Hindex/years_publishing, digits = 3)
  return(Mindex)
}



#' This function calculates the i10 index of a species.
#' i10 index counts all of the publications with 10 or more citations.
#'
#' @title Species i10 index
#' 
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return i10 index.
#' @export
#' 
#' @references 
#' Cornell University (2019). \emph{i10-index.} Retrieved from \url{https://guides.library.cornell.edu/c.php?g=32272&p=203393}.
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
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
#' @param data The dataframe generated from \code{\link{Fetch}}.
#'
#' @return H5 index.
#' @export
#'
#' @references 
#' Suzuki, H. (2012). \emph{Google Scholar Metrics for Publications.} Retrieved from \url{https://scholar.googleblog.com/2012/04/google-scholar-metrics-for-publications.html}.
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia", 
#'                 species = "penicillata")
#' }
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
#' @param data The dataframe generated from \code{\link{Fetch}}.
#' @param date The lower limit of the timeframe.
#'
#' @return H-index of the given time period.
#' @export 
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
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
#' @param data The dataframe generated from \code{\link{Fetch}}.
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#'
#' @return A datarame of all of the indices in the package.
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
#' Allindices(Woylie,
#'            genus = "Bettongia",
#'            species = "penicillata")
#' 
Allindices <- function(data,
                       genus,
                       species) {
  if (all(data$citations == 0)) {
    zeroIndex <- data.frame(genus_species = paste0(genus, " ", species),
                            species = paste0(species),
                            genus = paste0(genus),
                            publications = 0,
                            citations = 0,
                            journals = 0,
                            years_publishing = NA,
                            h = 0,
                            m = 0,
                            i10 = 0,
                            h5 = 0)
    return(zeroIndex)
  } else {
    combine <- data.frame(paste0(genus, " ", species),
                          paste0(species),
                          paste0(genus),
                          TotalPub(data),
                          TotalCite(data),
                          TotalJournals(data),
                          YearsPublishing(data),
                          SpHindex(data),
                          SpMindex(data),
                          Spi10(data),
                          SpH5(data))
    combine[is.na(combine)] <- 0 #replace NA values with 0
    colnames(combine) <- c("genus_species",
                           "species",
                           "genus",
                           "publications",
                           "citations",
                           "journals",
                           "years_publishing",
                           "h",
                           "m",
                           "i10",
                           "h5")
    print_allindices(data,genus,species)
    return(combine)
  } 
}  
  
#' Print all indices output
#' Title only.
#' 
#' @title Print all indices output
#'
#' @param data from allindices
#'
#' @noRd
#'
print_allindices<-function(data,genus,species){
  cat("\n", genus, species, "\n",
      TotalPub(data), "publications", "\n",
      TotalCite(data), "citations", "\n",
      TotalJournals(data), "journals", "\n",
      YearsPublishing(data), "years of publishing", "\n",
      "h:", SpHindex(data), "\n",
      "m:", SpMindex(data), "\n",
      "i10:", Spi10(data), "\n",
      "h5:", SpH5(data), "\n")
}



#' Plots the indices of a single species or combined.
#' 
#' @title Index plot
#'
#' @param data The dataframe generated from \code{\link{Allindices}}.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' Quokka <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Setonix",
#'                 species = "brachyurus")
#' Platypus <- Fetch(db = "scopus",
#'                   search = "tak",
#'                   genus = "Ornithorhynchus",
#'                   species = "anatinus")
#' Koala <- Fetch(db = "scopus",
#'                search = "tak",
#'                genus = "Phascolarctos",
#'                species = "cinereus")
#' }
#' W <- Allindices(Woylie,
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' Q <- Allindices(Quokka,
#'                 genus = "Setonix",
#'                 species = "brachyurus")
#' P <- Allindices(Platypus,
#'                 genus = "Ornithorhynchus",
#'                 species = "anatinus")
#' K <- Allindices(Koala,
#'                 genus = "Phascolarctos",
#'                 species = "cinereus")
#' CombineSp <- dplyr::bind_rows(W, Q, P, K)
#' plotAllindices(CombineSp)
#' 
plotAllindices <- function(data) {
  facet_data <- tidyr::pivot_longer(data, cols = h:h5,
                        names_to = "index",
                        values_to = "value")
  facet_data$index <- factor(facet_data$index,
                             levels = c("h", "m", "i10", "h5"),
                             labels = c("h-index", "m-index", "i10 index", "h5 index"))
  p <- ggplot2::ggplot(data = facet_data,
                       ggplot2::aes(x = genus_species,
                                    y = value,
                                    colour = genus_species)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(colour = "Species") +
    spindex_plot_theme() +
    ggplot2::facet_wrap(~index,
                        scales = "free",
                        nrow = 2,
                        ncol = 2)
  return(p)
}



#' Extracts the year of each publication of the output from any of the Fetch functions and counts the number of publications each year.
#'
#' @title Extract year
#' @param data Output from any of the fetch function.
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#'
#' @return A dataframe with the year and frequency of the publications
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' }
#' getYear(data = Woylie,
#'         genus = "Bettongia",
#'         species = "penicillata")
#' 
getYear <- function(data, genus, species) {
  data$year <- as.numeric(substr(data$cover_date, 1, 4))
  output_by_year <- data.frame(table(data$year))
  names(output_by_year)[names(output_by_year)=="Var1"] <- "Year"
  output_by_year$Year <- as.numeric(as.character(output_by_year$Year))
  output_by_year <- tidyr::complete(output_by_year, Year = min(Year):max(Year), fill = list(Freq = 0))
  output_by_year$spp <- paste(genus, species)
  return(output_by_year)
}



#' Plots the publication by year of a single species or combined.
#' 
#' @title Publication plot
#'
#' @param data The dataframe generated from \code{\link{getYear}}.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' Woylie <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Bettongia",
#'                 species = "penicillata")
#' Quokka <- Fetch(db = "scopus",
#'                 search = "tak",
#'                 genus = "Setonix",
#'                 species = "brachyurus")
#' Platypus <- Fetch(db = "scopus",
#'                   search = "tak",
#'                   genus = "Ornithorhynchus",
#'                   species = "anatinus")
#' Koala <- Fetch(db = "scopus",
#'                search = "tak",
#'                genus = "Phascolarctos",
#'                species = "cinereus")
#' }
#' extract_year_W <- getYear(data = Woylie,
#'                           genus = "Bettongia",
#'                           species = "penicillata")
#' extract_year_Q <- getYear(data = Quokka,
#'                           genus = "Setonix",
#'                           species = "brachyurus")
#' extract_year_P <- getYear(data = Platypus,
#'                           genus = "Ornithorhynchus",
#'                           species = "anatinus")
#' extract_year_K <- getYear(data = Koala,
#'                           genus = "Phascolarctos",
#'                           species = "cinereus")
#' Combine_pub <- rbind(extract_year_W,
#'                      extract_year_Q,
#'                      extract_year_P,
#'                      extract_year_K)
#' plotPub(Combine_pub)
#' 
plotPub <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = as.numeric(as.character(Year)),
                                     y = Freq,
                                     group = spp,
                                     colour = spp)) +
    ggplot2::geom_line(size = 0.5,
                       alpha = 0.8) +
    ggplot2::geom_point(size = 1,
                        alpha = 0.8) +
    ggplot2::labs(x = "Year",
                  y = "Number of articles",
                  colour = "Species") +
    sppub_plot_theme()
}



#' Creates a query string for Scopus to make functions with a query cleaner.
#' Title only.
#' 
#' @title Query string for Scopus
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_T_scopus <- function(genus,
                                         species = NULL,
                                         synonyms,
                                         additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE("', genus, ' ', species = paste0(species), '")'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TITLE("', genus, ' ', species = paste0(species), '" OR "', synonyms[1], '"')
    if (length(synonyms)==1) {
      return(paste0(temp_string, ')'))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0(temp_string, ' OR "', synonyms[i], '"')
      }
      return(paste0(temp_string, ')'))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('TITLE(("', genus, ' ', species = paste0(species), '" OR "',
                  synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Scopus to make functions with a query cleaner.
#' Title, abstract, or keywords.
#' 
#' @title Query string for Scopus
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_TAK_scopus <- function(genus,
                                           species = NULL,
                                           synonyms,
                                           additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY("', genus, ' ', species = paste0(species), '")'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TITLE-ABS-KEY("', genus, ' ', species = paste0(species), '" OR "', synonyms[1], '"')
    if (length(synonyms)==1) {
      return(paste0(temp_string, ')'))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0(temp_string, ' OR "', synonyms[i], '"')
      }
      return(paste0(temp_string, ')'))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY(("', genus, ' ', species = paste0(species), '" OR "',
                  synonyms, '") AND ', additionalkeywords, ')'))
  } 
}




#' Creates a query string for Web of Science to make functions with a query cleaner.
#' Title only.
#'
#' @title Query string for Web of Science
#' 
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_T_wos <- function(genus,
                                      species = NULL,
                                      synonyms,
                                      additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = "', genus, ' ', species = paste0(species), '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = ("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TI = ("', genus, ' ', species = paste0(species), '" OR "', synonyms[1], '")')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('TI = ("', genus, ' ', species = paste0(species), '" OR "', synonyms[i], '")')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('TI = (("', genus, ' ', species = paste0(species), '" OR "',
                  synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Web of Science to make functions with a query cleaner.
#' Title, abstract, or keywords.
#'
#' @title Query string for Web of Science
#' 
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_TAK_wos <- function(genus,
                                        species = NULL,
                                        synonyms,
                                        additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = "', genus, ' ', species = paste0(species),
                  '" OR AB = "', genus, ' ', species = paste0(species),
                  '" OR AK = "', genus, ' ', species = paste0(species), '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = ("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')',
                  ' OR AB = ("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')',
                  ' OR AK = ("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TI = ("', genus, ' ', species = paste0(species), '" OR "', synonyms[1], '")',
                          ' OR AB = ("', genus, ' ', species = paste0(species), '" OR "', synonyms[1], '")',
                          ' OR AK = ("', genus, ' ', species = paste0(species), '" OR "', synonyms[1], '")')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('TI = ("', genus, ' ', species = paste0(species), '" OR "', synonyms[i], '")',
                              ' OR AB = ("', genus, ' ', species = paste0(species), '" OR "', synonyms[i], '")',
                              ' OR AK = ("', genus, ' ', species = paste0(species), '" OR "', synonyms[i], '")')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('TI = (("', genus, ' ', species = paste0(species), '" OR "',
                  synonyms, '") AND ', additionalkeywords, ')',
                  ' OR AB = (("', genus, ' ', species = paste0(species), '" OR "',
                  synonyms, '") AND ', additionalkeywords, ')',
                  ' OR AK = (("', genus, ' ', species = paste0(species), '" OR "',
                  synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for BASE to make functions with a query cleaner.
#' Title only.
#'
#' @title Query string for BASE
#' 
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_T_base <- function(genus,
                                       species = NULL,
                                       synonyms,
                                       additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:"', genus, ' ', species = paste0(species), '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('dctitle:("', genus, ' ', species = paste0(species), '" OR ', synonyms[1])
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
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('dctitle:(("', genus, ' ', species = paste0(species), '" OR ',
                  synonyms, ') AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for BASE to make functions with a query cleaner.
#' Title, abstract, or keywords.
#'
#' @title Query string for BASE
#' 
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_TAK_base <- function(genus,
                                         species = NULL,
                                         synonyms,
                                         additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:"', genus, ' ', species = paste0(species), '"',
                  ' OR dcdescription:"', genus, ' ', species = paste0(species), '"',
                  ' OR dcsubject:"', genus, ' ', species = paste0(species), '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')',
                  ' OR dcdescription:("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')',
                  ' OR dcsubject:("', genus, ' ', species = paste0(species), '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('dctitle:("', genus, ' ', species = paste0(species), '" OR ', synonyms[1], ')',
                          ' OR dcdescription:("', genus, ' ', species = paste0(species), '" OR ', synonyms[1], ')',
                          ' OR dcsubject:("', genus, ' ', species = paste0(species), '" OR ', synonyms[1], ')')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('dctitle:("', genus, ' ', species = paste0(species), '" OR ',
                              synonyms[i], ')',
                              ' OR dcdescription:("', genus, ' ', species = paste0(species), '" OR ',
                              synonyms[i], ')',
                              ' OR dcsubject:("', genus, ' ', species = paste0(species), '" OR ',
                              synonyms[i], ')')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('dctitle:(("', genus, ' ', species = paste0(species), '"', ' OR ',
                  synonyms, ') AND ', additionalkeywords, ')',
                  ' OR dcdescription:(("', genus, ' ', species = paste0(species), '"', ' OR ',
                  synonyms, ') AND ', additionalkeywords, ')',
                  ' OR dcsubject:(("', genus, ' ', species = paste0(species), '"', ' OR ',
                  synonyms, ') AND ', additionalkeywords, ')'))
  } 
}



#' Function to parse request query to the Scopus API.
#' Title only.
#' 
#' @title Scopus query; title
#'
#' @param request Scopus request parsed to the API.
#'
#' @noRd
#' 
scopus_request_t <- function(request, species = NULL, ...) {
  rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus,
                                                                     species = paste0(species),
                                                                     synonyms,
                                                                     additionalkeywords),
                                        paste0(request)),
                         api_key = apikey,
                         verbose = TRUE,
                         wait_time = 3)
}



#' Function to parse request query to the Scopus API.
#' Title, abstract, or keywords.
#' 
#' @title Scopus query; title 
#'
#' @param request Scopus request parsed to the API.
#'
#' @noRd
#' 
scopus_request_tak <- function(request, species = NULL, ...) {
  rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus,
                                                                       species = paste0(species),
                                                                       synonyms,
                                                                       additionalkeywords),
                                        paste0(request)),
                         api_key = apikey,
                         verbose = TRUE,
                         wait_time = 3)
}



#' Theme for \code{\link{plotAllindices}}
#'
#' @title Theme for indices plot
#' 
#' @importFrom ggplot2 element_blank element_grob element_line element_rect element_render element_text
#'
#' @noRd
#' 
spindex_plot_theme <- function() {
  ggplot2::theme(title = element_blank(),
                 axis.title.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.line.x = element_line(colour = "grey20"),
                 axis.title.y = element_blank(),
                 axis.text.y = element_text(size = 10),
                 strip.background = element_rect(fill = "white"),
                 strip.text = element_text(size = 16,
                                           face = "bold",
                                           hjust = -0.02),
                 legend.title = element_text(size = 12,
                                             face = "bold"),
                 legend.key = element_rect(fill = "white"),
                 plot.background = element_rect(fill = "white"),
                 panel.background = element_rect(fill = "white"),
                 panel.grid.major.y = element_line(colour = "grey90"),
                 panel.grid.minor.y = element_line(colour = "grey90",
                                                   linetype = "longdash"),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.x = ggplot2::element_blank())
}



#' Theme for \code{\link{plotPub}}
#'
#' @title Theme for publications plot
#' 
#' @importFrom ggplot2 element_blank element_grob element_line element_rect element_render element_text
#'
#' @noRd
#' 
sppub_plot_theme <- function() {
  ggplot2::theme(title = element_blank(),
                 axis.title.x = element_text(size = 12,
                                             face = "bold"),
                 axis.text.x = element_text(size = 10),
                 axis.ticks.x = element_blank(),
                 axis.line.x = element_line(colour = "grey20"),
                 axis.title.y = element_text(size = 12,
                                             face = "bold"),
                 axis.text.y = element_text(size = 10),
                 legend.key = element_rect(fill = "white"),
                 plot.background = element_rect(fill = "white"),
                 panel.background = element_rect(fill = "white"),
                 panel.grid.major.y = element_line(colour = "grey90"),
                 panel.grid.minor.y = element_line(colour = "grey90",
                                                   linetype = "longdash"),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.x = ggplot2::element_blank())
}



#' Matches and checks species names on Catalogue of Life (Col), Integrated Taxonomic Information System (ITIS), National Center for Biotechnology Information (NCBI), and Encyclopedia of Life (EoL).
#'
#' @title Species check
#' 
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#'
#' @noRd
#' 
sp_check <- function(genus,
                     species = NULL) {
  findname <- taxize::gnr_resolve(sci = paste(genus, species = paste(species)),
                                  data_source_ids = list("1", "3", "4", "12")) #check if the species exist
  if (length(findname>0)) {
    findname <- findname[order(-findname$score),]
  } else {
    stop("Genus or species not found on CoL, ITIS, NCBI, or EoL. Please check your spelling and try again.")
  }
  if (findname$score[1]>=0.75) {
    print(paste("Species found on CoL, ITIS, NCBI, or EoL."))
  } else {
    stop("Genus or species not found on CoL, ITIS, NCBI, or EoL. Please check your spelling and try again.")
  }
}


#' Koala.
#'
#' A dataset with some literature on Koalas
#'
#' @format A data frame :
#' \describe{
#'   \item{citations}{number of cites}
#'   \item{journal}{journal}
#'   \item{authkeywords}{authkeywords}
#'   \item{cover_date}{cover_date}
#'   \item{cover_display_date}{cover_display_date}
#'   \item{dc_creator}{dc_creator}
#'   \item{dc_description}{dc_description}
#'   \item{dc_identifier}{dc_identifier}
#'   \item{description}{description}
#'   \item{doi}{doi}
#'   \item{eid}{eid}
#'   \item{pii}{pii}
#'   \item{prism_aggregationType}{prism_aggregationType}
#'   \item{prism_eIssn}{prism_eIssn}
#'   \item{prism_issn}{prism_issn}
#'   \item{prism_pageRange}{prism_pageRange}
#'   \item{prism_url}{prism_url}
#'   \item{source_id}{source_id}
#'   \item{subtype}{subtype}
#'   \item{title}{title}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"Koala"


#' Platypus
#'
#' A dataset with some literature on Platypus
#'
#' @format A data frame :
#' \describe{
#'   \item{citations}{number of cites}
#'   \item{journal}{journal}
#'   \item{authkeywords}{authkeywords}
#'   \item{cover_date}{cover_date}
#'   \item{cover_display_date}{cover_display_date}
#'   \item{dc_creator}{dc_creator}
#'   \item{dc_description}{dc_description}
#'   \item{dc_identifier}{dc_identifier}
#'   \item{description}{description}
#'   \item{doi}{doi}
#'   \item{eid}{eid}
#'   \item{pii}{pii}
#'   \item{prism_aggregationType}{prism_aggregationType}
#'   \item{prism_eIssn}{prism_eIssn}
#'   \item{prism_issn}{prism_issn}
#'   \item{prism_pageRange}{prism_pageRange}
#'   \item{prism_url}{prism_url}
#'   \item{source_id}{source_id}
#'   \item{subtype}{subtype}
#'   \item{title}{title}
#'   ...
#' }
#' @source WOS
"Platypus"

#' Quokka
#'
#' A dataset with some literature on Quokka
#'
#' @format A data frame :
#' \describe{
#'   \item{citations}{number of cites}
#'   \item{journal}{journal}
#'   \item{authkeywords}{authkeywords}
#'   \item{cover_date}{cover_date}
#'   \item{cover_display_date}{cover_display_date}
#'   \item{dc_creator}{dc_creator}
#'   \item{dc_description}{dc_description}
#'   \item{dc_identifier}{dc_identifier}
#'   \item{description}{description}
#'   \item{doi}{doi}
#'   \item{eid}{eid}
#'   \item{pii}{pii}
#'   \item{prism_aggregationType}{prism_aggregationType}
#'   \item{prism_eIssn}{prism_eIssn}
#'   \item{prism_issn}{prism_issn}
#'   \item{prism_pageRange}{prism_pageRange}
#'   \item{prism_url}{prism_url}
#'   \item{source_id}{source_id}
#'   \item{subtype}{subtype}
#'   \item{title}{title}
#'   ...
#' }
#' @source WOS
"Quokka"

#' Woylie
#'
#' A dataset with some literature on Woylie
#'
#' @format A data frame :
#' \describe{
#'   \item{citations}{number of cites}
#'   \item{journal}{journal}
#'   \item{authkeywords}{authkeywords}
#'   \item{cover_date}{cover_date}
#'   \item{cover_display_date}{cover_display_date}
#'   \item{dc_creator}{dc_creator}
#'   \item{dc_description}{dc_description}
#'   \item{dc_identifier}{dc_identifier}
#'   \item{description}{description}
#'   \item{doi}{doi}
#'   \item{eid}{eid}
#'   \item{pii}{pii}
#'   \item{prism_aggregationType}{prism_aggregationType}
#'   \item{prism_eIssn}{prism_eIssn}
#'   \item{prism_issn}{prism_issn}
#'   \item{prism_pageRange}{prism_pageRange}
#'   \item{prism_url}{prism_url}
#'   \item{source_id}{source_id}
#'   \item{subtype}{subtype}
#'   \item{title}{title}
#'   ...
#' }
#' @source WOS
"Woylie"

#' languages
#'
#' A dataset with some literature on languages
#'
#' @format A data frame :
#' \describe{
#'   \item{language}{names of languages}
#'   ...
#' }
#' @source somewhere?
"languages"


