#' This is a wrapper function for \code{\link{CountSpT_scopus}}, \code{\link{CountSpT_wos}}, \code{\link{CountSpT_base}}, and \code{\link{CountSpT_lens}}.
#' 
#' @title CountSpT wrapper
#'
#' @param db Literature database. Scopus ("scopus") or Web of Science ("wos") or Base ("base") or Lens ("lens").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers. Lens only.
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#'
#' @examples
#' \dontrun{
#' CountSpT("scopus", genus = "Osphranter", species = "rufus")
#' }
#' \dontrun{
#' CountSpT("scopus", genus = "Osphranter", species = "rufus", synonyms = "Macropus rufus", additionalkeywords = "conserv*")
#' }
CountSpT <- function(db, genus, species, synonyms, additionalkeywords, size = 50000) {
  if (db == "scopus") {
    countsp <- CountSpT_scopus(genus, species, synonyms, additionalkeywords)
  } else if (db == "wos") {
    countsp <- CountSpT_wos(genus, species, synonyms, additionalkeywords)
  } else if (db == "base") {
    countsp <- CountSpT_base(genus, species, synonyms, additionalkeywords)
  } else if (db == "lens") {
    countsp <- CountSpT_lens(genus, species, synonyms, additionalkeywords, size)
  }
  return(countsp)
}



#' This is a wrapper function for \code{\link{CountSpTAK_scopus}}, \code{\link{CountSpTAK_wos}}, \code{\link{CountSpTAK_base}}, and \code{\link{CountSpTAk_lens}}.
#' 
#' @title CountSpTAK wrapper
#'
#' @param db Literature database. Scopus ("scopus") or Web of Science ("wos") or Base ("base") or Lens ("lens").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers. Lens only.
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#'
#' @examples
#' \dontrun{
#' CountSpTAK("scopus", genus = "Osphranter", species = "rufus")
#' }
#' \dontrun{
#' CountSpTAK("scopus", genus = "Osphranter", species = "rufus", synonyms = "Macropus rufus", additionalkeywords = "conserv*")
#' }
CountSpTAK <- function(db, genus, species, synonyms, additionalkeywords, size = 50000) {
  if (db == "scopus") {
    countsp <- CountSpTAK_scopus(genus, species, synonyms, additionalkeywords)
  } else if (db == "wos") {
    countsp <- CountSpTAK_wos(genus, species, synonyms, additionalkeywords)
  } else if (db == "base") {
    countsp <- CountSpTAK_base(genus, species, synonyms, additionalkeywords)
  } else if (db == "lens") {
    countsp <- CountSpTAK_lens(genus, species, synonyms, additionalkeywords, size)
  }
  return(countsp)
}



#' This is a wrapper function for \code{\link{FetchSpT_scopus}}, \code{\link{FetchSpT_wos}}, \code{\link{FetchSpT_base}}, and \code{\link{FetchSpT_lens}}.
#' 
#' @title FetchSpT wrapper
#'
#' @param db Literature database. Scopus ("scopus") or Web of Science ("wos") or or Lens ("lens").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param language Language of the paper; default is 0, enter 1 to retrieve the variable. Scopus only.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers. Lens only.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @export
#'
#' @examples
#' \dontrun{
#' FetchSpT("scopus", genus = "Osphranter", species = "rufus")
#' }
#' \dontrun{
#' FetchSpT("scopus", genus = "Osphranter", species = "rufus", synonyms = "Macropus rufus", additionalkeywords = "conserv*")
#' }
FetchSpT <- function(db, genus, species, synonyms, additionalkeywords, language = 0, size = 50000) {
  if (db == "scopus") {
    fetchsp <- FetchSpT_scopus(genus, species, synonyms, additionalkeywords, language)
  } else if (db == "wos") {
    fetchsp <- FetchSpT_wos(genus, species, synonyms, additionalkeywords)
  } else if (db == "lens") {
    fetchsp <- FetchSpT_lens(genus, species, synonyms, additionalkeywords, size)
  }
  return(fetchsp)
}



#' This is a wrapper function for \code{\link{FetchSpTAK_scopus}}, \code{\link{FetchSpTAK_wos}}, \code{\link{FetchSpTAK_base}}, and \code{\link{FetchSpTAK_lens}}.
#' 
#' @title FetchSpTAK wrapper
#'
#' @param db Literature database. Scopus ("scopus") or Web of Science ("wos") or Lens ("lens").
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param language Language of the paper; default is 0, enter 1 to retrieve the variable. Scopus only.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers. Lens only.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @export
#'
#' @examples
#' \dontrun{
#' FetchSpTAK("scopus", genus = "Osphranter", species = "rufus")
#' }
#' \dontrun{
#' FetchSpTAK("scopus", genus = "Osphranter", species = "rufus", synonyms = "Macropus rufus", additionalkeywords = "conserv*")
#' }
FetchSpTAK <- function(db, genus, species, synonyms, additionalkeywords, language = 0, size = 50000) {
  if (db == "scopus") {
    fetchsp <- FetchSpTAK_scopus(genus, species, synonyms, additionalkeywords, language)
  } else if (db == "wos") {
    fetchsp <- FetchSpTAK_wos(genus, species, synonyms, additionalkeywords)
  } else if (db == "lens") {
    fetchsp <- FetchSpTAK_lens(genus, species, synonyms, additionalkeywords, size)
  }
  return(fetchsp)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title only.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from Scopus - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
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
#' CountSpT_scopus("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT_scopus("bettongia", "penicillata")
#' }
#' \dontrun{
#' CountSpT_scopus("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT_scopus("bettongia", "penicillata", "conserv*")
#' }
CountSpT_scopus <- function(genus, species, synonyms, additionalkeywords, datatype = "application/xml") {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  ) 
  response <- httr::GET("http://api.elsevier.com/content/search/scopus",
                        query = list(apiKey = apikey,
                                     query = create_query_string_T_scopus(genus, species, synonyms, additionalkeywords),
                                     httpAccept = "application/xml")) #format the URL to be sent to the API
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response) #parse the data to extract values
  resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
  return(resultCount)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title, abstract, and keywords.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from Scopus - title, abstract, and keywords
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
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
#' CountSpTAK_scopus("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK_scopus("bettongia", "penicillata")
#' }
#' \dontrun{
#' CountSpTAK_scopus("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK_scopus("bettongia", "penicillata")
#' }
CountSpTAK_scopus <- function(genus, species, synonyms, additionalkeywords, datatype = "application/xml") {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  ) 
  response <- httr::GET("http://api.elsevier.com/content/search/scopus",
                        query = list(apiKey = apikey,
                                     query = create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords),
                                     httpAccept = "application/xml")) #format the URL to be sent to the API
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response) #parse the data to extract values
  resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
  return(resultCount)
}



#' Title
#'
#' @param genus 
#' @param synonyms 
#' @param additionalkeywords 
#' @param datatype 
#'
#' @return
#' @export
#'
#' @examples
CountGenusT_scopus <- function(genus, synonyms, additionalkeywords, datatype = "application/xml") {
  findname <- taxize::gnr_resolve(sci = c(genus)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Genus found on the Encyclopedia of Life."))
  ) 
  response <- httr::GET("http://api.elsevier.com/content/search/scopus",
                        query = list(apiKey = apikey,
                                     query = create_query_string_T_scopus_genus(genus, synonyms, additionalkeywords),
                                     httpAccept = "application/xml")) #format the URL to be sent to the API
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response) #parse the data to extract values
  resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
  return(resultCount)
}



#' Title
#'
#' @param genus 
#' @param synonyms 
#' @param additionalkeywords 
#' @param datatype 
#'
#' @return
#' @export
#'
#' @examples
CountGenusTAK_scopus <- function(genus, synonyms, additionalkeywords, datatype = "application/xml") {
  findname <- taxize::gnr_resolve(sci = c(genus)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Genus found on the Encyclopedia of Life."))
  ) 
  response <- httr::GET("http://api.elsevier.com/content/search/scopus",
                        query = list(apiKey = apikey,
                                     query = create_query_string_TAK_scopus_genus(genus, synonyms, additionalkeywords),
                                     httpAccept = "application/xml")) #format the URL to be sent to the API
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
#' @export 
#'
#' @examples
#' \dontrun{
#' FetchSpT_scopus("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT_scopus("bettongia", "penicillata")
#' }
#' \dontrun{
#' FetchSpT_scopus("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT_scopus("bettongia", "penicillata", "conserv*")
#' }
FetchSpT_scopus <- function(genus, species, synonyms, additionalkeywords, language = 0) {
  count <- CountSpT_scopus(genus, species, synonyms, additionalkeywords) #check the number of records
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
                                         query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords),
                                                        " AND LANGUAGE(", lang$language[j], ")"),
                                         httpAccept = "application/xml")) #format the URL to be sent to the API
      httr::stop_for_status(response) #pass any HTTP errors to the R console
      response_data <- XML::xmlParse(response) #parse the data to extract values
      resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
      if (resultCount > 0) {
        #search begins
        search2020 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 2019 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf = data.frame()
        for (i in 2019:1990) {
          searchloop <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR = ", i, " AND LANGUAGE(", lang$language[j], ")")),
                                               api_key = apikey,
                                               verbose = TRUE,
                                               wait_time = 3)
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf, searchlooplist)
        }
        search1985 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1984 AND PUBYEAR < 1990 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1979 AND PUBYEAR < 1985 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1974 AND PUBYEAR < 1980 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1969 AND PUBYEAR < 1975 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR < 1970 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
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
        search <- rscopus::scopus_search(query = create_query_string_T_scopus(genus, species, synonyms, additionalkeywords),
                                         api_key = apikey,
                                         verbose = TRUE,
                                         max_count = step_size,
                                         start = step_size*i,
                                         wait_time = 3)
        start_record <- as.numeric(summary(search)[1,1]) #move the pointer of starting record for each iteration to a new value
        searchdf <- rscopus::entries_to_citation_df(search$entries)
        list <- data.frame(searchdf)
        datalist <- dplyr::bind_rows(datalist, list)
        #loop ends
      }} else {
        #search begins
        search2020 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 2019")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf = data.frame()
        for (i in 2019:1990) {
          searchloop <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR = ", i)),
                                               api_key = apikey,
                                               verbose = TRUE,
                                               wait_time = 3)
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf, searchlooplist)
        }
        search1985 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1984 AND PUBYEAR < 1990")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1979 AND PUBYEAR < 1985")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1974 AND PUBYEAR < 1980")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1969 AND PUBYEAR < 1975")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- rscopus::scopus_search(query = paste0(create_query_string_T_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR < 1970")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)  
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        datalist <- dplyr::bind_rows(search2020df, searchloopdf, search1985df, search1980df, search1975df, search1970df, search_olddf)
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
#' @export 
#'
#' @examples
#' \dontrun{
#' FetchSpTAK_scopus("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpTAK_scopus("bettongia", "penicillata")
#' }
#' \dontrun{
#' FetchSpTAK_scopus("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpTAK_scopus("bettongia", "penicillata", "conserv*")
#' }
FetchSpTAK_scopus <- function(genus, species, synonyms, additionalkeywords, language = 0) {
  count <- CountSpTAK_scopus(genus, species, synonyms, additionalkeywords) #check the number of records
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
                                         query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords),
                                                        " AND LANGUAGE(", lang$language[j], ")"),
                                         httpAccept = "application/xml")) #format the URL to be sent to the API
      httr::stop_for_status(response) #pass any HTTP errors to the R console
      response_data <- XML::xmlParse(response) #parse the data to extract values
      resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue)) #get the total number of search results for the string
      if (resultCount > 0) {
        #search begins
        search2020 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 2019 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf = data.frame()
        for (i in 2019:1990) {
          searchloop <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR = ", i, " AND LANGUAGE(", lang$language[j], ")")),
                                               api_key = apikey,
                                               verbose = TRUE,
                                               wait_time = 3)
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf, searchlooplist)
        }
        search1985 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1984 AND PUBYEAR < 1990 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1979 AND PUBYEAR < 1985 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1974 AND PUBYEAR < 1980 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1969 AND PUBYEAR < 1975 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR < 1970 AND LANGUAGE(", lang$language[j], ")")),
                                             api_key = apikey,
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
        search <- rscopus::scopus_search(query = create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords),
                                         api_key = apikey,
                                         verbose = TRUE,
                                         max_count = step_size,
                                         start = step_size*i,
                                         wait_time = 3)
        start_record <- as.numeric(summary(search)[1,1]) #move the pointer of starting record for each iteration to a new value
        searchdf <- rscopus::entries_to_citation_df(search$entries)
        list <- data.frame(searchdf)
        datalist <- dplyr::bind_rows(datalist, list)
        #loop ends
      }} else {
        #search begins
        search2020 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 2019")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search2020df <- rscopus::entries_to_citation_df(search2020$entries)
        searchloopdf = data.frame()
        for (i in 2019:1990) {
          searchloop <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR = ", i)),
                                               api_key = apikey,
                                               verbose = TRUE,
                                               wait_time = 3)
          searchlooplist <- rscopus::entries_to_citation_df(searchloop$entries)
          searchloopdf <- dplyr::bind_rows(searchloopdf, searchlooplist)
        }
        search1985 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1984 AND PUBYEAR < 1990")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1985df <- rscopus::entries_to_citation_df(search1985$entries)
        search1980 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1979 AND PUBYEAR < 1985")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)
        search1980df <- rscopus::entries_to_citation_df(search1980$entries)
        search1975 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1974 AND PUBYEAR < 1980")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1975df <- rscopus::entries_to_citation_df(search1975$entries)
        search1970 <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR > 1969 AND PUBYEAR < 1975")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)  
        search1970df <- rscopus::entries_to_citation_df(search1970$entries)
        search_old <- rscopus::scopus_search(query = paste0(create_query_string_TAK_scopus(genus, species, synonyms, additionalkeywords), paste0(" AND PUBYEAR < 1970")),
                                             api_key = apikey,
                                             verbose = TRUE,
                                             wait_time = 3)  
        search_olddf <- rscopus::entries_to_citation_df(search_old$entries)
        datalist <- dplyr::bind_rows(search2020df, searchloopdf, search1985df, search1980df, search1975df, search1970df, search_olddf)
        #search ends  
      }
  }
  returned <- dim(datalist)[1]
  print(paste(returned, "records retrived in total."))
  return(datalist)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title only.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from Web of Science - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#'
#' @examples
#' \dontrun{
#' CountSpT_wos("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT_wos("bettongia", "penicillata")
#' }
#' \dontrun{
#' CountSpT_wos("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT_wos("bettongia", "penicillata", "conserv*")
#' }
CountSpT_wos <- function(genus, species, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  )
  count <- wosr::query_wos(query = create_query_string_T_wos(genus, species, synonyms, additionalkeywords),
                           sid = sid) 
  return(count)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title, abstract and author keywords.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from Web of Science - title, abstract and author keywords
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#'
#' @examples
#' \dontrun{
#' CountSpTAK_wos("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK_wos("bettongia", "penicillata")
#' }
#' \dontrun{
#' CountSpTAK_wos("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK_wos("bettongia", "penicillata", "conserv*")
#' }
CountSpTAK_wos <- function(genus, species, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  )
  count <- wosr::query_wos(query = create_query_string_TAK_wos(genus, species, synonyms, additionalkeywords),
                           sid = sid) 
  return(count)
}



#' Title
#'
#' @param genus 
#' @param synonyms 
#' @param additionalkeywords 
#'
#' @return
#' @export
#'
#' @examples
CountGenusT_wos <- function(genus, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Genus found on the Encyclopedia of Life."))
  )
  count <- wosr::query_wos(query = create_query_string_T_wos_genus(genus, synonyms, additionalkeywords),
                           sid = sid) 
  return(count)
}



#' Title
#'
#' @param genus 
#' @param synonyms 
#' @param additionalkeywords 
#'
#' @return
#' @export
#'
#' @examples
CountGenusTAK_wos <- function(genus, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Genus found on the Encyclopedia of Life."))
  )
  count <- wosr::query_wos(query = create_query_string_TAK_wos_genus(genus, synonyms, additionalkeywords),
                           sid = sid) 
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
#' @export
#'
#' @examples
#' \dontrun{
#' FetchSpT_wos("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT_wos("bettongia", "penicillata")
#' }
#' \dontrun{
#' FetchSpT_wos("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT_wos("bettongia", "penicillata", "conserv*")
#' }
FetchSpT_wos <- function(genus, species, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  )
  query <- wosr::pull_wos(query = create_query_string_T_wos(genus, species, synonyms, additionalkeywords),
                          sid = sid) 
  results <- rbindlist(query, fill = TRUE)
  results <- setDT(results)[, lapply(.SD, function(x) toString(na.omit(x))), by = ut]
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
#' @export
#'
#' @examples
#' \dontrun{
#' FetchSpTAK_wos("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpTAK_wos("bettongia", "penicillata")
#' }
#' \dontrun{
#' FetchSpTAK_wos("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpTAK_wos("bettongia", "penicillata", "conserv*")
#' }
FetchSpTAK_wos <- function(genus, species, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  )
  query <- wosr::pull_wos(query = create_query_string_TAK_wos(genus, species, synonyms, additionalkeywords),
                          sid = sid) 
  results <- rbindlist(query, fill = TRUE)
  results <- setDT(results)[, lapply(.SD, function(x) toString(na.omit(x))), by = ut]
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
#' It counts the publications with the binomial name in the title only.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from BASE - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#'
#' @examples
#' \dontrun{
#' CountSpT_wos("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT_wos("bettongia", "penicillata")
#' }
#' \dontrun{
#' CountSpT_wos("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT_wos("bettongia", "penicillata", "conserv*")
#' }
CountSpT_base <- function(genus, species, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  )
  response <- httr::GET("https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi",
                        query = list(func = "PerformSearch",
                                     query = create_query_string_T_base(genus, species, synonyms, additionalkeywords)))
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response)
  resultCount <- as.numeric(XML::xpathSApply(response_data, "//response/result/@numFound"))
  return(resultCount)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title, abstract and author keywords.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from BASE - title, abstract and author keywords
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#'
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#'
#' @examples
#' \dontrun{
#' CountSpTAK_base("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK_base("bettongia", "penicillata")
#' }
#' \dontrun{
#' CountSpTAK_base("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK_base("bettongia", "penicillata", "conserv*")
#' }
CountSpTAK_base <- function(genus, species, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  )
  response <- httr::GET("https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi",
                        query = list(func = "PerformSearch",
                                     query = create_query_string_TAK_base(genus, species, synonyms, additionalkeywords)))
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response)
  resultCount <- as.numeric(XML::xpathSApply(response_data, "//response/result/@numFound"))
  return(resultCount)
}



#' Title
#'
#' @param genus 
#' @param synonyms 
#' @param additionalkeywords 
#'
#' @return
#' @export
#'
#' @examples
CountGenusT_base <- function(genus, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus)) #check if the species exist
  if (findname$user_supplied_name[1] %in% findname$matched_name) {
    print("Genus found on the Encyclopedia of Life.")
  } else {stop("Genus missing from the Encyclopedia of Life.")}
  response <- httr::GET("https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi",
                        query = list(func = "PerformSearch",
                                     query = create_query_string_T_base_genus(genus, synonyms, additionalkeywords)))
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response)
  resultCount <- as.numeric(XML::xpathSApply(response_data, "//response/result/@numFound"))
  return(resultCount)
}



#' Title
#'
#' @param genus 
#' @param synonyms 
#' @param additionalkeywords 
#'
#' @return
#' @export
#'
#' @examples
CountGenusTAK_base <- function(genus, synonyms, additionalkeywords) {
  findname <- taxize::gnr_resolve(sci = c(genus)) #check if the species exist
  if (findname$user_supplied_name[1] %in% findname$matched_name) {
    print("Genus found on the Encyclopedia of Life.")
  } else {print("Genus missing from the Encyclopedia of Life.")}
  response <- httr::GET("https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi",
                        query = list(func = "PerformSearch",
                                     query = create_query_string_TAK_base_genus(genus, synonyms, additionalkeywords)))
  httr::stop_for_status(response) #pass any HTTP errors to the R console
  response_data <- XML::xmlParse(response)
  resultCount <- as.numeric(XML::xpathSApply(response_data, "//response/result/@numFound"))
  return(resultCount)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title only.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from Lens - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers.
#' 
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#'
#' @examples
#' \dontrun{
#' CountSpT_lens("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT_lens("bettongia", "penicillata")
#' }
#' \dontrun{
#' CountSpT_lens("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpT_lens("bettongia", "penicillata", "conserv*")
#' }
CountSpT_lens <- function(genus, species, synonyms, additionalkeywords, size = 50000) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  ) 
  response <- httr::POST(url = "https://api.lens.org/scholarly/search",
                         add_headers(.headers = c("Authorization" = token,
                                                  "Content-Type" = "application/json")),
                         body = create_query_string_T_lens(genus, species, synonyms, additionalkeywords, size))
  lens_content <- jsonlite::fromJSON(rawToChar(response$content))
  if (!is.null(lens_content$total)) {
    resultCount <- as.numeric(lens_content$total)
  } else {
    resultCount <- 0
  }
  return(resultCount)
}



#' This function counts the total number of search results.
#' It counts the publications with the binomial name in the title, abstract and author keywords.
#' A check will be conducted via \code{\link[taxize]{gnr_resolve}} to validate the genus and species names.
#' 
#' @title Search count from Lens - title, abstract and author keywords
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers.
#' @param token Lens token needed to access and download data from their database.
#' 
#' @return Search count of the species with the given \code{genus} and \code{species}.
#' @export
#' 
#' @references 
#' Chamberlain, S. & Szocs, E. (2013). taxize - taxonomic search and retrieval in R. \emph{F1000Research, 2}, 191.
#'
#' @examples
#' \dontrun{
#' CountSpTAK_lens("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK_lens("bettongia", "penicillata")
#' }
#' \dontrun{
#' CountSpTAK_lens("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' CountSpTAK_lens("bettongia", "penicillata", "conserv*")
#' }
CountSpTAK_lens <- function(genus, species, synonyms, additionalkeywords, size = 50000) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) 
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  ) 
  response <- httr::POST(url = "https://api.lens.org/scholarly/search",
                         add_headers(.headers = c("Authorization" = token,
                                                  "Content-Type" = "application/json")),
                         body = create_query_string_TAK_lens(genus, species, synonyms, additionalkeywords, size))
  lens_content <- jsonlite::fromJSON(rawToChar(response$content))
  if (!is.null(lens_content$total)) {
    resultCount <- as.numeric(lens_content$total)
  } else {
    resultCount <- 0
  }
  return(resultCount)
}



#' Title
#'
#' @param genus 
#' @param synonyms 
#' @param additionalkeywords 
#' @param size 
#'
#' @return
#' @export
#'
#' @examples
CountGenusT_lens <- function(genus, synonyms, additionalkeywords, size = 50000) {
  findname <- taxize::gnr_resolve(sci = c(genus)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Genus found on the Encyclopedia of Life."))
  ) 
  response <- httr::POST(url = "https://api.lens.org/scholarly/search",
                         add_headers(.headers = c("Authorization" = token,
                                                  "Content-Type" = "application/json")),
                         body = create_query_string_T_lens_genus(genus, synonyms, additionalkeywords, size))
  lens_content <- jsonlite::fromJSON(rawToChar(response$content))
  if (!is.null(lens_content$total)) {
    resultCount <- as.numeric(lens_content$total)
  } else {
    resultCount <- 0
  }
  return(resultCount)
}



#' Title
#'
#' @param genus 
#' @param synonyms 
#' @param additionalkeywords 
#' @param size 
#'
#' @return
#' @export
#'
#' @examples
CountGenusTAK_lens <- function(genus, synonyms, additionalkeywords, size = 50000) {
  findname <- taxize::gnr_resolve(sci = c(genus)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Genus found on the Encyclopedia of Life."))
  ) 
  response <- httr::POST(url = "https://api.lens.org/scholarly/search",
                         add_headers(.headers = c("Authorization" = token,
                                                  "Content-Type" = "application/json")),
                         body = create_query_string_TAK_lens_genus(genus, synonyms, additionalkeywords, size))
  lens_content <- jsonlite::fromJSON(rawToChar(response$content))
  if (!is.null(lens_content$total)) {
    resultCount <- as.numeric(lens_content$total)
  } else {
    resultCount <- 0
  }
  return(resultCount)
}



#' This function fetches citation information from Lens using genus and species name found in the title of the publications.
#' Duplicates are to be removed by the user after fetching the data.
#'
#' @title Fetch data from Lens - title only
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @export
#'
#' @examples
#' \dontrun{
#' FetchSpT_lens("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT_lens("bettongia", "penicillata")
#' }
#' \dontrun{
#' FetchSpT_lens("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpT_lens("bettongia", "penicillata", "conserv*")
#' }
FetchSpT_lens <- function(genus, species, synonyms, additionalkeywords, size = 50000) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  )
  results <- lens2r::get_scholarly_df(query = create_query_string_T_lens(genus, species, synonyms, additionalkeywords, size),
                                      token = token)
  #renaming columns
  names(results)[names(results) == "scholarly_citations_count"] <- "citations"
  names(results)[names(results) == "source.title"] <- "journal"
  names(results)[names(results) == "publication_type"] <- "description"
  names(results)[names(results) == "date_published"] <- "cover_date"
  #replacing NA with 0
  for (i in 1:nrow(results)) {
    if (is.na(results$citations[i])) {
      results$citations[i] <- 0
    }
  }
  #clean cover_date
  results$cover_date <- substr(results$cover_date, 1, 10)
  #showing final list of records
  returned <- nrow(results)
  print(paste(returned, "records retrived in total."))
  return(results)
}



#' This function fetches citation information from Lens using genus and species name found in the title, abstract, and keywords of the publications.
#' Duplicates are to be removed by the user after fetching the data.
#'
#' @title Fetch data from Lens - title, abstract, and keywords.
#'
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers.
#'
#' @return A dataframe of the species' citation records with the given \code{genus} and \code{species}.
#' @export
#'
#' @examples
#' \dontrun{
#' FetchSpTAK_lens("Bettongia", "penicillata")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpTAK_lens("bettongia", "penicillata")
#' }
#' \dontrun{
#' FetchSpTAK_lens("Bettongia", "penicillata", "conserv*")
#' 
#' #lower case letter in genus is also accepted and will return identical results
#' 
#' FetchSpTAK_lens("bettongia", "penicillata", "conserv*")
#' }
FetchSpTAK_lens <- function(genus, species, synonyms, additionalkeywords, size = 50000) {
  findname <- taxize::gnr_resolve(sci = c(genus, species)) #check if the species exist
  dplyr::case_when(
    findname$submitted_name %in% findname$matched_name ~ print(paste("Species found on the Encyclopedia of Life."))
  ) 
  results <- lens2r::get_scholarly_df(query = create_query_string_TAK_lens(genus, species, synonyms, additionalkeywords, size),
                                      token = token)
  #renaming columns
  names(results)[names(results) == "scholarly_citations_count"] <- "citations"
  names(results)[names(results) == "source.title"] <- "journal"
  names(results)[names(results) == "publication_type"] <- "description"
  names(results)[names(results) == "year_published"] <- "cover_date"
  #replacing NA with 0
  for (i in 1:nrow(results)) {
    if (is.na(results$citations[i])) {
      results$citations[i] <- 0
    }
  }
  #clean cover_date
  results$cover_date <- substr(results$cover_date, 1, 10)
  #showing final list of records
  returned <- nrow(results)
  print(paste(returned, "records retrived in total."))
  return(results)
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



#' This function calculates the total number of items for each document type.
#' 
#' @title Source type
#'
#' @param data The dataframe generated from \code{\link{FetchSpT}} or \code{\link{FetchSpTAK}}.
#'
#' @return A dataframe with each document and their counts.
#' @export
#'
#' @examples
#' data(Woylie)
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
  years_publishing <- as.numeric(substr(Sys.Date(), 1, 4)) - min(data$year, na.rm = TRUE) 
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
  years_publishing <- as.numeric(substr(Sys.Date(), 1, 4)) - min(data$year, na.rm = TRUE) 
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
#' @param sourcetype Source type; default is 0, enter 1 to add SourceType variables.
#'
#' @return A datarame of all of the indices in the package.
#' @export
#'
#' @examples
#' data(Woylie)
#' Allindices(Woylie, genus = "genus_name", species = "species_name", sourcetype = 0)
#' 
Allindices <- function(data, genus, species, sourcetype = 0) {
  if (sourcetype == 1 & all.equal(0, data$citations) == FALSE) {
    combine <- data.frame(paste0(genus, " ", species), paste0(species), paste0(genus), TotalPub(data), TotalCite(data),
                          TotalJournals(data), YearsPublishing(data), SpHindex(data), SpMindex(data), Spi10(data), SpH5(data))
    combine[is.na(combine)] <- 0 #replace NA values with 0
    combine_st <- cbind(combine, SourceType(data))
    colnames(combine_st) <- c("genus_species", "species", "genus","publications", "citations", "journals", "years_publishing",
                              "h", "m", "i10", "h5", names(SourceType(data)))
    return(combine_st)
  } else if (all.equal(0, data$citations) == TRUE) {
    zeroIndex <- data.frame(genus_species = paste0(genus, " ", species),
                            species = paste0(species),
                            genus = paste0(genus),
                            publications = 0, citations = 0, journals = 0, years_publishing = NA, h = 0, m = 0, i10 = 0, h5 = 0)
    return(zeroIndex)
  } else {
    combine <- data.frame(paste0(genus, " ", species), paste0(species), paste0(genus), TotalPub(data), TotalCite(data),
                          TotalJournals(data), YearsPublishing(data), SpHindex(data), SpMindex(data), Spi10(data), SpH5(data))
    combine[is.na(combine)] <- 0 #replace NA values with 0
    colnames(combine) <- c("genus_species", "species", "genus","publications", "citations", "journals", "years_publishing",
                           "h", "m", "i10", "h5")
    return(combine)
  } 
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



#' Plots the data of a single species or combined.
#' 
#' @title Plot
#'
#' @param data The dataframe generated from \code{\link{Allindices}}.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' W <- Allindices(Woylie, genus = "Bettongia", species = "penicillata")
#' Q <- Allindices(Quokka, genus = "Setonix", species = "brachyurus")
#' P <- Allindices(Platypus, genus = "Ornithorhynchus", species = "anatinus")
#' K <- Allindices(Koala, genus = "Phascolarctos", species = "cinereus")
#' CombineSp <- dplyr::bind_rows(W, Q, P, K)
#' plotAllindices(CombineSp)
#' 
plotAllindices <- function(data) {
  #h-index
  h_plot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = genus_species,
                                         y = h,
                                         colour = genus_species)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "h-index",
                  colour = "Species") +
    spindex_plots_theme()
  #m-index
  m_plot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = genus_species,
                                         y = m,
                                         colour = genus_species)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "m-index",
                  colour = "Species") +
    spindex_plots_theme()
  #i10
  i10_plot <- ggplot2::ggplot(data = data,
                              ggplot2::aes(x = genus_species,
                                           y = i10,
                                           colour = genus_species)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "i10 index",
                  colour = "Species") +
    spindex_plots_theme()
  #h5
  h5_plot <- ggplot2::ggplot(data = data,
                             ggplot2::aes(x = genus_species,
                                          y = h5,
                                          colour = genus_species)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "h5 index",
                  colour = "Species") +
    spindex_plots_theme()
  #combining the plots
  combine_plot <- ggpubr::ggarrange(h_plot, m_plot, i10_plot, h5_plot,
                                    common.legend = TRUE,
                                    legend = "right")
  return(combine_plot)
}



#' Creates a query string for Scopus to make functions with a query cleaner.
#' Title only; genus species.
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
create_query_string_T_scopus <- function(genus, species, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE("', genus, ' ', species, '")'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE("', genus, ' ', species, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TITLE("', genus, ' ', species, '" OR "', synonyms[1], '"')
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
    return(paste0('TITLE(("', genus, ' ', species, '" OR "', synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Scopus to make functions with a query cleaner.
#' Title, abstract, and keywords; genus species.
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
create_query_string_TAK_scopus <- function(genus, species, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY("', genus, ' ', species, '")'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY("', genus, ' ', species, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TITLE-ABS-KEY("', genus, ' ', species, '" OR "', synonyms[1], '"')
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
  if (!missing(additionalkeywords)&!missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY(("', genus, ' ', species, '" OR "', synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Scopus to make functions with a query cleaner.
#' Title only; genus.
#' 
#' @title Query string for Scopus
#'
#' @param genus Genus classification from the binomial name.
#' @param synonyms Alternate genus names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_T_scopus_genus <- function(genus, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE("', genus, '")'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE("', genus, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TITLE("', genus, '" OR "', synonyms[1], '"')
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
    return(paste0('TITLE(("', genus, '" OR "', synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Scopus to make functions with a query cleaner.
#' Title, abstract, and keywords; genus.
#'
#' @title Query string for Scopus
#'
#' @param genus Genus classification from the binomial name.
#' @param synonyms Alternate genus names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_TAK_scopus_genus <- function(genus, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY("', genus, '")'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TITLE-ABS-KEY("', genus, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TITLE-ABS-KEY("', genus, '" OR "', synonyms[1], '"')
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
    return(paste0('TITLE-ABS-KEY(("', genus, '" OR "', synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Web of Science to make functions with a query cleaner.
#' Title only; genus species.
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
create_query_string_T_wos <- function(genus, species, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = "', genus, ' ', species, '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = ("', genus, ' ', species, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TI = ("', genus, ' ', species, '" OR "', synonyms[1], '")')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('TI = ("', genus, ' ', species, '" OR "', synonyms[i], '")')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('TI = (("', genus, ' ', species, '" OR "', synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Web of Science to make functions with a query cleaner.
#' Title, abstract, and keywords; genus species.
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
create_query_string_TAK_wos <- function(genus, species, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = "', genus, ' ', species,
                  '" OR AB = "', genus, ' ', species,
                  '" OR AK = "', genus, ' ', species, '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = ("', genus, ' ', species, '" AND ', additionalkeywords, ')',
                  ' OR AB = ("', genus, ' ', species, '" AND ', additionalkeywords, ')',
                  ' OR AK = ("', genus, ' ', species, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TI = ("', genus, ' ', species, '" OR "', synonyms[1], '")',
                          ' OR AB = ("', genus, ' ', species, '" OR "', synonyms[1], '")',
                          ' OR AK = ("', genus, ' ', species, '" OR "', synonyms[1], '")')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('TI = ("', genus, ' ', species, '" OR "', synonyms[i], '")',
                              ' OR AB = ("', genus, ' ', species, '" OR "', synonyms[i], '")',
                              ' OR AK = ("', genus, ' ', species, '" OR "', synonyms[i], '")')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('TI = (("', genus, ' ', species, '" OR "', synonyms, '") AND ', additionalkeywords, ')',
                  ' OR AB = (("', genus, ' ', species, '" OR "', synonyms, '") AND ', additionalkeywords, ')',
                  ' OR AK = (("', genus, ' ', species, '" OR "', synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Web of Science to make functions with a query cleaner.
#' Title only; genus.
#'
#' @title Query string for Web of Science
#' 
#' @param genus Genus classification from the binomial name.
#' @param synonyms Alternate genus names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_T_wos_genus <- function(genus, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = "', genus, '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = ("', genus, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TI = ("', genus, '" OR "', synonyms[1], '")')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('TI = ("', genus, '" OR "', synonyms[i], '")')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('TI = (("', genus, '" OR "', synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Web of Science to make functions with a query cleaner.
#' Title, abstract, and keywords; genus.
#'
#' @title Query string for Web of Science
#' 
#' @param genus Genus classification from the binomial name.
#' @param synonyms Alternate species genus names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_TAK_wos_genus <- function(genus, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = "', genus,
                  '" OR AB = "', genus,
                  '" OR AK = "', genus, '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('TI = ("', genus, '" AND ', additionalkeywords, ')',
                  ' OR AB = ("', genus, '" AND ', additionalkeywords, ')',
                  ' OR AK = ("', genus, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('TI = ("', genus, '" OR "', synonyms[1], '")',
                          ' OR AB = ("', genus, '" OR "', synonyms[1], '")',
                          ' OR AK = ("', genus, '" OR "', synonyms[1], '")')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('TI = ("', genus, '" OR "', synonyms[i], '")',
                              ' OR AB = ("', genus, '" OR "', synonyms[i], '")',
                              ' OR AK = ("', genus, '" OR "', synonyms[i], '")')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('TI = (("', genus, '" OR "', synonyms, '") AND ', additionalkeywords, ')',
                  ' OR AB = (("', genus, '" OR "', synonyms, '") AND ', additionalkeywords, ')',
                  ' OR AK = (("', genus, '" OR "', synonyms, '") AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for BASE to make functions with a query cleaner.
#' Title only; genus species.
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
create_query_string_T_base <- function(genus, species, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:"', genus, ' ', species, '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:("', genus, ' ', species, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('dctitle:("', genus, ' ', species, '" OR ', synonyms[1])
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
    return(paste0('dctitle:(("', genus, ' ', species, '" OR ', synonyms, ') AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for BASE to make functions with a query cleaner.
#' Title, abstract, and keywords; genus species.
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
create_query_string_TAK_base <- function(genus, species, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:"', genus, ' ', species, '"',
                  ' OR dcdescription:"', genus, ' ', species, '"',
                  ' OR dcsubject:"', genus, ' ', species, '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:("', genus, ' ', species, '" AND ', additionalkeywords, ')',
                  ' OR dcdescription:("', genus, ' ', species, '" AND ', additionalkeywords, ')',
                  ' OR dcsubject:("', genus, ' ', species, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('dctitle:("', genus, ' ', species, '" OR ', synonyms[1], ')',
                          ' OR dcdescription:("', genus, ' ', species, '" OR ', synonyms[1], ')',
                          ' OR dcsubject:("', genus, ' ', species, '" OR ', synonyms[1], ')')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('dctitle:("', genus, ' ', species, '" OR ', synonyms[i], ')',
                              ' OR dcdescription:("', genus, ' ', species, '" OR ', synonyms[i], ')',
                              ' OR dcsubject:("', genus, ' ', species, '" OR ', synonyms[i], ')')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('dctitle:(("', genus, ' ', species, '"', ' OR ', synonyms, ') AND ', additionalkeywords, ')',
                  ' OR dcdescription:(("', genus, ' ', species, '"', ' OR ', synonyms, ') AND ', additionalkeywords, ')',
                  ' OR dcsubject:(("', genus, ' ', species, '"', ' OR ', synonyms, ') AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for BASE to make functions with a query cleaner.
#' Title only; genus.
#'
#' @title Query string for BASE
#' 
#' @param genus Genus classification from the binomial name.
#' @param synonyms Alternate genus names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_T_base_genus <- function(genus, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:"', genus, '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:("', genus, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('dctitle:("', genus, '" OR ', synonyms[1])
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
    return(paste0('dctitle:(("', genus, '" OR ', synonyms, ') AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for BASE to make functions with a query cleaner.
#' Title, abstract, and keywords; genus.
#'
#' @title Query string for BASE
#' 
#' @param genus Genus classification from the binomial name.
#' @param synonyms Alternate genus names.
#' @param additionalkeywords Optional search terms.
#'
#' @noRd
#' 
create_query_string_TAK_base_genus <- function(genus, synonyms, additionalkeywords){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:"', genus, '"',
                  ' OR dcdescription:"', genus, '"',
                  ' OR dcsubject:"', genus, '"'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('dctitle:("', genus, '" AND ', additionalkeywords, ')',
                  ' OR dcdescription:("', genus, '" AND ', additionalkeywords, ')',
                  ' OR dcsubject:("', genus, '" AND ', additionalkeywords, ')'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('dctitle:("', genus, '" OR ', synonyms[1], ')',
                          ' OR dcdescription:("', genus, '" OR ', synonyms[1], ')',
                          ' OR dcsubject:("', genus, '" OR ', synonyms[1], ')')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('dctitle:("', genus, '" OR ', synonyms[i], ')',
                              ' OR dcdescription:("', genus, '" OR ', synonyms[i], ')',
                              ' OR dcsubject:("', genus, '" OR ', synonyms[i], ')')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('dctitle:(("', genus, '"', ' OR ', synonyms, ') AND ', additionalkeywords, ')',
                  ' OR dcdescription:(("', genus, '"', ' OR ', synonyms, ') AND ', additionalkeywords, ')',
                  ' OR dcsubject:(("', genus, '"', ' OR ', synonyms, ') AND ', additionalkeywords, ')'))
  } 
}



#' Creates a query string for Lens to make functions with a query cleaner.
#' Title only; genus species.
#'
#' @title Query string for Lens
#' 
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers.
#'
#' @noRd
#' 
create_query_string_T_lens <- function(genus, species, synonyms, additionalkeywords, size = 50000){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, ' ', species, '\\"",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, ' ', species, '\\" AND ', additionalkeywords, '",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  }
  if (missing(additionalkeywords)&!missing(synonyms)) {
    temp_string <- paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, ' ', species, '\\" OR \\"', synonyms[1], '\\"",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, ' ', species, '\\" OR \\"', synonyms[i], '\\"",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "(\\"', genus, ' ', species, '\\" OR \\"', synonyms, '\\") AND ', additionalkeywords, '",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  } 
}



#' Creates a query string for Lens to make functions with a query cleaner.
#' Title, abstract, and keywords; genus species.
#'
#' @title Query string for Lens
#' 
#' @param genus Genus classification from the binomial name.
#' @param species Species classification from the binomial name.
#' @param synonyms Alternate species names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers.
#'
#' @noRd
#' 
create_query_string_TAK_lens <- function(genus, species, synonyms, additionalkeywords, size = 50000){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, ' ', species, '\\"",
					  "fields": ["title", "abstract", "keyword"],
					  "default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, ' ', species, '\\" AND ', additionalkeywords, '",
					  "fields": ["title", "abstract", "keyword"],
					"default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, ' ', species, '\\" OR \\"', synonyms[1], '\\"",
					  "fields": ["title", "abstract", "keyword"],
					"default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, ' ', species, '\\" OR \\"', synonyms[i], '\\"",
					  "fields": ["title", "abstract", "keyword"],
					"default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "(\\"', genus, ' ', species, '\\" OR \\"', synonyms, '\\") AND ', additionalkeywords, '",
					  "fields": ["title", "abstract", "keyword"],
					"default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  } 
}



#' Creates a query string for Lens to make functions with a query cleaner.
#' Title only; genus.
#'
#' @title Query string for Lens
#' 
#' @param genus Genus classification from the binomial name.
#' @param synonyms Alternate genus names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers.
#'
#' @noRd
#' 
create_query_string_T_lens_genus <- function(genus, synonyms, additionalkeywords, size = 50000){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, '\\"",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, '\\" AND ', additionalkeywords, '",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  }
  if (missing(additionalkeywords)&!missing(synonyms)) {
    temp_string <- paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, '\\" OR \\"', synonyms[1], '\\"",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, '\\" OR \\"', synonyms[i], '\\"",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "(\\"', genus, '\\" OR \\"', synonyms, '\\") AND ', additionalkeywords, '",
					  "fields": ["title"]
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  } 
}



#' Creates a query string for Lens to make functions with a query cleaner.
#' Title, abstract, and keywords; genus.
#'
#' @title Query string for Lens
#' 
#' @param genus Genus classification from the binomial name.
#' @param synonyms Alternate genus names.
#' @param additionalkeywords Optional search terms.
#' @param size Maximum number of documents that can be downloaded depending on the users token. Default is set to 50,000 for subscribers, the alternative is 1,000 for non-subscribers.
#'
#' @noRd
#' 
create_query_string_TAK_lens_genus <- function(genus, synonyms, additionalkeywords, size = 50000){
  if (missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, '\\"",
					  "fields": ["title", "abstract", "keyword"],
					  "default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  } 
  if (!missing(additionalkeywords) & missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, '\\" AND ', additionalkeywords, '",
					  "fields": ["title", "abstract", "keyword"],
					"default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  }
  if (missing(additionalkeywords) & !missing(synonyms)) {
    temp_string <- paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, '\\" OR \\"', synonyms[1], '\\"",
					  "fields": ["title", "abstract", "keyword"],
					"default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }')
    if (length(synonyms)==1) {
      return(paste0(temp_string))
    }
    else {
      for (i in 2:length(synonyms)){
        temp_string <- paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "\\"', genus, '\\" OR \\"', synonyms[i], '\\"",
					  "fields": ["title", "abstract", "keyword"],
					"default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }')
      }
      return(paste0(temp_string))
    }
  }
  if (!missing(additionalkeywords) & !missing(synonyms)) {
    return(paste0('{
    "query": {
		  "bool": {
			  "must": [{
				  "query_string": {
					  "query": "(\\"', genus, '\\" OR \\"', synonyms, '\\") AND ', additionalkeywords, '",
					  "fields": ["title", "abstract", "keyword"],
					"default_operator": "or"
				  }
			  }]
		  }
	  },
	  "size": ', size, '
  }'))
  } 
}



#' Theme for \code{\link{plotAllindices}}
#'
#' @title Theme for plot
#' 
#' @importFrom ggplot2 element_blank element_grob element_line element_rect element_render element_text
#'
#' @noRd
#' 
spindex_plots_theme <- function() {
  ggplot2::theme(title = element_text(size = 12,
                                      face = "bold"),
                 axis.title.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.line.x = element_line(colour = "grey20"),
                 axis.title.y = element_blank(),
                 axis.text.y = element_text(size = 10),
                 legend.key = element_rect(fill = "white"),
                 plot.background = element_rect(fill = "white"),
                 panel.background = element_rect(fill = "white"),
                 panel.grid.major.y = element_line(colour = "grey90"),
                 panel.grid.minor.y = element_line(colour = "grey90",
                                                   linetype = "longdash"),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank())
}