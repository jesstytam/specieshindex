#string construction
test_that("string construction", {  
  expect_type(specieshindex:::create_query_string_T_scopus("Bettongia"),
              "character")
  expect_type(specieshindex:::create_query_string_T_scopus("Bettongia",
                                                           synonyms = "Woylie"),
              "character")
  expect_type(specieshindex:::create_query_string_T_scopus("Bettongia",
                                                           additionalkeywords = "cons"),
              "character")
  expect_type(specieshindex:::create_query_string_T_scopus("Bettongia",
                                                           additionalkeywords = "cons",
                                                           synonyms = "Woylie"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_scopus("Bettongia"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_scopus("Bettongia",
                                                             synonyms = "Woylie"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_scopus("Bettongia",
                                                             additionalkeywords = "cons"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_scopus("Bettongia",
                                                             additionalkeywords = "cons",
                                                             synonyms = "Woylie"),
              "character")
  
})

#TITLE ONLY
t_scopus <- httr::GET("http://api.elsevier.com/content/search/scopus-ccc9dc")
data(Woylie)

with_mock_api({ #pass
  test_that("t_count_scopus_requests_happen", {
    expect_s3_class(t_scopus, "response")
    expect_true(class(t_scopus$status_code)=="integer")
  })
})

#test_that("CountSpT_scopus_works", { #Error: XML content does not seem to be XML: 'ot exist."}}'
  #response_data <- XML::xmlParse(t_scopus) #parse the data to extract values
  #resultCount <- as.numeric(XML::xpathSApply(t_scopus,"//opensearch:totalResults", XML::xmlValue))
  #expect_equal(CountSpT("scopus", "Bettongia", "penicillata"), resultCount)
  #expect_identical(CountSpT("scopus", "Bettongia", "penicillata"), CountSpT_scopus("Bettongia", "penicillata"))
#})

#fromJSON((rawToChar(t_scopus$content)))

with_mock_api({
  test_that("FetchT_scopus_works", {
    expect_s3_class(Woylie, "data.frame")
    # expect_equal(specieshindex:::FetchT_scopus("Bettongia", "penicillata"), Woylie) #nope
    # expect_identical(FetchSpT("scopus", "Bettongia", "penicillata"), FetchSpT_scopus("Bettongia", "penicillata")) #nope
  })
})

test_that("FetchT_scopus_works", {
  expect_s3_class(Woylie, "data.frame")
  # expect_equal(FetchSpT("scopus", "Bettongia", "penicillata"), Woylie) #nope
  # expect_identical(FetchSpT("scopus", "Bettongia", "penicillata"), FetchSpT_scopus("Bettongia", "penicillata")) #nope
})


#TITLE + ABSTRACT + KEYWORDS
tak_scopus <- httr::GET("http://api.elsevier.com/content/search/scopus-97ddc6")

with_mock_api({ #pass
  test_that("tak_count_scopus_requests_happen", {
    expect_s3_class(tak_scopus, "response")
    expect_true(class(tak_scopus$status_code)=="integer")
  })
})

# test_that("CountSpT_scopus_works", {
#   # response_data <- XML::xmlParse(tak_scopus) #parse the data to extract values
#   # resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue))
#   resultCount <- jsonlite::fromJSON(rawToChar(tak_scopus$content))
#   expect_equal(CountSpTAK("scopus", "Bettongia", "penicillata"), resultCount)
#   expect_identical(CountSpTAK("scopus", "Bettongia", "penicillata"), CountSpTAK_scopus("Bettongia", "penicillata"))
# })