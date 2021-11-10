use_oauth_token("foo")

with_mock_dir("scopus-get", {
  a <- httr::GET("http://api.elsevier.com/content/search/scopus",
                 query = list(apiKey = "442b9048417ef20cf680a0ae26ee4d86",
                              query = 'TITLE-ABS-KEY("Bettongia penincillata")',
                              httpAccept = "application/xml"))
  print(a)
})

with_mock_api({
  test_that("scopus_requests_happen", {
    expect_s3_class(httr::GET("http://api.elsevier.com/content/search/scopus",
                              query = list(apiKey = apikey,
                                           query = create_query_string_T_scopus(genus, species, synonyms, additionalkeywords),
                                           httpAccept = "application/xml")),
                    "response")
  })
})
