tak_request <- lapply(readline("tests/testthat/api.elsevier.com/content/search/scopus-97ddc6.json"), fromJSON)

with_mock_api({
  test_that("scopus_requests_happen", {
    expect_s3_class(tak_request,
                    "response")
  })
})


#try this
#save(a,file="tests/testthat/api.elsevier.com/content/search/scopus-8efb8b.json")
#dir.create("api.elsevier.com/content/search",recursive = TRUE)
#a <- httr::GET("http://api.elsevier.com/content/search/scopus",
# query = list(apiKey = "442b9048417ef20cf680a0ae26ee4d86",
#              query = 'TITLE-ABS-KEY("Bettongia penincillata")',
#              httpAccept = "application/xml"))

#write a new request and see what new hash is given, use that as the file path
#use real api key to get request, use a fake one to call it again ("test)