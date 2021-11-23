a <- httr::GET("http://api.elsevier.com/content/search/scopus",
               query = list(apiKey = "442b9048417ef20cf680a0ae26ee4d86",
                            query = 'TITLE-ABS-KEY("Bettongia penicillata")',
                            httpAccept = "application/xml"))

with_mock_api({
  test_that("scopus_requests_happen", {
    expect_s3_class(a,
                    "response")
  })
})
