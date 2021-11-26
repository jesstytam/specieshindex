#TITLE ONLY
with_mock_api({
  test_that("t_scopus_requests_happen", {
    expect_s3_class(t_scopus, "response")
    expect_true(class(t_scopus$status_code)=="integer")
  })
})

test_that("CountSpT_scopus_works", {
  response_data <- XML::xmlParse(t_scopus) #parse the data to extract values
  resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue))
  expect_equal(CountSpT("scopus", "Bettongia", "penicillata"), resultCount)
  expect_identical(CountSpT("scopus", "Bettongia", "penicillata"), CountSpT_scopus("Bettongia", "penicillata"))
})

with_mock_api({
  test_that("FetchSpT_scopus_works", {
    expect_s3_class(Woylie, "data.frame")
    # expect_equal(FetchSpT("scopus", "Bettongia", "penicillata"), Woylie) #nope
    # expect_identical(FetchSpT("scopus", "Bettongia", "penicillata"), FetchSpT_scopus("Bettongia", "penicillata")) #nope
  })
})

test_that("FetchSpT_scopus_works", {
  expect_s3_class(Woylie, "data.frame")
  # expect_equal(FetchSpT("scopus", "Bettongia", "penicillata"), Woylie) #nope
  # expect_identical(FetchSpT("scopus", "Bettongia", "penicillata"), FetchSpT_scopus("Bettongia", "penicillata")) #nope
})


#TITLE + ABSTRACT +KEYWORDS
with_mock_api({
  test_that("tak_scopus_requests_happen", {
    expect_s3_class(tak_scopus,
                    "response")
    expect_true(class(tak_scopus$status_code)=="integer")
  })
})

test_that("CountSpT_scopus_works", {
  response_data <- XML::xmlParse(tak_scopus) #parse the data to extract values
  resultCount <- as.numeric(XML::xpathSApply(response_data,"//opensearch:totalResults", XML::xmlValue))
  expect_equal(CountSpTAK("scopus", "Bettongia", "penicillata"), resultCount)
  expect_identical(CountSpTAK("scopus", "Bettongia", "penicillata"), CountSpTAK_scopus("Bettongia", "penicillata"))
})
