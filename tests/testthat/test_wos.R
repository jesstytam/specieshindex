#TITLE ONLY
t_wos <- httr::GET("http://search.webofknowledge.com/esti/wokmws/ws/WokSearch-47b6e0-POST")
data(Woylie)

with_mock_api({ #pass
  test_that("t_count_wos_requests_happen", {
    expect_s3_class(t_wos, "response")
    expect_true(class(t_wos$status_code)=="integer")
  })
})

#TITLE + ABSTRACT + KEYWORDS
tak_wos <- httr::GET("http://search.webofknowledge.com/esti/wokmws/ws/WokSearch-7a52af-POST")

with_mock_api({ #pass
  test_that("tak_count_wos_requests_happen", {
    expect_s3_class(tak_wos, "response")
    expect_true(class(tak_wos$status_code)=="integer")
  })
})
