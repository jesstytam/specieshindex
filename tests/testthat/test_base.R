#TITLE ONLY
# 
 with_mock_api({ #pass
  t_base <- httr::GET("http://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi-8985d6")
  data(Woylie)

  test_that("t_count_base_requests_happen", {
     expect_s3_class(t_base, "response")
     expect_true(class(t_base$status_code)=="integer")
   })
 })
# 
# #TITLE + ABSTRACT + KEYWORDS
# 
 with_mock_api({ #pass
 tak_base <- httr::GET("http://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi-1213bf")
 
   test_that("tak_count_base_requests_happen", {
     expect_s3_class(tak_base, "response")
     expect_true(class(tak_base$status_code)=="integer")
   })
 }) 
   
  
  test_that("base errors", {
     expect_error(Fetch("base",search = "t",genus="Bettongia"))
 })

  test_that("missing genus errors", {
    expect_error(Fetch("base",search = "t"))
  })
  
  