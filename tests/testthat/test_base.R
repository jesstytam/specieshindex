#string construction
test_that("string construction", {  
  expect_type(specieshindex:::create_query_string_T_base("Bettongia"),
              "character")
  expect_type(specieshindex:::create_query_string_T_base("Bettongia",
                                                         synonyms = "Woylie"),
              "character")
  expect_type(specieshindex:::create_query_string_T_base("Bettongia",
                                                         additionalkeywords = "cons"),
              "character")
  expect_type(specieshindex:::create_query_string_T_base("Bettongia",
                                                         synonyms = "Woylie",
                                                         additionalkeywords = "cons"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_base("Bettongia"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_base("Bettongia",
                                                           synonyms = "Woylie"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_base("Bettongia",
                                                           additionalkeywords = "cons"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_base("Bettongia",
                                                           additionalkeywords = "cons",
                                                           synonyms = "Woylie"),
              "character")
  expect_type(create_query_string_T_base("genus","species",c("syn1","syn2")),"character")
  expect_type(create_query_string_TAK_base("genus","species",c("syn1","syn2")),"character")
})

#Count()
with_mock_api({
  test_that("base count requests", {
    expect_GET(specieshindex:::Count_base(search = "t",
                                          genus = "Bettongia"),
               "https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi")
    expect_GET(specieshindex:::Count_base(search = "tak",
                                          genus = "Bettongia"),
               "https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi")
  })
})

with_mock_api({
  test_that("base count errors", {
    expect_error(specieshindex:::Count_base(genus = "Bettongia"))
    expect_error(specieshindex:::Count_base(search = " ",
                                            genus = "Bettongia"),
                 'Set search = "t" for title-only searches, or "tak" for searches in the title, abstract, or keywords.')
  })
})

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
   
