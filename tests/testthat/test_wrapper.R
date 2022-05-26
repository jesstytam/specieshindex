#count()
with_mock_api({
  test_that("count wrapper requests", {
    expect_GET(Count(db = "scopus",
                     search = "t",
                     genus = "Bettongia"))
    expect_GET(Count(db = "scopus",
                     search = "tak",
                     genus = "Bettongia"))
    expect_POST(Count(db = "wos",
                     search = "t",
                     genus = "Bettongia"))
    expect_POST(Count(db = "wos",
                     search = "tak",
                     genus = "Bettongia"))
    expect_GET(Count(db = "base",
                     search = "t",
                     genus = "Bettongia"),
               "https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi")
    expect_GET(Count(db = "base",
                     search = "tak",
                     genus = "Bettongia"),
               "https://api.base-search.net/cgi-bin/BaseHttpSearchInterface.fcgi")
  })
})

# with_mock_api({
#   test_that("count request return", {
#     expect_type(Count(db = "scopus",
#                       search = "t",
#                       genus = "Bettongia"),
#                 "numeric")
#   })
# })

test_that("count wrapper errors", {  
  expect_error(Count(search = "t",
                     genus = "Bettongia"))
  expect_error(Count(db = "scopus",
                     genus = "Bettongia"))
  expect_error(Count(db = "scopus",
                     search = "t"))
})

#fetch()
with_mock_api({
  test_that("fetch wrapper requests", {
    expect_GET(Fetch(db = "scopus",
                     search = "t",
                     genus = "Bettongia"))
    expect_GET(Fetch(db = "scopus",
                     search = "tak",
                     genus = "Bettongia"))
    expect_POST(Fetch(db = "wos",
                      search = "t",
                      genus = "Bettongia"))
    expect_POST(Fetch(db = "wos",
                      search = "tak",
                      genus = "Bettongia"))
  })
})

test_that("fetch wrapper errors", {  
  expect_error(Fetch(search = "t",
                     genus = "Bettongia"))
  expect_error(Fetch(db = "scopus",
                     genus = "Bettongia"))
  expect_error(Fetch(db = "scopus",
                     search = "t"))
  expect_error(Fetch(db = "base",
                     search = "t",
                     genus = "Bettongia"))
})

#sp_check and query string construction
test_that("sp_check() errors", {
  expect_error(specieshindex:::sp_check("Betongia"),
               "not found on CoL, ITIS, NCBI, or EoL. Please check your spelling and try again.")
  expect_type(create_query_string_TAK_base("genus","species",c("syn1","syn2")),"character")
  expect_type(create_query_string_T_base("genus","species",c("syn1","syn2")),"character")
  expect_type(create_query_string_TAK_wos("genus","species",c("syn1","syn2")),"character")
  expect_type(create_query_string_T_wos("genus","species",c("syn1","syn2")),"character")
  expect_type(create_query_string_TAK_scopus("genus","species",c("syn1","syn2")),"character")
  expect_type(create_query_string_T_scopus("genus","species",c("syn1","syn2")),"character")
})
