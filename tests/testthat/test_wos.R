#string construction
test_that("string construction", {  
  expect_type(specieshindex:::create_query_string_T_wos("Bettongia"),
              "character")
  expect_type(specieshindex:::create_query_string_T_wos("Bettongia",
                                                        synonyms = "Woylie"),
              "character")
  expect_type(specieshindex:::create_query_string_T_wos("Bettongia",
                                                        additionalkeywords = "cons"),
              "character")
  expect_type(specieshindex:::create_query_string_T_wos("Bettongia",
                                                        additionalkeywords = "cons",
                                                        synonyms = "Woylie"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_wos("Bettongia"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_wos("Bettongia",
                                                           synonyms = "Woylie"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_wos("Bettongia",
                                                           additionalkeywords = "cons"),
              "character")
  expect_type(specieshindex:::create_query_string_TAK_wos("Bettongia",
                                                          additionalkeywords = "cons",
                                                          synonyms = "Woylie"),
              "character")
})

#TITLE ONLY
# sid <- wosr::auth(username = NULL, password = NULL)
# t_wos <- httr::GET("http://search.webofknowledge.com/esti/wokmws/ws/WokSearch-47b6e0-POST")
# data(Woylie)
# 
# with_mock_api({ #pass
#   test_that("wos_count_requests_happen", {
#     expect_s3_class(t_wos <- wosr::query_wos(query = specieshindex:::create_query_string_T_wos("Bettongia", "penicillata"),
#                                              sid = sid),
#                     "query_result")
#   })
# })
# 
# test_that("wos_count_requests_happen", { #pass; without loading t_wos
#   expect_s3_class(wosr::query_wos(query = specieshindex:::create_query_string_T_wos("Bettongia", "penicillata"),
#                                            sid = sid),
#                   "query_result")
# })
# 
# test_that("CountSpT_wos_works", { #nope
#   expect_snapshot_value(Count(db = "wos",
#                      search = "t",
#                      level = "species",
#                      genus = "Bettongia",
#                      species = "penicillata"),
#                    wosr::query_wos(query = specieshindex:::create_query_string_T_wos("Bettongia", "penicillata"),
#                                    sid = sid))
# })
# 
# with_mock_api({ #nope
#   test_that("CountSpT_wos_works", {
#     expect_snapshot_output(Count(db = "wos",
#                               search = "t",
#                               level = "species",
#                               genus = "Bettongia",
#                               species = "penicillata"),
#                         wosr::query_wos(query = specieshindex:::create_query_string_T_wos("Bettongia", "penicillata"),
#                                         sid = sid))
#     })
# })
# 
# 
# 
# #TITLE + ABSTRACT + KEYWORDS
# tak_wos <- httr::GET("http://search.webofknowledge.com/esti/wokmws/ws/WokSearch-7a52af-POST")
# 
# with_mock_api({ #pass
#   test_that("tak_count_wos_requests_happen", {
#     expect_s3_class(tak_wos, "response")
#     expect_true(class(tak_wos$status_code)=="integer")
#   })
# })