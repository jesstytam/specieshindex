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
  expect_type(create_query_string_T_wos("genus","species",c("syn1","syn2")),"character")
  expect_type(create_query_string_TAK_wos("genus","species",c("syn1","syn2")),"character")
})

#Count()
with_mock_api({
  test_that("wos count title requests", {
    expect_POST(specieshindex:::Count_wos(search = "t",
                                          genus = "Bettongia"),
               "http://search.webofknowledge.com/esti/wokmws/ws/WokSearch")
    expect_POST(specieshindex:::Count_wos(search = "tak",
                                          genus = "Bettongia"),
                "http://search.webofknowledge.com/esti/wokmws/ws/WokSearch")
  })
})

with_mock_api({
  test_that("wos count errors", {
    expect_error(specieshindex:::Count_wos(genus = "Bettongia"))
    expect_error(specieshindex:::Count_wos(search = " ",
                                           genus = "Bettongia"),
                 'Set search = "t" for title-only searches, or "tak" for searches in the title, abstract, or keywords.')
  })
})

#Fetch()
with_mock_api({
  test_that("wos scopus works", {
    expect_POST(specieshindex:::FetchT_wos(genus = "Bettongia"),
               'http://search.webofknowledge.com/esti/wokmws/ws/WokSearch <soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"')
    expect_POST(specieshindex:::FetchTAK_wos(genus = "Bettongia"),
                'http://search.webofknowledge.com/esti/wokmws/ws/WokSearch <soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"')
  })
})

with_mock_api({ 
  test_that("wos query working", {
    expect_POST(wosr::pull_wos(create_query_string_T_wos(genus = "Bettongia"),
                               sid = sid),
               'http://search.webofknowledge.com/esti/wokmws/ws/WokSearch <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"')
    expect_POST(wosr::pull_wos(create_query_string_TAK_wos(genus = "Bettongia"),
                               sid = sid),
               'http://search.webofknowledge.com/esti/wokmws/ws/WokSearch <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"')
  })
})

# with_mock_api({
#   test_that("code within fetch wos works", {
#     expect_output(specieshindex:::Count_wos(search = "t",
#                                             genus = "Bettongia"),
#                   "records found")
#     # search.webofknowledge.com/esti/wokmws/ws/WokSearch-dd82ab-POST.json
#     expect_output(specieshindex:::Count_wos(search = "tak",
#                                             genus = "Bettongia"),
#                   "records found")
#   })
# })


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
