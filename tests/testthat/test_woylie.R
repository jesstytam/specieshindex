test_that("Woylie_stats", {
  data(Woylie)
  expect_true(is.data.frame(Woylie))
  expect_true(TotalPub(Woylie)>=113)
  expect_true(TotalCite(Woylie)>=1897)
  expect_true(TotalJournals(Woylie)>=55)
  expect_true(YearsPublishing(Woylie)>=43)
  expect_true(SpHindex(Woylie)>=26)
  expect_true(SpMindex(Woylie)>=0.5)
  expect_true(Spi10(Woylie)>=54)
  expect_true(SpH5(Woylie)>=5)
  expect_true(SpHAfterdate(Woylie, "2000-01-01")>=20)
  expect_type(specieshindex:::sp_check("Bettongia", "penicillata"),
              "character")
})

test_that("Allindices", {
  data(Woylie)
  genus <- "Bettongia"
  species <- "penicillata"
  B_penicillata <- Allindices(Woylie,
                              "Bettongia",
                              "penicillata")
  combine <- data.frame(paste0(genus, " ", species),
                        paste0(species),
                        paste0(genus),
                        TotalPub(Woylie),
                        TotalCite(Woylie),
                        TotalJournals(Woylie),
                        YearsPublishing(Woylie),
                        SpHindex(Woylie),
                        SpMindex(Woylie),
                        Spi10(Woylie),
                        SpH5(Woylie))
  expect_type(combine,
              "list")
  combine_st <- cbind(combine, SourceType(Woylie))
  expect_type(combine_st,
              "list")
  expect_true(is.data.frame(B_penicillata))
  # expect_output(Allindices(Woylie,
  #                          "Bettongia",
  #                          "penicillata"),
  #               "genue_species") #not working
  # expect_output(str(B_penicillata), #doesn't test anything
  #               "1 obs")
  # expect_output(str(B_penicillata), #doesn't test anything
  #               "11 variables")
})
