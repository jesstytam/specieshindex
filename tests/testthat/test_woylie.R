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
  B_penicillata <- Allindices(Woylie,
                              "Bettongia",
                              "penicillata")
  expect_true(is.data.frame(B_penicillata))
  expect_output(str(B_penicillata),
                "1 obs")
})
