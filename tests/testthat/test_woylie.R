context("testing_indices")

test_that("Woylie_stats", {
  data(Woylie)
  expect_true(is.data.frame(Woylie))
  expect_true(TotalPub(Woylie)>=113)
  expect_true(TotalCite(Woylie)>=1897)
  expect_true(TotalJournals(Woylie)>=55)
  expect_true(TotalArt(Woylie)>=110)
  expect_true(TotalRev(Woylie)>=3)
  expect_true(is.character(ARRatio(Woylie))) #97.3451327433628:2.65486725663717
  expect_true(YearsPublishing(Woylie)>=43)
  expect_true(SpHindex(Woylie)>=26)
  expect_true(SpMindex(Woylie)>=0.6)
  expect_true(Spi10(Woylie)>=54)
  expect_true(SpH5(Woylie)>=7)
  expect_true(SpHAfterdate(Woylie, "2000-01-01")>=20)
  B_penicillata <- Allindices(Woylie,"Bettongia","penicillata")
  expect_true(is.data.frame(B_penicillata))
})
#
