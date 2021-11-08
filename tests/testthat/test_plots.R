context("testing_plots")

test_that("preparing_data_for_plots", {
  data(Woylie)
  Woylie_source <- SourceType(Woylie)
  expect_true(is.data.frame(Woylie_source))
  Woylie_year <- getYear(Woylie, "Bettongia", "penicillata")
  expect_true(is.data.frame(Woylie_year))
  B_penicillata <- Allindices(Woylie, "Bettongia", "penicillata")
  expect_true(is.data.frame(B_penicillata))
})

test_that("plotting", {
  expect_identical(class(index_plot$layers[[1]]$geom)[1], "GeomDrawGrob")
  expect_identical(class(index_plot$layers[[1]]$stat)[1], "StatIdentity")
  expect_identical(class(pub_plot$layers[[1]]$geom)[1], "GeomLine")
  expect_identical(class(pub_plot$layers[[1]]$stat)[1], "StatIdentity")
})