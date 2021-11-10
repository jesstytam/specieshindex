context("testing_authentication")

test_that("wos_sid_presence", {
  sid <<- wosr::auth()
  expect_true(is.character(sid))
})