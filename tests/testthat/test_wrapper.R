#count()
test_that("count wrapper errors", {  
  expect_error(Count(search = "t",
                     genus = "Bettongia"))
  expect_error(Count(db = "scopus",
                     genus = "Bettongia"))
  expect_error(Count(db = "scopus",
                     search = "t"))
})

#fetch()
test_that("fetch wrapper errors", {  
  expect_error(Fetch(search = "t",
                     genus = "Bettongia"))
  expect_error(Fetch(db = "scopus",
                     genus = "Bettongia"))
  expect_error(Fetch(db = "scopus",
                     search = "t"))
})

#sp_check
test_that("sp_check() errors", {
  expect_error(specieshindex:::sp_check("Betongia"),
               "not found on CoL, ITIS, NCBI, or EoL. Please check your spelling and try again.")
})
