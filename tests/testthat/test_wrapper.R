#count()
test_that("count wrapper", {  
  expect_error(Count(search = "t",
                     genus = "Bettongia"))
  expect_error(Count(db = "scopus",
                     genus = "Bettongia"))
  expect_error(Count(db = "scopus",
                     search = "t"))
})

#fetch()
test_that("fetch wrapper", {  
  expect_error(Fetch(search = "t",
                     genus = "Bettongia"))
  expect_error(Fetch(db = "scopus",
                     genus = "Bettongia"))
  expect_error(Fetch(db = "scopus",
                     search = "t"))
})