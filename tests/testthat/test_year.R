
test_that("year is numeric", {
  year = 2013
  file <- make_filename(2013)
  expect_that(file, equals("accident_2013.csv.bz2"))
})
