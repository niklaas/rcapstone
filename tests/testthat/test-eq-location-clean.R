context("test-eq-location-clean.R")

test_that("single word before and after colon", {
  expect_equal(eq_location_clean("beforecolon:  aftercolon"), "Aftercolon")
  expect_equal(eq_location_clean("before, colon:  aftercolon"), "Aftercolon")
  expect_equal(eq_location_clean("before, colon:  after colon"), "After Colon")
  expect_equal(eq_location_clean("before, colon:  after, colon"), "After, Colon")
  expect_equal(eq_location_clean("before, colon:  afTer, coLon"), "After, Colon")
})
