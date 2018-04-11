context("test-eq-location-clean.R")

test_that("removing left part of colon sep with spaces", {
  expect_equal(eq_location_clean("beforecolon:  aftercolon"), "Aftercolon")
  expect_equal(eq_location_clean("before, colon:  aftercolon"), "Aftercolon")
  expect_equal(eq_location_clean("before, colon:  after colon"), "After Colon")
  expect_equal(eq_location_clean("before, colon:  after, colon"), "After, Colon")
  expect_equal(eq_location_clean("before, colon:  afTer, coLon"), "After, Colon")
})

test_that("apply title case", {
  expect_equal(eq_location_clean("before, colon:  afTer, coLon"), "After, Colon")
})

test_that("for multiple colons only take rightest part", {
  expect_equal(eq_location_clean("before,+soMe colon:  colon in the mid:  rightest PaRt"), "Rightest Part")
})

test_that("remove superfluous whitespace", {
  expect_equal(eq_location_clean("before,+soMe colon:  colon in the mid:   rightest PaRt   "), "Rightest Part")
})

test_that("deal with vectors", {
  expect_equal(eq_location_clean(c("some here: morE here+here: and there", "No COLON")), c("And There", "No Colon"))
})