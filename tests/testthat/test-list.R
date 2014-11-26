context("list functions")

test_that("trim a list", {
  expect_error(listTrim())
  expect_error(listTrim(random_test_integers()))

  cl <- as.list(cars)
  expect_identical(listTrim(cl), cl)
  expect_identical(listTrim(c(cl, NA)), cl)
  expect_identical(listTrim(c(NA, cl)), cl)
  expect_identical(listTrim(c(NA, cl, NA)), cl)
  expect_identical(listTrim(c(list(NULL), NA, cl)), cl)


})

test_that("is a list flat", {
  expect_error(isFlat())

  expect_true(isFlat(list(1)))
  expect_true(isFlat(list(1,2,3)))
  expect_that(length(isFlat(bad_input)), testthat::equals(1))
})
