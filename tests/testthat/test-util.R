#' @title unit tests for 'CaseControlFunctions'
#' @import testthat logging

context("Utility Functions")

test_that("countNotNumeric", {
  expect_equal(countNotNumeric(c(NA)), 1)
  expect_equal(countNotNumeric(c("badger's mount")), 1)
  expect_equal(countNotNumeric(c("1 ", NA)), 1)
  expect_equal(countNotNumeric(c(" 1", NA)), 1)
  expect_equal(countNotNumeric(c(" 1 ", NA)), 1)
  expect_equal(countNotNumeric(c("1","two", NA)), 2)
  expect_equal(countNotNumeric(c("1","two", c(NA,1))), 2)
  expect_equal(countNotNumeric(c("1","two", c("2",NA,1))), 2)
  expect_equal(countNotNumeric(c()),0) # no non-numeric values in an empty vector
})

test_that("propIsNa", {
  expect_equal(propIsNa(c()), 0) # don't divide by zero
  expect_equal(propIsNa(c(1)), 0)
  expect_equal(propIsNa(c(NA)), 1)
  expect_equal(propIsNa(c(NA, NA)), 1)
  expect_equal(propIsNa(c(1, NA)), 0.5)
  expect_equal(propIsNa(c(NA, 1)), 0.5)
  expect_equal(propIsNa(c(NA, "two", 1, NA)), 0.5)
})

test_that("logicalToBinary", {

  dbinary <- data.frame(jack=c("21232",421412,123123), hayley=c(1,0,0), ham=c(0,1,0))

  expect_error(logicalToBinary())
  expect_error(logicalToBinary(FALSE))
  expect_error(logicalToBinary(list(dbinary, "rubbish")))
  expect_error(logicalToBinary(data.frame()))

  result <- logicalToBinary(dbinary)
  expect_identical(result, dbinary) # no logicals!

  dlogical <- data.frame(jack=c("21232",421412,123123), hayley=c(T,F,F), ham=c(F,T,F))
  rlogical <- logicalToBinary(dlogical)
  expect_equivalent(rlogical, dbinary)

})
