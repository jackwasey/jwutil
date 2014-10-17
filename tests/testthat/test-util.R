#' @title unit tests for 'CaseControlFunctions'
#' @import testthat logging

context("Utility Functions")

f1 = factor(x=c(1,2,3,1,2,3,3,2,1))
f2 = factor(x=c(10,20,30,10,20,30,30,20,10))
f3 = factor(x=c("a","b","c","d","c","b","a"))
f.short = factor(x=c(1))
f.empty = factor()
dframe <- data.frame(f1, f2)
fd <- data.frame(f2, f1)
mixed.df <- data.frame(fd, v1=as.numeric(f1))
v1 = as.numeric(f1)
v2 = as.numeric(f2)
vdf <- data.frame(v1, v2)
fdv1 <- data.frame(v2, v1)
fdv2 <- data.frame(v2=v1, v1=v2)

# CaseControlFunctions:
test_that("getNonFactorNames", {
  
  expect_identical(
    sort(getNonFactorNames(vdf)),
    sort(getNonFactorNames(fdv1))
  )
  expect_identical(
    sort(getNonFactorNames(vdf)),
    sort(getNonFactorNames(fdv2))
  )
  expect_equal(getNonFactorNames(vdf), c("v1","v2"))
  expect_equal(getNonFactorNames(fdv1), c("v2","v1"))
  expect_equal(getNonFactorNames(mixed.df), c("v1"))
  expect_equal(getNonFactorNames(data.frame(f1,v1,f2)), c("v1"))
  expect_equal(getNonFactorNames(data.frame(v1,f1,v2)), c("v1","v2"))
  #expect_equal(getNonFactorNames(data.frame()), NULL)
  #expect_equal(getNonFactorNames(data.frame(), considerFactors=NULL), NULL)
  expect_warning(getNonFactorNames(data.frame()))
  #expect_output(getNonFactorNames(data.frame()), regexp='.*WARNING.*')
  expect_warning(getNonFactorNames(data.frame(), considerFactors=NULL))
  #expect_output(getNonFactorNames(data.frame(), considerFactors=NULL), regexp='.*WARNING.*')
})

test_that("getFactorNames",{
  
  expect_identical(
    sort(getFactorNames(dframe)),
    sort(getFactorNames(fd))
  )
  expect_identical(getFactorNames(dframe), c("f1","f2"))
  expect_identical(getFactorNames(mixed.df), c("f2","f1"))
  expect_identical(getFactorNames(vdf), NULL)
  oldWarn <- options("warn")
  options(warn=-1)
  expect_equal(getFactorNames(data.frame()), NULL)
  expect_equal(getFactorNames(data.frame(), considerFactors=NULL), NULL)
  expect_equal(getFactorNames(data.frame(), NULL), NULL)
  options(oldWarn)
  # TODO: DO expect warnings - logging nightmare.
  expect_warning(getFactorNames(data.frame()))
  expect_warning(getFactorNames(data.frame(), considerFactors=NULL))
  expect_warning(getFactorNames(data.frame(), NULL))
  #expect_output(getFactorNames(data.frame()), 'WARNING')
  #expect_output(getFactorNames(data.frame(), considerFactors=NULL), 'WARNING')
  #expect_output(getFactorNames(data.frame(), NULL), 'WARNING')
  #check duplicate field names
  #TODO: should this error out?
  expect_equal(getFactorNames(cbind(mixed.df, dframe)), c("f2","f1","f1","f2"))
})

test_that("exFactor", {
  
  expect_equal(
    exFactor(dframe, considerFactors=c("f1","f2")),
    exFactor(dframe)
  )
  
  expect_warning(out <- exFactor(dframe, considerFactors=NULL))
  #expect_output(out <- exFactor(dframe, considerFactors=NULL), 'WARNING')
  expect_identical(dframe, out$dat)
  expect_true(is.null(out$newFields))
  
  out <- exFactor(dframe, considerFactors=c("f1","f2"))
  expect_equal(dim(out$dat), c(9,6))
  expect_identical(out$newFields, c("f1.1","f1.2","f1.3","f2.10","f2.20","f2.30"))
  expect_equal(class(out$dat[[1]]), "logical")
  expect_equal(names(out$dat), c("f1.1","f1.2","f1.3","f2.10","f2.20","f2.30"))
  
  out <- exFactor(dframe, considerFactors=c("f1"))
  expect_equal(dim(out$dat), c(9,4))
  expect_identical(out$newFields, c("f1.1","f1.2","f1.3"))
  expect_equal(class(out$dat[["f1.1"]]), "logical")
  expect_equal(names(out$dat), c("f2","f1.1","f1.2","f1.3"))
  
  out <- exFactor(mixed.df, considerFactors=c("v1","f1"))
  expect_equal(dim(out$dat), c(9,5))
  expect_identical(out$newFields, c("f1.1","f1.2","f1.3"))
  expect_equal(class(out$dat[["f1.1"]]), "logical")
  expect_equal(names(out$dat), c("f2","v1","f1.1","f1.2","f1.3"))
})

test_that("factorToCols", {
  
  expect_error(factorToCols())
  expect_error(factorToCols(dframe))
  expect_error(factorToCols("some string of characters"))
  expect_equal(dim(factorToCols(f1)), c(9,3))
  expect_equal(class(factorToCols(f1)), "data.frame")
  expect_equal(as.vector(sapply(factorToCols(f1), class)), c("logical", "logical", "logical"))
  
})

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
  expect_equal(propIsNa(c(NA,NA)), 1)
  expect_equal(propIsNa(c(1,NA)), 0.5)
  expect_equal(propIsNa(c(NA,1)), 0.5)
  expect_equal(propIsNa(c(NA,"two",1,NA)), 0.5)
})

test_that("idealWeight", {
inch=100/2.54
expect_error(idealWeight(male=T))
expect_error(idealWeight(heightm=1.7))

  expect_warning(idealWeight(heightm=0, male=T)) # should warn when height is out of validated range of the formula
  expect_warning(idealWeight(heightm=-1, male=T)) # should warn when height is out of validated range of the formula
  expect_warning(idealWeight(heightm=3, male=T)) # should warn when height is out of validated range of the formula
  expect_warning(idealWeight(heightm=59/inch, male=T)) # should warn when height is out of validated range of the formula


expect_equal(idealWeight(60/inch, male=T), 50) 
expect_equal(idealWeight(60/inch, male=F), 45.5) 
expect_equal(idealWeight(c(60/inch, 60/inch), male=c(F,T)), c(45.5, 50))
expect_equal(idealWeight(c(60/inch, 60/inch, NA), male=c(F,T,T)), c(45.5, 50, NA))
expect_equal(idealWeight(c(60/inch, 60/inch, 60/inch), male=c(F,NA,T)), c(45.5, NA, 50))
expect_error(idealWeight(c(60/inch, 60/inch, 60/inch), male=c(F,T)))
expect_error(idealWeight(c(60/inch, 60/inch), male=c(F,T,T)))
expect_error(idealWeight(c(), male=c(F,T,T)))
expect_error(idealWeight(c(60/inch, 60/inch), male=c()))

expect_warning(idealWeight(12*8.1/inch, male=T))

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