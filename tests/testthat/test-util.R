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


test_that("Converting separate dates and times", {

  # overall stategy: anything invalid gives error. Consider giving warnings for syntactically correct but invalid inputs, e.g. time "2505" but error for "55555"

  not_dates_or_times <- flatten_list(extreme_numbers, random_test_letters(), random_test_numbers(hole=c(0,2400))) # NA should just give an NA

  #baseposix <- as.POSIXlt("2010-06-30") # date without time
  #basedate <- as.Date("2010-06-30")

  valid_dates <- list(
    "1899-12-31",
    "1900-01-01",
    "1900-1-1",
    "2012-6-06",
    "2013-06-6",
    "2015-10-10")

  invalid_short_dates <- list(
    "14-12-31",
    "200-12-31",
    "20141231",
    "2014-31-12",
    "1849-12-01")

  invalid_long_dates <- list(
    "2005-12-31 23:59",
    #    "2005-12-31 24:00", # 24:00 is technically valid POSIX. I don't want to test all of R date functions here, but I'm sure MV throws out some stupid numbers
    #    "2005-02-20 24:00", #
    "2005-02-31 12:00", # 31st feb
    "2005-01-01 24:01",
    "2005-12-31 00:01",
    "2005-12-31 00:00:01")
  #posixfulldates <- as.POSIXlt(fulldates, format="%Y-%m-%d %H:%M")
  #posixvaliddateonly <- as.POSIXlt(validdateonly, format="%Y-%m-%d")

  # unlist with recursive FALSE means a list of mixed type is returned, but flattened to depth of one.
  invalid_dates <- flatten_list(invalid_short_dates, invalid_long_dates)

  #TODO: include nonsense numbers and strings in the bad dates and times, randomly generated
  #TODO: include empty strings and NULLs

  numbertimes <- numbers_to_long_and_float(2359, 959, 10, 1, 0, na.rm=TRUE)
  stringtimes <- list("2359","959","0","1") # no leading zeros
  paddedtimes <- list("0959","100","0000","0001")
  numbertimesbad <- flatten_list(
    -0.1, 0.7, 100.6,
    numbers_to_long_and_float(2400, 2401, 999, -1, 2500, -1.1, 12345, 100000000000000) # technically 2400 is ISO POSIX valid, but I want an error in this case
  )
  stringtimesbad <- c("2401", "999", "0999", "9999", "-1", "100.5", "2500", "-0.1", "1-1")
  valid_times <- flatten_list(numbertimes, stringtimes, paddedtimes)
  invalid_times <- flatten_list(numbertimesbad, stringtimesbad, not_dates_or_times, valid_dates, invalid_long_dates, invalid_short_dates)
  #"invalid times: %s", paste(invalid_times, collapse=" "

  # nested tests, now the harness has been set up...
  test_that("completely stupid inputs, e.g. giving (valid) dates as time field", {

    for (jd in valid_dates) { # use valid input, but in the wrong place
      for (jt in valid_times) {
        # if Date class is put in time field, then this is a programming error, not a data error, so stop.
        expect_error(add_time_to_date(tms=as.Date(jd), dts=jd), label="incorrectly put date in time column", info=paste(jd, sep=" ", collapse=", "))
        expect_error(add_time_to_date(dts=c(jd,jd), tms=c(jt,jt,jt)), info=paste(jd, jt, sep=" ", collapse=", ")) # vector lengths differ
        expect_error(add_time_to_date(dts=c(jd,jd), tms=jt), info=paste(jd, jt, sep=" ", collapse=", ")) # vector lengths differ
      }
    }
  })

  expect_true(is.na(add_time_to_date(NA, NA)))
  #expect_true(is.na(add_time_to_date(as.POSIXlt(NA),NA)))
  #expect_true(is.na(add_time_to_date(NA,as.POSIXlt(NA))))
  #expect_true(is.na(add_time_to_date(as.POSIXlt(NA),as.POSIXlt(NA))))

  expect_error(add_time_to_date(dts = 7.7, tms = "2020")) # numeric class should error for Date

  test_that("bad dates, give warnings regardless of time input", {
    for (jd in invalid_dates) {
      for (jt in flatten_list(valid_times, invalid_times)) {
        #paste("classes: ", class(jd), class(jt), "values: ", jd, jt, collapse=" ", sep=" ")
        expect_warning(add_time_to_date(dts=jd, tms=jt), info=paste("classes: ", class(jd), class(jt), "values: ", jd, jt, collapse=" ", sep=" "))
      }
    }
  })

  test_that("bad times give warnings, regardless of date input", {
    for (jd in flatten_list(valid_dates, invalid_dates)) {
      for (jt in invalid_times) {
        expect_warning(add_time_to_date(dts=jd, tms=jt), info=paste("classes: ", class(jd), class(jt),  "values: ", jd, jt, collapse=" ", sep=" "))
      }
    }
  })

  #can't give datetime for as date: we're expecting just a date
  expect_error(add_time_to_date(fulldates, rep(x="1230", times=length(fulldates))))

  test_that("good inputs don't give errors, including NA", {
    for (jd in c(valid_dates, NA)) {
      for (jt in c(valid_times, NA)) {
        add_time_to_date(
          dts = jd,
          tms = jt
        )
        expect_that(
          add_time_to_date(
            dts = jd,
            tms = jt
            ),
          #not(throws_error()), # not(throws_error()) appears to be a bug in testthat right now.
          is_a("POSIXlt"),
          info = paste("classes: ", class(jd), class(jt), jd, jt, collapse = ", ", sep=", "))
      }
    }
  })
  # TODO: ?error if one value is NA and the other is invalid?

})
#
