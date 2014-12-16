
context("data frame manipulation")


f1 <-  factor(x = c(1,2,3,1,2,3,3,2,1))
f2 <- factor(x = c(10,20,30,10,20,30,30,20,10))
f3 <- factor(x = c("a","b","c","d","c","b","a"))
fOneLevel <- factor(x = c("jack", "jack"))
fTwoLevels <- factor(x = c("jack", "alfie", "jack", "alfie"))
fOneExtraLevel <- factor(c("jack", "jack"),
                         levels = c("jack", "alfie", "liv"))
fTwoExtraLevel <- factor(c("jack", "alfie", "jack", "alfie"),
                         levels = c("jack", "alfie", "liv"))
f.short <- factor(x = c(1))
f.empty <- factor()
dframe <- data.frame(f1, f2)
fd <- data.frame(f2, f1)
mixed.df <- data.frame(fd, v1=as.numeric(f1))
v1 <- as.numeric(f1)
v2 <- as.numeric(f2)
vdf <- data.frame(v1, v2)
fdv1 <- data.frame(v2, v1)
fdv2 <- data.frame(v2=v1, v1=v2)

dfa <- dfb <- dfc <- data.frame(a = c(1,2,3,4),
                                b = c(11,12,13,14),
                                c = c(101,102,103,104))
dfb[1, "b"] <- 999
dfc[1, "b"] <- 999
dfc[2, "c"] <- 888

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
  #expect_equal(getNonFactorNames(data.frame(), consider=NULL), NULL)
  expect_warning(getNonFactorNames(data.frame()))
  #expect_output(getNonFactorNames(data.frame()), regexp='.*WARNING.*')
  expect_warning(getNonFactorNames(data.frame(), consider=NULL))
})

test_that("getFactorNames",{

  expect_identical(
    sort(getFactorNames(dframe)),
    sort(getFactorNames(fd))
  )
  expect_identical(getFactorNames(dframe), c("f1","f2"))
  expect_identical(getFactorNames(mixed.df), c("f2","f1"))
  expect_identical(getFactorNames(vdf), character(0))
  oldWarn <- options("warn")
  options(warn = - 1)
  expect_equal(getFactorNames(data.frame()), NULL)
  expect_equal(getFactorNames(data.frame(), consider=NULL), NULL)
  expect_equal(getFactorNames(data.frame(), NULL), NULL)
  options(oldWarn)
  # TODO: DO expect warnings - logging nightmare.
  expect_warning(getFactorNames(data.frame()))
  expect_warning(getFactorNames(data.frame(), consider=NULL))
  expect_warning(getFactorNames(data.frame(), NULL))
  #expect_output(getFactorNames(data.frame()), 'WARNING')
  #expect_output(getFactorNames(data.frame(), consider=NULL), 'WARNING')
  #expect_output(getFactorNames(data.frame(), NULL), 'WARNING')
  #check duplicate field names
  #TODO: should this error out?
  expect_equal(getFactorNames(cbind(mixed.df, dframe)), c("f2","f1","f1","f2"))
})

test_that("expandFactors", {

  expect_equal(
    expandFactors(dframe, consider=c("f1","f2")),
    expandFactors(dframe)
  )

  expect_warning(out <- expandFactors(dframe, consider = NULL))
  expect_identical(dframe, out)

  out <- expandFactors(dframe, consider=c("f1","f2"), sep = ".", verbose = TRUE)
  expect_equal(dim(out), c(9,6))
  expect_equal(class(out[[1]]), "logical")
  expect_equal(names(out), c("f1.1","f1.2","f1.3","f2.10","f2.20","f2.30"))

  out <- expandFactors(dframe, consider=c("f1"), sep = ".")
  expect_equal(dim(out), c(9,4))
  expect_equal(class(out[["f1.1"]]), "logical")
  expect_equal(names(out), c("f2","f1.1","f1.2","f1.3"))

  out <- expandFactors(mixed.df, consider = c("v1","f1"), sep = ".")
  expect_equal(dim(out), c(9,5))
  expect_equal(class(out[["f1.1"]]), "logical")
  expect_equal(names(out), c("f2","v1","f1.1","f1.2","f1.3"))
})

test_that("factorToDataframeLogical bad input fails", {

  expect_error(factorToDataframeLogical())
  expect_error(factorToDataframeLogical(dframe))
  expect_error(factorToDataframeLogical(bad_input))
  expect_error(factorToDataframeLogical(c("some","string of characters")))

  expect_error(factorToDataframeLogical(f1, prefix = c("1", "2")))
  expect_error(factorToDataframeLogical(f1, prefix = 1))

})

test_that("factorToDataframeLogical works", {
  expect_equal(dim(factorToDataframeLogical(f1, prefix = "f1", verbose = TRUE)), c(9, 3))
  expect_is(factorToDataframeLogical(f1, prefix = "f1"), "data.frame")
  expect_true(all(sapply(factorToDataframeLogical(f1, prefix = "f1"), is.logical)))

})

test_that("factorToDataframeLogical works for extra factor levels, one and two level factors", {
  expect_that(dim(factorToDataframeLogical(fTwoExtraLevel)), testthat::equals(c(4, 1)))
  expect_that(dim(factorToDataframeLogical(fOneExtraLevel)), testthat::equals(c(2, 1)))
  expect_that(dim(factorToDataframeLogical(fTwoLevels)), testthat::equals(c(4, 1)))
  expect_that(dim(factorToDataframeLogical(fOneLevel)), testthat::equals(c(2, 1)))

  # for two-level factors, we keep the first level, drop the second.

})

test_that("factorToDataframeLogical works for NA factor levels", {

  f <- factor(c("jack", "alfie", NA), exclude = NULL)  # make NA a level
  df <- data.frame(fjack = c(T, F, F), falfie = c(F,T,F), fNA = c(F,F,T))
  expect_equal(factorToDataframeLogical(f, prefix = "f"), df)

  f <- factor(c("jack", "alfie", NA), exclude = NA)  # make NA not a level
  df <- data.frame(fjack = c(T, F, F), falfie = c(F,T,F), fNA = c(F,F,T))
  expect_equal(factorToDataframeLogical(f, prefix = "f"), df)

  f <- factor(c("jack", NA))
  df <- data.frame(fjack =c(T,F))
  expect_equal(factorToDataframeLogical(f, prefix = "f"), df)

  f <- factor(c("jack", "alfie"), levels = c("jack", "alfie", NA))
  df <- data.frame(fjack = c(T,F))
  expect_equal(factorToDataframeLogical(f, prefix = "f"), df)

})




test_that("drop duplicate fields in a data frame", {
  expect_error(dropDuplicateFields(bad_input))
  expect_error(dropDuplicateFields(dfa, dfa))
  expect_error(dropDuplicateFields(extreme_numbers))
  expect_equal(dropDuplicateFields(dfa), dfa)
  expect_equal(dropDuplicateFields(dfa[c("a","b","c","c")]), dfa)
  expect_equal(dropDuplicateFields(dfa[c("a", "a", "b", "c")]), dfa)
  expect_equal(dropDuplicateFields(dfa[c("a", "a", "b", "c", "c")]), dfa)
})


test_that("drop rows with NA values in given fields bad data", {
  # don't test \code{complete.cases}, just my function
  expect_error(dropRowsWithNAField(c(4,5)))
  expect_error(dropRowsWithNAField(bad_input))
  expect_error(dropRowsWithNAField(random_test_dates()))
  expect_error(dropRowsWithNAField())
  expect_error(dropRowsWithNAField(cars, "speed", "dist"))
  expect_error(dropRowsWithNAField(cars, "doesnotexist"))
  expect_error(dropRowsWithNAField(cars, c("speed", "doesnotexist")))
})

test_that("drop rows with NA values in given fields good data", {
  expect_identical(dropRowsWithNAField(cars, "speed", verbose = TRUE), cars)
  expect_identical(dropRowsWithNAField(cars, c("speed", "dist")), cars)

  carsna1 <- carsna2 <- carsna3 <- cars
  carsna1[1,1] <- NA_integer_
  carsna2[1,2] <- NA_integer_
  carsna3[1,1:2] <- NA_integer_

  expect_identical(dropRowsWithNAField(cars), cars)
  expect_identical(dropRowsWithNAField(carsna1), cars[2:50, ])
  expect_identical(dropRowsWithNAField(carsna2), cars[2:50, ])
  expect_identical(dropRowsWithNAField(carsna3), cars[2:50, ])
  # without specifying particular fields, this should be equivalent to
  # \code{na.omit} (but not an 'omit' object)
  expect_equivalent(dropRowsWithNAField(carsna1), na.omit(carsna1))
  expect_equivalent(dropRowsWithNAField(carsna2), na.omit(carsna2))
  expect_equivalent(dropRowsWithNAField(carsna3), na.omit(carsna3))

  expect_identical(dropRowsWithNAField(cars, "speed"), cars)
  expect_identical(dropRowsWithNAField(carsna1, "speed"), cars[2:50, ])
  expect_identical(dropRowsWithNAField(carsna2, "speed"), carsna2)
  expect_identical(dropRowsWithNAField(carsna3, "speed"), cars[2:50, ])

  expect_identical(dropRowsWithNAField(cars, "dist"), cars)
  expect_identical(dropRowsWithNAField(carsna1, "dist"), carsna1)
  expect_identical(dropRowsWithNAField(carsna2, "dist"), cars[2:50, ])
  expect_identical(dropRowsWithNAField(carsna3, "dist"), cars[2:50, ])

})

test_that("get NA and non-NA  fields", {

  carsna1 <- carsna2 <- carsna3 <- cars
  carsna1[1,1] <- NA_integer_
  carsna2[1,2] <- NA_integer_
  carsna3[1,1:2] <- NA_integer_

  expect_error(getNAFields())
  expect_error(getNAFields(NA))
  expect_error(getNAFields(bad_input))

  expect_equal(getNAFields(cars), character(0))
  expect_equal(getNAFields(carsna1), "speed")
  expect_equal(getNAFields(carsna2), "dist")
  expect_equal(getNAFields(carsna3), c("speed", "dist"))

  expect_equal(getNonNAFields(cars), c("speed", "dist"))
  expect_equal(getNonNAFields(carsna1), "dist")
  expect_equal(getNonNAFields(carsna2), "speed")
  expect_equal(getNonNAFields(carsna3), character(0))

  expect_equal(propNaPerField(cars), c(speed = 0, dist = 0))
  expect_equal(propNaPerField(carsna1), c(speed = 0.02, dist = 0))
  expect_equal(propNaPerField(carsna2), c(speed = 0, dist = 0.02))
  expect_equal(propNaPerField(carsna3), c(speed = 0.02, dist = 0.02))

})

test_that("set diff on data frame indices before merging", {
  expect_error(getDropped())
  expect_error(getDropped(1))
  expect_error(getDropped("two"))

  expect_equal(getDropped(c("1","2"), c("2","3")),
               list(missing_from_x = "3",
                    missing_from_y = "1"))
  expect_equal(getDropped(c(1L, 2L), c(2L, 3L)),
               list(missing_from_x = 3L,
                    missing_from_y = 1L))
  expect_equal(getDropped(c(1.1,2.2), c(2.2,3.3)),
               list(missing_from_x = 3.3,
                    missing_from_y = 1.1))

  # both sides the same
  expect_equal(getDropped(c("1","2","3"), c("1","2","3")),
               list(missing_from_x = character(0),
                    missing_from_y = character(0)))
  expect_equal(getDropped(c(1,2,3), c(1,2,3)),
               list(missing_from_x = numeric(0),
                    missing_from_y = numeric(0)))
  expect_equal(getDropped(c(1L, 2L, 3L), c(1L, 2L, 3L)),
               list(missing_from_x = integer(0),
                    missing_from_y = integer(0)))

})

test_that("affix fields bad inputs", {
  expect_error(affixFields(c("a", "b")))
  expect_error(affixFields(c("a", "b"), affix = ""))
  expect_error(affixFields(c("a", "b"), affix = "x", skip = bad_input))
  expect_error(affixFields(c("a", "b"), affix = "x", renameHow = "no"))
})

test_that("affix good inputs", {
  expect_equal(
    affixFields("a", affix = "x",
                renameHow = "prefix"),
    "x.a"
  )
  expect_equal(
    affixFields(c("a", "b"), affix = "x",
                renameHow = "prefix"),
    c("x.a", "x.b")
  )
  expect_equal(
    affixFields(c("a", "b"), affix = "x",
                renameHow = "suffix"),
    c("a.x", "b.x")
  )
  expect_equal(
    affixFields(c("a", "b"), affix = "x",
                renameHow = "prefix", skip = "a"),
    c("a", "x.b")
  )
  expect_equal(
    affixFields(c("a", "b"), affix = "x",
                renameHow = "prefix", skip = "b"),
    c("x.a", "b")
  )
  expect_equal(
    affixFields(c("a", "b"), affix = "x",
                renameHow = "prefix", sep = "op"),
    c("xopa", "xopb")
  )
})
