
context("data frame manipulation")


f1 <-  factor(x=c(1,2,3,1,2,3,3,2,1))
f2 <- factor(x=c(10,20,30,10,20,30,30,20,10))
f3 <- factor(x=c("a","b","c","d","c","b","a"))
fOneLevel <- factor(x = c("jack", "jack"))
fTwoLevels <- factor(x = c("jack", "alfie", "jack", "alfie"))
fOneExtraLevel <- factor(x = c("jack", "jack"), levels = c("jack", "alfie", "liv"))
fTwoExtraLevel <- factor(x = c("jack", "alfie", "jack", "alfie"), levels = c("jack", "alfie", "liv"))
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

dfa <- dfb <- dfc <- data.frame(a = c(1,2,3,4), b = c(11,12,13,14), c = c(101,102,103,104))
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
  expect_identical(getFactorNames(vdf), character(0))
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

test_that("expandFactors", {

  expect_equal(
    expandFactors(dframe, considerFactors=c("f1","f2")),
    expandFactors(dframe)
  )

  expect_warning(out <- expandFactors(dframe, considerFactors = NULL))
  expect_identical(dframe, out)

  out <- expandFactors(dframe, considerFactors=c("f1","f2"), sep = ".")
  expect_equal(dim(out), c(9,6))
  expect_equal(class(out[[1]]), "logical")
  expect_equal(names(out), c("f1.1","f1.2","f1.3","f2.10","f2.20","f2.30"))

  out <- expandFactors(dframe, considerFactors=c("f1"), sep = ".")
  expect_equal(dim(out), c(9,4))
  expect_equal(class(out[["f1.1"]]), "logical")
  expect_equal(names(out), c("f2","f1.1","f1.2","f1.3"))

  out <- expandFactors(mixed.df, considerFactors = c("v1","f1"), sep = ".")
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
  expect_equal(dim(factorToDataframeLogical(f1, prefix = "f1")), c(9, 3))
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

  f <- factor(c("jack", "alfie", NA), exclude = NULL) # make NA a level
  df <- data.frame(fjack = c(T, F, F), falfie = c(F,T,F), fNA = c(F,F,T))
  expect_equal(factorToDataframeLogical(f, prefix = "f"), df)

  f <- factor(c("jack", "alfie", NA), exclude = NA) # make NA not a level
  df <- data.frame(fjack = c(T, F, F), falfie = c(F,T,F), fNA = c(F,F,T))
  expect_equal(factorToDataframeLogical(f, prefix = "f"), df)

  f <- factor(c("jack", NA))
  df <- data.frame(fjack =c(T,F))
  expect_equal(factorToDataframeLogical(f, prefix = "f"), df)

  f <- factor(c("jack", "alfie"), levels = c("jack", "alfie", NA))
  df <- data.frame(fjack = c(T,F))
  expect_equal(factorToDataframeLogical(f, prefix = "f"), df)

})




context("test merging")

test_that("merge identical frames should give identical result" , {
  r <- mergeBetter(x = dfa, y = dfa, by.x = "a", by.y = "a")
  expect_equal(dfa, r)
})

test_that("merge identical frames with reordered should give identical result" , {
  expect_equal(mergeBetter(x = dfa, y = dfa[c("a", "c", "b")], by.x = "a", by.y = "a"), dfa)
  expect_equal(mergeBetter(x = dfa, y = dfa[c("c", "b", "a")], by.x = "a", by.y = "a"), dfa)
})

test_that("can't handle double duplicate fields, esp not keys", {
  dfA <- dfa[c("a", "b", "c", "c")]
  names(dfA) <- c("a", "b", "C", "c")
  expect_error(mergeBetter(x = dfa, y =  dfA, by.x = "a", by.y = "a"))
  dfA <- dfa[c("a", "b", "b", "c")]
  names(dfA) <- c("a", "b", "B", "c")
  expect_error(mergeBetter(x = dfa, y =  dfA, by.x = "a", by.y = "a"))
  dfA <- dfa[c("a", "a", "b", "c")]
  names(dfA) <- c("a", "A", "b", "c")
  expect_error(mergeBetter(x = dfa, y =  dfA, by.x = "a", by.y = "a"))
  dfA <- dfa[c("a", "a", "b", "b")]
  names(dfA) <- c("a", "A", "B", "b")
  expect_error(mergeBetter(x = dfa, y =  dfA, by.x = "a", by.y = "a"))
})

test_that("merge non-identical frames should suffix field name by default" , {
  r <- mergeBetter(x = dfa, y = dfb, by.x = "a", by.y = "a")
  e <- dfa
  e[["b.dfb"]] <- c(999,12,13,14)
  expect_equal(r, e)
})

test_that("merge identical frames should give identical result" , {
  r <- mergeBetter(x = dfa, y = dfc, by.x = "a", by.y = "a")
  e <- dfa
  e[["b.dfc"]] <- c(999, 12, 13, 14)
  e[["c.dfc"]] <- c(101, 888, 103, 104)
  expect_identical(r, e)

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
