
context("data frame manipulation")


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

  expect_warning(out <- expandFactors(dframe, considerFactors=NULL))
  #expect_output(out <- expandFactors(dframe, considerFactors=NULL), 'WARNING')
  expect_identical(dframe, out$dat)
  expect_true(is.null(out$newFields))

  out <- expandFactors(dframe, considerFactors=c("f1","f2"))
  expect_equal(dim(out$dat), c(9,6))
  expect_identical(out$newFields, c("f1.1","f1.2","f1.3","f2.10","f2.20","f2.30"))
  expect_equal(class(out$dat[[1]]), "logical")
  expect_equal(names(out$dat), c("f1.1","f1.2","f1.3","f2.10","f2.20","f2.30"))

  out <- expandFactors(dframe, considerFactors=c("f1"))
  expect_equal(dim(out$dat), c(9,4))
  expect_identical(out$newFields, c("f1.1","f1.2","f1.3"))
  expect_equal(class(out$dat[["f1.1"]]), "logical")
  expect_equal(names(out$dat), c("f2","f1.1","f1.2","f1.3"))

  out <- expandFactors(mixed.df, considerFactors=c("v1","f1"))
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


  dfa <- data.frame(a = c(1,2,3,4), b = c(11,12,13,14), c = c(101,102,103,104))
  dfb <- dfa
  dfb[1, "b"] <- 999
  dfc <- dfb
  dfc[2, "c"] <- 888


test_that("merge identical frames should give identical result" , {
  r <- mergeBetter(x = dfa, y = dfa, by.x = "a", by.y = "a")
  expect_equal(dfa, r)
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
  e[["b.dfc"]] <- c(999,12,13,14)
  e[["c.dfc"]] <- c(101,888,103,104)
  expect_equal(r, e)

})

