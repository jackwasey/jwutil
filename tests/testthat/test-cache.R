context("caching")

test_that("save and retrieve from cache", {

  testdat <- data.frame(a=c(1,2), b=c(4,3))
  saveToCache(varName = "testdat")
  expect_that(getFromCache(varName = "testdat"), testthat::equals(testdat))

})

test_that("error, not warning, for file not found", {
    expect_error(getFromCache("badger cull"))
})
