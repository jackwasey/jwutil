context("caching")

test_that("save and retrieve from cache", {

  testdat <- data.frame(a=c(1,2), b=c(4,3))
  saveToCache(varName = "testdat")
  expect_that(getFromCache(varName = "testdat"), testthat::equals(testdat))

})

test_that("error, not warning, for file not found", {
    expect_error(getFromCache("badger cull"))
})

test_that("integration test of caching", {
  # find the cache
  cachedir <- findCacheDir()
  expect_true(file.exists(cachedir))
  jack = "text"

  # save something to cache
  saveToCache("jack")
  expect_that(file.exists(findCacheFilePath("jack")))

  # assign something else to cache
  assignCache("oliver", value = "writing")
  expect_true(exists("oliver"))
  expect_true(file.exists(findCacheFilePath("oliver")))



})
