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

  # make sure this test is starting from a clean slate
  expect_false(exists("jack"))
  expect_false(exists("oliver"))

  mytestenv <- new.env(parent = .GlobalEnv)

  # find the cache
  cachedir <- findCacheDir()
  expect_true(file.exists(cachedir))

  assign("jack", "text", envir = mytestenv)

  initialcache <- lsCache()

  # save something to cache
  saveToCache("jack", envir = mytestenv)
  expect_true(file.exists(findCacheFilePath("jack")))

  # assign something else to cache
  assignCache("oliver", value = "writing", envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))
  expect_true(file.exists(findCacheFilePath("oliver")))

  #   print(pryr::where('jack'))
  #   print(environment())
  #   print(find("jack", ))

  # delete from cache and environment
  rmCache("jack", envir = mytestenv)
  rmCache("oliver", envir = mytestenv)

  # make sure all have been removed.
  expect_false(exists("jack", envir = mytestenv))
  expect_false(exists("oliver, envir = mytestenv"))
  expect_false(exists("jack"))
  expect_false(exists("oliver"))
  expect_false(file.exists(findCacheFilePath("jack")))
  expect_false(file.exists(findCacheFilePath("oliver")))

})
