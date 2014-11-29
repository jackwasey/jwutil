context("caching")

test_that("save and retrieve from cache", {

  testdat <- data.frame(a = c(1,2), b = c(4,3))
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
  assignCache(varName = "oliver", value = "writing", envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))
  expect_true(file.exists(findCacheFilePath("oliver")))

  expect_true(isCached("oliver"))
  expect_true(isCached("oliver",
                       cacheDir = findCacheDir()))
  expect_true(isCached("oliver",
                       cacheDir = findCacheDir(cacheDir = findCacheDir())))


  expect_true(any(grepl(pattern = "oliver", x = list.files(findCacheDir()))))
  expect_true(any(grepl(pattern = "oliver", x = lsCache())))
  expect_false(any(grepl(pattern = "not in the cache", x = lsCache())))

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

test_that("cache with date limited files", {

  startDate <- "2010-02-01"
  endDate <- "2011-03-15"

  # make sure this test is starting from a clean slate
  expect_false(exists("jack"))
  expect_false(exists("oliver"))

  mytestenv <- new.env(parent = .GlobalEnv)

  # find the cache
  cd <- tempfile()
  dir.create(cd)
  expect_true(file.exists(cd))  # make sure we're up and running

  assign("jack", "text", envir = mytestenv)

  expect_that(length(lsCache(cacheDir = cd)), testthat::equals(0))

  # save something to cache
  saveToCache("jack", startDate = startDate, endDate = endDate,
              envir = mytestenv, cacheDir = cd)

  expect_equal(lsCache(cacheDir = cd), "jack2010-02-01to2011-03-15.RData")

  expect_true(file.exists(
    findCacheFilePath("jack2010-02-01to2011-03-15", cacheDir = cd)))

  # assign something else to cache
  assignCache("oliver", startDate = startDate, endDate = endDate,
                   value = "writing", envir = mytestenv, cacheDir = cd)
  # we keep both the dated and undated var names.
  expect_true(exists("oliver", envir = mytestenv))
  # we keep both the dated and undated var names?
  #expect_true(exists("oliver2010-02-01to2011-03-15", envir = mytestenv))
  expect_true(file.exists(
    findCacheFilePath("oliver2010-02-01to2011-03-15", cacheDir = cd)))
  expect_true("oliver2010-02-01to2011-03-15.RData"
              %in% lsCache(cacheDir = cd))

  expect_true(isCached(
    getCacheVarDated("oliver", startDate, endDate), cacheDir = cd))

  # same thing, but isCache should know what to do with date range
  expect_true(isCached("oliver", startDate, endDate, cacheDir = cd))

  expect_true(any(grepl("oliver", list.files(findCacheDir(cacheDir = cd)))))
  expect_true(any(grepl("oliver", lsCache(cacheDir = cd))))
  expect_false(any(grepl("not in the cache", lsCache(cacheDir = cd))))

  # we actually don't store the dated variable name in the working environment:
  expect_error(getDated("oliver", startDate = startDate, endDate = endDate,
                        envir = mytestenv))

  expect_equal(getFromCache("oliver", startDate = startDate, endDate = endDate,
                                 cacheDir = cd, envir = mytestenv),
               get("oliver", envir = mytestenv))
  # repeat, without force should just get from memory (and be the same!)
  expect_equal(getFromCache("oliver", startDate = startDate, endDate = endDate,
                                 cacheDir = cd, envir = mytestenv),
               get("oliver", envir = mytestenv))

  suppressWarnings(rm(list = "oliver", envir = mytestenv))
  loadDatedFromCache("oliver", startDate = startDate, endDate = endDate,
                     cacheDir = cd, envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))
  # and bypass cache if already in memory
  loadFromCache("oliver", cacheDir = cd, envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))

  suppressWarnings(rm(list = c("oliver", "oliver2010-02-01to2011-03-15"),
                      envir = mytestenv))
  loadDatedFromCache("oliver", startDate = startDate, endDate = endDate,
                     cacheDir = cd, envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))
  expect_false(exists("oliver2010-02-01to2011-03-15", envir = mytestenv))
  # and bypass cache if already in memory
  loadDatedFromCache("oliver", startDate = startDate, endDate = endDate,
                     cacheDir = cd, envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))
  expect_false(exists("oliver2010-02-01to2011-03-15", envir = mytestenv))

  #   print(pryr::where('jack'))
  #   print(environment())
  #   print(find("jack", ))

  # delete from cache and stated environment
  rmCache("jack", startDate, endDate, envir = mytestenv, cacheDir = cd)
  rmCache("oliver", startDate, endDate, envir = mytestenv, cacheDir = cd)

  # make sure all have been removed.
  expect_false(exists("jack", envir = mytestenv))
  expect_false(exists("oliver, envir = mytestenv"))
  expect_false(exists("jack"))
  expect_false(exists("oliver"))
  expect_false(exists("jack2010-02-01to2011-03-15", envir = mytestenv))
  expect_false(exists("oliver2010-02-01to2011-03-15, envir = mytestenv"))
  expect_false(exists("jack2010-02-01to2011-03-15"))
  expect_false(exists("oliver2010-02-01to2011-03-15"))
  expect_false(file.exists(findCacheFilePath(
    getCacheVarDated("jack", startDate, endDate))))
  expect_false(file.exists(findCacheFilePath(
    getCacheVarDated("oliver", startDate, endDate))))

  file.remove(list.files(cd))
  file.remove(cd)

})
