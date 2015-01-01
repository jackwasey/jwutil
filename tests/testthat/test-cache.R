context("caching")

testdat <- data.frame(a = c(1,2), b = c(4,3))

test_that("save and retrieve from cache", {
  saveToCache(var = "testdat")
  expect_that(getFromCache(var = "testdat"), testthat::equals(testdat))
  expect_error(saveToCache())

})

test_that("error, not warning, for file not found", {
  expect_error(getFromCache("badger cull"))
})

mytestenv <- new.env(parent = globalenv())
cachedir <- findCacheDir()

test_that("integration test of caching", {

  # make sure this test is starting from a clean slate
  expect_false(exists("jack"))
  expect_false(exists("oliver"))


  # find the cache
  expect_true(file.exists(cachedir))

  assign("jack", "text", envir = mytestenv)

  initialcache <- lsCache()

  # save something to cache
  saveToCache("jack", envir = mytestenv)
  expect_true(file.exists(findCacheFilePath("jack")))

  # assign something else to cache
  assignCache(var = "oliver", value = "writing", envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))
  expect_true(file.exists(findCacheFilePath("oliver")))

  expect_true(isCached("oliver"))
  expect_true(isCached("oliver",
                       cacheDir = findCacheDir()))
  expect_true(isCached("oliver",
                       cacheDir = findCacheDir(cacheDir = findCacheDir())))

  renameCache("oliver", "atlas")

  expect_true(isCached("atlas"))
  expect_false(isCached("oliver"))
  renameCache("atlas", "oliver")

  expect_true(any(grepl(pattern = "oliver", x = list.files(findCacheDir()))))
  expect_true(any(grepl(pattern = "oliver", x = lsCache())))
  expect_false(any(grepl(pattern = "not in the cache", x = lsCache())))

  # delete from cache
  rmCache("jack")
  rmCache("oliver")

  # make sure all have been removed from cache. don't worry about the
  # environment anymore.
  expect_false(file.exists(findCacheFilePath("jack")))
  expect_false(file.exists(findCacheFilePath("oliver")))

})

test_that("cache with date limited files", {

  from <- as.Date("2010-02-01")
  to <- as.Date("2011-03-15")
  from2 <- as.Date("2012-02-01")
  to2 <- as.Date("2013-03-15")

  # make sure this test is starting from a clean slate in current env
  suppressWarnings(rm(list = c("jack", "oliver")))

  mytestenv <- new.env(parent = globalenv())

  # create a cache "file", actually a path which we make a directory
  cd <- tempfile()
  dir.create(cd)
  expect_true(file.exists(cd))  # make sure we're up and running

  assign("jack", "somedata", envir = mytestenv)

  expect_that(length(lsCache(cacheDir = cd)), testthat::equals(0))

  saveToCache("jack", from = from, to = to,
              envir = mytestenv, cacheDir = cd)

  expect_equal(lsCacheFiles(cacheDir = cd), "jack2010-02-01to2011-03-15.RData")
  expect_equal(lsCache(cacheDir = cd), "jack2010-02-01to2011-03-15")
  saveToCache("jack", from = from2, to = to2,
              envir = mytestenv, cacheDir = cd)

  expect_true(all(c("jack2010-02-01to2011-03-15", "jack2012-02-01to2013-03-15")
                  %in% lsCache(cacheDir = cd)))

  expect_false(file.exists(findCacheFilePath("jack", cacheDir = cd)))
  expect_false(isCached("jack", cacheDir = cd))
  expect_true(
    file.exists(findCacheFilePath("jack2010-02-01to2011-03-15", cacheDir = cd)))
  expect_true(isCached("jack2010-02-01to2011-03-15", cacheDir = cd))
  expect_true(
    file.exists(findCacheFilePath("jack2012-02-01to2013-03-15", cacheDir = cd)))
  expect_true(isCached("jack2012-02-01to2013-03-15", cacheDir = cd))
  # same again, but check these things don't exist in default environment, too:
  expect_false(isCached("jack", cacheDir = cd))
  expect_true(isCached("jack", from = from, to = to, cacheDir = cd))
  expect_true(isCached("jack2010-02-01to2011-03-15", cacheDir = cd))

  renameCache("jack", "douglas", cacheDir = cd)

  # disk cache no trace?
  expect_false("jack" %in% lsCache(cacheDir = cd))
  expect_false("jack2010-02-01to2011-03-15.RData" %in% lsCache(cacheDir = cd))
  expect_false("jack2012-02-01to2013-03-15.RData" %in% lsCache(cacheDir = cd))

  expect_false(file.exists(findCacheFilePath("jack", cacheDir = cd)))
  expect_false(isCached("jack", cacheDir = cd))
  expect_false(
    file.exists(findCacheFilePath("jack2010-02-01to2011-03-15", cacheDir = cd)))
  expect_false(isCached("jack2010-02-01to2011-03-15",cacheDir = cd))
  expect_false(file.exists(findCacheFilePath("jack2012-02-01to2013-03-15",
                                             cacheDir = cd)))
  expect_false(isCached("jack2012-02-01to2013-03-15", cacheDir = cd))
  expect_false(isCached("jack2012-02-01to2013-03-15", cacheDir = cd))
  # same again, but check these things don't exist in default environment, too:
  expect_false(isCached("jack", cacheDir = cd))
  expect_false(isCached("jack2010-02-01to2011-03-15", cacheDir = cd))
  expect_false(isCached("jack2012-02-01to2013-03-15", cacheDir = cd))
  # and did 'douglas' appear instead?
  expect_false("douglas" %in% lsCache(cacheDir = cd))
  expect_true("douglas2010-02-01to2011-03-15" %in% lsCache(cacheDir = cd))
  expect_true("douglas2012-02-01to2013-03-15" %in% lsCache(cacheDir = cd))

  expect_false(file.exists(findCacheFilePath("douglas", cacheDir = cd)))
  expect_false(isCached("douglas", cacheDir = cd))
  expect_false(isCached("douglas", cacheDir = cd))
  expect_true(isCached("douglas", from = from, to = to, cacheDir = cd))
  expect_true(isCached("douglas", from = from2, to = to2, cacheDir = cd))
  expect_true(file.exists(findCacheFilePath("douglas2010-02-01to2011-03-15",
                                            cacheDir = cd)))
  expect_true(isCached("douglas2010-02-01to2011-03-15", cacheDir = cd))
  expect_true(file.exists(findCacheFilePath("douglas2012-02-01to2013-03-15",
                                            cacheDir = cd)))
  expect_true(isCached("douglas2012-02-01to2013-03-15", cacheDir = cd))
  # same again, but check these things don't exist in default environment, too:
  expect_false(isCached("douglas", cacheDir = cd))

  expect_true(isCached("douglas", from = from, to = to,cacheDir = cd))
  expect_true(isCached("douglas", from = from2, to = to2, cacheDir = cd))
  expect_true(isCached("douglas2010-02-01to2011-03-15", cacheDir = cd))
  expect_true(isCached("douglas2012-02-01to2013-03-15", cacheDir = cd))

  expect_true(isCached("douglas", from = from, to = to, cacheDir = cd))
  expect_true(isCached("douglas", from = from2, to = to2, cacheDir = cd))
  expect_true(isCached("douglas2010-02-01to2011-03-15", cacheDir = cd))
  expect_true(isCached("douglas2012-02-01to2013-03-15", cacheDir = cd))

  # are we back to where we started?
  renameCache("douglas", "jack", cacheDir = cd)

  expect_false(file.exists(findCacheFilePath("jack", cacheDir = cd)))
  expect_false(isCached("jack", cacheDir = cd))
  expect_true(file.exists(findCacheFilePath("jack2010-02-01to2011-03-15",
                                            cacheDir = cd)))
  expect_true(isCached("jack2010-02-01to2011-03-15", cacheDir = cd))
  expect_true(file.exists(findCacheFilePath("jack2012-02-01to2013-03-15",
                                            cacheDir = cd)))
  expect_true(isCached("jack2012-02-01to2013-03-15", cacheDir = cd))
  # same again, but check these things don't exist in default environment, too:
  expect_true(isCached("jack2010-02-01to2011-03-15", cacheDir = cd))
  expect_true(isCached("jack2012-02-01to2013-03-15", cacheDir = cd))

  # assign something else to cache
  assignCache("oliver", from = from, to = to,
              value = "writing", envir = mytestenv, cacheDir = cd)
  expect_true(file.exists(
    findCacheFilePath("oliver2010-02-01to2011-03-15", cacheDir = cd)))
  expect_true("oliver2010-02-01to2011-03-15" %in% lsCache(cacheDir = cd))

  expect_true(isCached(
    getCacheVarDated("oliver", from, to), cacheDir = cd))

  # same thing, but isCache should know what to do with date range
  expect_true(isCached("oliver", from, to, cacheDir = cd))

  expect_true(any(grepl("oliver", list.files(findCacheDir(cacheDir = cd)))))
  expect_true(any(grepl("oliver", lsCache(cacheDir = cd))))
  expect_false(any(grepl("not in the cache", lsCache(cacheDir = cd))))

  expect_equal(getFromCache("oliver", from = from, to = to, cacheDir = cd),
               get("oliver", envir = mytestenv))
  # repeat, without force should just get from memory (and be the same!)
  expect_equal(getFromCache("oliver", from = from, to = to, cacheDir = cd),
               get("oliver", envir = mytestenv))

  suppressWarnings(rm(list = "oliver", envir = mytestenv))
  loadFromCache("oliver", from = from, to = to,
                cacheDir = cd, envir = mytestenv)

  expect_true(exists("oliver", envir = mytestenv))
  expect_false(exists("oliver2010-02-01to2011-03-15", envir = mytestenv))

  suppressWarnings(rm(list = c("oliver", "oliver2010-02-01to2011-03-15"),
                      envir = mytestenv))
  loadFromCache("oliver", from = from, to = to,
                cacheDir = cd, envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))
  expect_false(exists("oliver2010-02-01to2011-03-15", envir = mytestenv))
  # and bypass cache if already in memory
  loadFromCache("oliver", from = from, to = to,
                cacheDir = cd, envir = mytestenv)
  expect_true(exists("oliver", envir = mytestenv))
  expect_false(exists("oliver2010-02-01to2011-03-15", envir = mytestenv))

  #   print(pryr::where('jack'))
  #   print(environment())
  #   print(find("jack", ))

  # delete from cache and stated environment
  rmCache(var = "jack", from = from, to = to, cacheDir = cd)
  rmCache(var = "oliver", from = from, to = to, cacheDir = cd)

  # make sure all have been removed.
  expect_false(file.exists(findCacheFilePath(
    getCacheVarDated("jack", from, to))))
  expect_false(file.exists(findCacheFilePath(
    getCacheVarDated("oliver", from, to))))

  #file.remove(file.path(cd, list.files(cd)))
  unlink(cd)

})
