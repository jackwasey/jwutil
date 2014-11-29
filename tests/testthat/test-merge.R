context("test merging")

dfa <- dfb <- dfc <- data.frame(a = c(1,2,3,4),
                                b = c(11,12,13,14),
                                c = c(101,102,103,104))
dfb[1, "b"] <- 999
dfc[1, "b"] <- 999
dfc[2, "c"] <- 888

test_that("merge identical frames should give identical result unless prefix or suffix requested" , {
  r <- mergeBetter(x = dfa, y = dfa, by.x = "a", by.y = "a", verbose = TRUE)
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
  r <- mergeBetter(x = dfa, y = dfb, by.x = "a", by.y = "a", verbose = TRUE)
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

test_that("merge with missing id field fails with error", {
  expect_that(mergeBetter(x = dfa, y = dfb, by.x = "sloe", by.y = "gin"), throws_error())
})

test_that("merge frames forcing prefix" , {

  r <- mergeBetter(x = dfa, y = dfa, by.x = "a", by.y = "a", renameAll = "prefix")

  r <- mergeBetter(x = dfa, y = dfc, by.x = "a", by.y = "a", renameAll = "prefix")
  e <- structure(list(a = c(1, 2, 3, 4),
                      b = c(11, 12, 13, 14),
                      c = c(101, 102, 103, 104),
                      dfc.b = c(999, 12, 13, 14),
                      dfc.c = c(101, 888, 103, 104)),
                 .Names = c("a", "b", "c", "dfc.b", "dfc.c"),
                 class = "data.frame")
  expect_equivalent(r, e)

  r <- mergeBetter(x = dfa, y = dfc, by.x = "a", by.y = "a", renameAll = "suffix", verbose = TRUE)
  e <- structure(list(a = c(1, 2, 3, 4),
                      b = c(11, 12, 13, 14),
                      c = c(101, 102, 103, 104),
                      dfc.b = c(999, 12, 13, 14),
                      dfc.c = c(101, 888, 103, 104)),
                 .Names = c("a", "b", "c", "b.dfc", "c.dfc"),
                 class = "data.frame")
  expect_equivalent(r, e)

  r <- mergeBetter(x = dfa, y = dfc, by.x = "a", by.y = "a", renameAll = "suffix", affix = "jack")
  e <- structure(list(a = c(1, 2, 3, 4),
                      b = c(11, 12, 13, 14),
                      c = c(101, 102, 103, 104),
                      dfc.b = c(999, 12, 13, 14),
                      dfc.c = c(101, 888, 103, 104)),
                 .Names = c("a", "b", "c", "b.jack", "c.jack"),
                 class = "data.frame")
  expect_equivalent(r, e)

  skip("to do tests covering case where there are duplicate frames but rename is explicitly requested")
})