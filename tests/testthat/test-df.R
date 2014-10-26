context("data frame manipulation")


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

