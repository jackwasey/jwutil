context("test testing extensions for testthat")

test_that("combinations of commutative arguments", {
  # result has 'info' for the last test before returning.
  result <- expect_that_combine_all_args(sum(1, 2), testthat::equals(3))
  #   result <- expect_that_combine(sum(1,2,3), testthat::equals(6))
  #   result <- expect_that_combine(sum(1,2,3,4), testthat::equals(10))
  #   result <- expect_that_combine(sum(1,2,3,4,5), testthat::equals(15))
  #   result <- expect_that_combine(sum(1,2,3,4,5,6), testthat::equals(21))
  result <- expect_that_combine_all_args(sum(1,2,3,4,5,6,7), testthat::equals(28)) # this runs 7! = 5040 tests!

})

test_that("throw error for ?every iteration", {
  result <- expect_that_combine_all_args(stop("a", "b"), testthat::throws_error())
})
