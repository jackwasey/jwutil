context("test character processing functions")

test_that("trim with bad inputs fails" , {
  expect_error(trim())
  expect_error(trim(data.frame(a=c("jack", "hayley"), b=c(1,2))))
})

test_that("trim with empty inputs" , {
  expect_equal(trim(character(0)), character(0))
  expect_equal(trim(""), "")
})

test_that("trim with acceptable inputs" , {
  expect_equal(trim("jack"), "jack")
  expect_equal(trim("jack "), "jack")
  expect_equal(trim(" jack "), "jack")
  expect_equal(trim(" jack"), "jack")
  expect_equal(trim("jack\t"), "jack")
  expect_equal(trim("\tjack\t"), "jack")
  expect_equal(trim("\tjack"), "jack")
  expect_equal(trim("ja ck"), "ja ck")
  expect_equal(trim("ja ck "), "ja ck")
  expect_equal(trim(" ja ck "), "ja ck")
  expect_equal(trim(" ja ck"), "ja ck")
})
