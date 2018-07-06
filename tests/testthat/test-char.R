context("test character processing functions")

test_that("trim with bad inputs fails", {
  expect_error(trim())
  expect_error(trim(data.frame(a = c("jack", "hayley"), b = c(1, 2))))
})

test_that("trim with empty inputs", {
  expect_equal(trim(character(0)), character(0))
  expect_equal(trim(""), "")
})

test_that("trim with acceptable inputs", {
  expect_equal(trim(NA_character_), NA_character_)
  # not necesarily desirable, but this states the expectation.
  expect_equal(trim(NA_integer_), NA_character_)
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
  expect_equal(trim(c(" jack", "hayley ")), c("jack", "hayley"))
})

test_that("strip whitespace", {
  expect_error(strip(x = "jack", pattern = c(1, 2)))
  expect_error(strip(x = "jack", pattern = c("1", "2")))
  expect_error(strip(x = "jack", useBytes = "yes"))
  expect_error(strip(x = "jack", useBytes = c(TRUE, FALSE)))
  expect_equal(strip(NA_integer_), NA_character_)
  expect_equal(strip("jack"), "jack")
  expect_equal(strip("jack "), "jack")
  expect_equal(strip(" jack "), "jack")
  expect_equal(strip(" jack"), "jack")
  expect_equal(strip("ja ck"), "jack")
  expect_equal(strip("ja ck "), "jack")
  expect_equal(strip(" ja ck "), "jack")
  expect_equal(strip(" ja ck"), "jack")
  expect_equal(strip(pattern = "  ", "jack  "),  "jack")
  expect_equal(strip(pattern = "  ", "  jack  "),  "jack")
  expect_equal(strip(pattern = "  ", "  jack"),  "jack")
  expect_equal(strip(pattern = "  ", "ja  ck"),  "jack")
  expect_equal(strip(pattern = "  ", "ja  ck  "),  "jack")
  expect_equal(strip(pattern = "  ", "  ja  ck  "),  "jack")
  expect_equal(strip(pattern = "  ", "  ja  ck"),  "jack")
  expect_equal(strip("jack\t", pattern = "\t"), "jack")
  expect_equal(strip("\tjack\t", pattern = "\t"), "jack")
  expect_equal(strip("\tjack", pattern = "\t"), "jack")
  expect_equal(strip("\tjac\tk", pattern = "\t"), "jack")
  expect_equal(strip(c(" jac k", "h ayley ")), c("jack", "hayley"))
})

test_that("strip characters so they can work in a formula", {
  expect_error(strip_for_formula())
  # if we introduce a duplicate, we should stop
  expect_error(strip_for_formula(c("jack", "jack")))
  expect_error(strip_for_formula(c("jack", "ja~ck")))
  expect_error(strip_for_formula(c("ja~ck", "jack")))
  expect_equal(strip_for_formula(NA_integer_), NA_character_)
  expect_equal(strip_for_formula("jack"), "jack")
  expect_equal(strip_for_formula("jack "), "jack")
  expect_equal(strip_for_formula(" jack "), "jack")
  expect_equal(strip_for_formula(" jack"), "jack")
  expect_equal(strip_for_formula("ja ck"), "jack")
  expect_equal(strip_for_formula("ja ck "), "jack")
  expect_equal(strip_for_formula(" ja ck "), "jack")
  expect_equal(strip_for_formula(" ja ck"), "jack")
  expect_equal(strip_for_formula("jack  "),  "jack")
  expect_equal(strip_for_formula("  jack  "),  "jack")
  expect_equal(strip_for_formula("  jack"),  "jack")
  expect_equal(strip_for_formula("ja  ck"),  "jack")
  expect_equal(strip_for_formula("ja  ck  "),  "jack")
  expect_equal(strip_for_formula("  ja  ck  "),  "jack")
  expect_equal(strip_for_formula("  ja  ck"),  "jack")
  expect_equal(strip_for_formula("jack\t"), "jack")
  expect_equal(strip_for_formula("\tjack\t"), "jack")
  expect_equal(strip_for_formula("\tjack"), "jack")
  expect_equal(strip_for_formula("\tjac\tk"), "jack")
  expect_equal(
    strip_for_formula("\t+j-a#^&!*#(%!^$&#%!c\\&!(#)^*&^!~\t|||||k"),
    "jack")
  expect_equal(strip_for_formula(c(" jac k", "h ayley ")), c("jack", "hayley"))
})

test_that("string pair match extraction", {
  expect_error(str_pair_match())
  expect_error(str_pair_match(pattern = "(abc)def(ghi)"))
  expect_error(str_pair_match(string = "bougie"))
  expect_error(str_pair_match(pattern = c("(a)b(c)", "(d)e(f)"),
                              string = "abc"))
  expect_error(str_pair_match(pattern = c("(a)b(c)", "(d)e(f)"),
                              string = c("abc", "def")))

  expect_error(str_pair_match(pattern = "[", string = "abc")) # invalid regex
  # only one parenthesis
  expect_error(str_pair_match(pattern = "(a).*", string = "abc"))
  expect_error(str_pair_match(pattern = ".*(j)", string = "abc"))
  expect_equal(str_pair_match(pattern = "(a*)b(c*)", string = "abc"), c(a = "c"))
  expect_equal(str_pair_match(pattern = "([^mackarel]*)(spain)",
                              string = "togospain"),
               c(togo = "spain"))
  expect_equal(str_pair_match(pattern = "([^mackarel]*)(spain)",
                              string = c("togospain", "djiboutispain")),
               c(togo = "spain", djibouti = "spain"))
  expect_equal(str_pair_match(pattern = "(a*)b(c*)", string = c("abc", "aabcc")),
               c(a = "c", aa = "cc"))
})

test_that("str_pair_match error if more than two outputs", {
  expect_error(str_pair_match(string = "hadoop", pattern = "(ha)(do)(op)"))
  # no error if explicit
  str_pair_match(string = "hadoop", pattern = "(ha)(do)(op)", pos = c(1, 2))
})
