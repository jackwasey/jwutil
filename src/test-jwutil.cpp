#ifdef HAVE_TESTTHAT_H
#include <testthat.h>
#include <Rcpp.h>

context("C++ Catch") {
  test_that("two plus two is four") {
    int result = 2 + 2;
    expect_true(result == 4);
  }
}

context("get OMP max threads") {
  test_that("max threads give a semi-sensible number") {
    int i = getOmpMaxThreads();
    expect_true(i >= 0);
    debug_parallel();
  }
}

context("get OMP current threads") {
  test_that("threads give a semi-sensible number") {
    int i = getOmpThreads();
    expect_true(i >= 0);
    debug_parallel();
  }
}

context("valgrind hooks") {
  test_that("start callgrind") {
    int i = valgrindCallgrindStart(false);
    expect_true(i == 0);
  }

  test_that("stop callgrind") {
    int i = valgrindCallgrindStop();
    expect_true(i == 0);
  }
}

context("fast int to string") {
  test_that("Rcpp version works") {
    Rcpp::IntegerVector iv;
    iv = Rcpp::IntegerVector::create(1);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "1");

    iv = Rcpp::IntegerVector::create(9);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "9");

    iv = Rcpp::IntegerVector::create(123456);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "123456");

    iv = Rcpp::IntegerVector::create(2, 33, 444, 5555, 66666, 123456);
    CV cv = CV::create("2", "33", "444", "5555", "66666", "123456");
    expect_true(Rcpp::is_true(Rcpp::all(fastIntToStringRcpp(iv) == cv)));

  }

  // duplicated code...
  test_that("std version works") {
    std::vector<int> iv;
    iv.push_back(1);
    VecStr cv;
    cv.push_back("1");
    expect_true(fastIntToStringStd(iv) == cv);

    iv[0] = 9;
    cv[0] = "9";
    expect_true(fastIntToStringStd(iv) == cv);

    iv[0] = 123456;
    cv[0] = "123456";
    expect_true(fastIntToStringStd(iv) == cv);

    iv.push_back(2);
    iv.push_back(33);
    iv.push_back(444);
    iv.push_back(5555);
    iv.push_back(66666);
    cv.push_back("2");
    cv.push_back("33");
    cv.push_back("444");
    cv.push_back("5555");
    cv.push_back("66666");
    expect_true(fastIntToStringStd(iv) == cv);
  }

}

// endif have testthat
#endif
