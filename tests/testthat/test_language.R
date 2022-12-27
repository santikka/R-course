testthat::test_that("no mismatch in translated elements", {
  testthat::expect_identical(names(english), names(finnish))
})

testthat::test_that("no english text within finnish text", {
  testthat::expect_false(any(unlist(english) %in% unlist(finnish)))
})

testthat::test_that("no finnish text within english text", {
  testthat::expect_false(any(unlist(finnish) %in% unlist(english)))
})
