testthat::test_that("JYU exam can be initialized", {
  e <- new.env()
  testthat::expect_silent(exams_$JYU(e))
})

testthat::test_that("UEF exam can be initialized", {
  e <- new.env()
  testthat::expect_silent(exams_$UEF(e))
})
