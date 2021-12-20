testthat::test_that("JYU exam part 1 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exams_$JYU[[1]](e))
})

testthat::test_that("JYU exam part 2 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exams_$JYU[[2]](e))
})

testthat::test_that("UEF exam can be initialized", {
    e <- new.env()
    testthat::expect_silent(exams_$UEF(e))
})