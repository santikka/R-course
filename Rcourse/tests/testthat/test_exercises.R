testthat::test_that("section 1 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[1]](e))
})

testthat::test_that("section 2 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[2]](e))
})

testthat::test_that("section 3 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[3]](e, write = FALSE))
})

testthat::test_that("section 4 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[4]](e))
})

testthat::test_that("section 5 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[5]](e))
})

testthat::test_that("section 6 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[6]](e))
})

testthat::test_that("section 7 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[7]](e))
})

testthat::test_that("section 8 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[8]](e))
})

testthat::test_that("section 9 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[9]](e))
})

testthat::test_that("section 10 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[10]](e, write = FALSE))
})

testthat::test_that("section 11 can be initialized", {
    e <- new.env()
    testthat::expect_silent(exercises_[[11]](e))
})
