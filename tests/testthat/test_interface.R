testthat::test_that("interface works", {
  expect_message(select_language("english"), NULL)
  expect_message(info(), NULL)
})
