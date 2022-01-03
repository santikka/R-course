# Bangbang (!!) used here to get the actual index values in case of error

testthat::test_that("sections can be initialized", {
    for (i in 1:11) {
        e <- new.env()
        if (i %in% c(3, 10)) {
            testthat::expect_silent(exercises_[[!!i]](e, write = FALSE))
        } else {
            testthat::expect_silent(exercises_[[!!i]](e))
        }
    }
})

testthat::test_that("example solutions are correct", {
    envs <- list()
    for (i in 1:11) {
        envs[[i]] <- new.env()
        if (i %in% c(3, 10)) {
            exercises_[[i]](envs[[i]], write = FALSE)
        } else {
            exercises_[[i]](envs[[i]])
        }
        for (j in seq_along(envs[[i]]$ex)) {
            testthat::expect_true(evaluate_submission(envs[[!!i]]$ex[[!!j]], envs[[!!i]]$ex[[!!j]]$solution))
        }
    }
})