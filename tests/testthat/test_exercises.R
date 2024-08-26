# Bangbang (!!) used here to get the actual index values in case of error

testthat::test_that("sections can be initialized", {
  for (i in seq_along(exercises_)) {
    e <- new.env()
    testthat::expect_silent(exercises_[[!!i]](e, write = FALSE))
  }
})

testthat::test_that("example solutions are correct", {
  envs <- list()
  for (i in seq_along(exercises_)) {
    envs[[i]] <- new.env()
    exercises_[[i]](envs[[i]], write = FALSE)
    for (j in seq_along(envs[[i]]$ex)) {
      testthat::expect_true(
        evaluate_submission(
          envs[[!!i]]$ex[[!!j]]$solution,
          envs[[!!i]]$ex[[!!j]]
        )
      )
    }
  }
})

testthat::test_that("example code snippets are correct", {
  envs <- list()
  for (i in seq_along(exercises_)) {
    envs[[i]] <- new.env()
    exercises_[[i]](envs[[i]], test = TRUE)
    null_data <- is.null(envs[[i]]$data)
    for (j in seq_along(envs[[i]]$ex)) {
      dat <- envs[[i]]$ex[[j]]$data
      e <- new.env()
      if (!null_data) {
        dat_names <- names(dat)
        for (d in dat_names) {
          e[[d]] <- envs[[i]]$data[[dat[[d]]]]
        }
      }
      code <- envs[[i]]$ex[[j]]$code
      code <- strsplit(code, "\n", fixed = TRUE)[[1]]
      eval(
        str2expression(
          paste0(code[seq_along(code) - 1], collapse = "\n")
        ),
        envir = e
      )
      solution_submit <- code[length(code)]
      solution_match <- regexec(
        pattern = ".*submit\\((.+)\\)",
        text = solution_submit,
        perl = TRUE
      )
      solution_obj_name <- regmatches(solution_submit, solution_match)[[1]][2]
      if (!is.na(solution_obj_name)) {
        solution_obj <- get(x = solution_obj_name, envir = e)
        testthat::expect_true(
          evaluate_submission(
            solution_obj,
            envs[[!!i]]$ex[[!!j]]
          )
        )
      }
    }
  }
})
