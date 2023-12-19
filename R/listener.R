## A listener that uses the callback functionality to provide an interactive interface
listen <- function(...) UseMethod("listen")

listen.default <- function(e) {
  # Initialize the current section, verify the working directory and construct the exercises
  if (e$init) {
    if (detect("exit", e$expr)) {
      cleanup(e)
      return(FALSE)
    }
    if (initialize_listener(e)) {
      return(TRUE)
    }
  }
  # Update the remaining exam time
  if (e$is_exam) {
    if (!update_time(e)) {
      return(FALSE)
    }
    if (e$record) {
      e$code[e$ix] <- paste0(
        e$code[e$ix],
        "\n",
        ifelse(e$incoming, repchar("-", e$con_width), ""),
        deparse1(e$expr, width.cutoff = 100)
      )
      e$incoming <- FALSE
    }
  }
  detected <- e$funs[detect(e$funs, e$expr)]
  if (length(detected)) {
    # Process navigation commands
    process <- do.call(
      what = paste0("process_", detected[1L]),
      args = list(e = e)
    )
    if (!is.null(process)) {
      return(process)
    }
  }
  # A (correct) answer was submitted, present the next exercise
  if (e$ask && e$ix <= e$n_ex) {
    show_next(e)
  }
  # Last problem is completed
  if (e$ix > e$n_ex) {
    return(process_completion(e))
  }
  return(TRUE)
}

# Initialize the listener environment
initialize_listener <- function(e) {
  if (e$require_wd) {
    file_path <- paste0(getwd(), e$file)
    if (file.exists(file_path)) {
      translate_message("The working directory was set successfully!")
      custom_message("")
      e$require_wd <- FALSE
    } else {
      if (e$init_warn) {
        e$init_warn <- FALSE
      } else {
        translate_message("Unable to find the required data files!")
        translate_message("The data files must reside in the working directory itself and not in a subdirectory for example.")
      }
      return(TRUE)
    }
  }
  if (!e$require_wd) {
    if (e$is_exam) {
      if (identical(e$inst, "1")) {
        options(prompt = paste0(l() %a% "exam", e$part, "> "))
        exams_$JYU[[e$part]](e)
      } else {
        options(prompt = paste0(l() %a% "exam", "> "))
        exams_$UEF(e)
      }
    } else {
      exercises_[[e$part]](e)
    }
    e$n_ex <- length(e$ex)
    if (e$is_exam) {
      e$visited <- logical(e$n_ex)
      e$questions <- sapply(e$ex, "[[", "question")
      e$answers <- rep(list("This question was skipped by the student"), e$n_ex)
      e$submitted <- logical(e$n_ex)
      e$code <- character(e$n_ex)
      e$mutable <- sapply(e$ex, function(x) x$params$mutable)
      e$times <- integer(e$n_ex)
      e$current_start <- Sys.time()
      if (!e$test_mode) {
        verification(e)
      }
    } else {
      e$completed <- logical(e$n_ex)
    }
    e$con_width <- floor(0.9 * getOption("width"))
    e$init <- FALSE
    e$ask <- TRUE
    e$ix <- 1L
    e$funs <- c(
      "ask",
      "exit",
      "skip",
      "submit",
      "solution",
      "code",
      "go"
    )
  }
  return(FALSE)
}

update_time <- function(e) {
  time_remaining <- as.integer(difftime(e$time_start, Sys.time(), units = "mins")) + e$time_max
  if (time_remaining <= 0) {
    translate_message("The exam time has expired!")
    verification(e, verbose = TRUE)
    cleanup(e)
    return(FALSE)
  }
  e$modulus <- time_remaining %% 60
  e$hours <- (time_remaining - e$modulus) / 60
  e$minutes <- floor(e$modulus)
  return(TRUE)
}

process_ask <- function(e) {
  custom_message(repchar("-", e$con_width))
  translate_message(e$ex[[e$ix]]$question)
  custom_message(repchar("-", e$con_width))
  return(NULL)
}

process_code <- function(e) {
  if (e$is_exam) {
    translate_message("Code cannot be shown for the exam.")
  } else {
    cat(translate_code(e$ex[[e$ix]]$code), "\n")
  }
  return(NULL)
}

process_exit <- function(e) {
  if (e$is_exam) {
    if (!e$completed && e$warn_complete) {
      translate_message("You have not yet submitted answers to every exam question.")
      translate_message("If you exit now, your results will be saved, but you cannot continue the exam afterwards.")
      translate_message("Are you sure you want to exit? (type exit() again to confirm).")
      e$warn_complete <- FALSE
      return(TRUE)
    }
    if (!e$test_mode) {
      verification(e, verbose = TRUE)
    }
  }
  cleanup(e)
  return(FALSE)
}

process_go <- function(e) {
  if (is.null(e$val)) {
    if (e$is_exam) {
      translate_message("Error: no question number given.")
    } else {
      translate_message("Error: no exercise number given.")
    }
    return(TRUE)
  }
  e$val <- suppressWarnings(try(as.integer(e$val), silent = TRUE))
  if ("try-error" %in% class(e$val) || is.na(e$val) || is.nan(e$val)) {
    translate_message("Invalid argument.")
    return(TRUE)
  }
  if (identical(e$val, e$ix)) {
    return(TRUE)
  }
  if (e$is_exam) {
    if (e$val > e$max_ix) {
      translate_message("The exam problems must be completed in order.")
      translate_message("However, you can go back to some questions you have already answered.")
      custom_message(l() %a% "You can go up to question number", " ", e$max_ix, ".")
      return(TRUE)
    }
  }
  if (e$val > 0 && e$val <= e$n_ex) {
    if (e$is_exam) {
      if (!e$mutable[e$val] && e$max_ix > e$val) {
        custom_message(l() %a% "You cannot return to question number", " ", e$val, ".")
        return(TRUE)
      }
    }
    e$ix <- e$val
    e$ask <- TRUE
    e$incoming <- TRUE
    return(NULL)
  } else {
    custom_message(l() %a% "Error: the number must be between", " ", 1, " ", l() %a% "and", " ", e$n_ex, ".")
    return(TRUE)
  }
}

process_skip <- function(e) {
  if (e$is_exam) {
    if (!e$submitted[e$ix] && e$warn_submit) {
      translate_message("You have not submitted an answer for this problem, are you sure you want to skip? (type skip() again to confirm)")
      e$warn_submit <- FALSE
      return(TRUE)
    }
    e$times[e$ix] <- as.integer(difftime(Sys.time(), e$current_start, units = "secs"))
    e$visited[e$ix] <- TRUE
    e$ix <- e$ix + 1L
    e$ask <- TRUE
    while (!e$mutable[e$ix] && e$ix <= e$n_ex && e$visited[e$ix]) {
      e$ix <- e$ix + 1L
    }
  } else {
    e$ix <- e$ix + 1L
    e$ask <- TRUE
  }
  return(NULL)
}

process_solution <- function(e) {
  if (e$is_exam) {
    translate_message("Solutions cannot be shown for the exam.")
  } else {
    if (e$ex[[e$ix]]$params$is_function) {
      cat(deparse1(e$ex[[e$ix]]$solution, collapse = "\n"), "\n")
    } else if (e$ex[[e$ix]]$params$graphical) {
      eval(e$ex[[e$ix]]$solution, envir = globalenv())
    } else {
      print(e$ex[[e$ix]]$solution)
    }
  }
  return(TRUE)
}

process_submit <- function(e) {
  if (e$check_answers &&
      (is.null(e$val) || length(e$val) > 1L || !exists(e$val, envir = globalenv()))) {
    translate_message("A submitted answer must be an R object in the global environment.")
    return(TRUE)
  }
  result <- NULL
  answer <- get(e$val, envir = globalenv())
  if (e$is_exam) {
    e$times[e$ix] <- as.integer(difftime(Sys.time(), e$current_start, units = "secs"))
    e$visited[e$ix] <- TRUE
    e$submitted[e$ix] <- TRUE
    if (!is.null(e$val) && !is.null(answer)) {
      e$answers[[e$ix]] <- answer
    }
    e$ix <- e$ix + 1L
    e$ask <- TRUE
    while (!e$mutable[e$ix] && e$ix <= e$n_ex && e$visited[e$ix]) {
      e$ix <- e$ix + 1L
    }
  } else {
    if (e$check_answers) {
      result <- try(evaluate_submission(answer, e$ex[[e$ix]]), silent = TRUE)
      if (!isTRUE(result)) {
        translate_message("Try again! If you want to skip this exercise, type skip().")
        return(TRUE)
      } else {
        e$completed[e$ix] <- TRUE
        translate_message("Correct!")
        custom_message("")
      }
    }
    e$ix <- e$ix + 1L
    e$ask <- TRUE
  }
  return(NULL)
}

show_next <- function(e) {
  if (e$is_exam) {
    e$record <- TRUE
    e$current_start <- Sys.time()
    custom_message(
      l() %a% "Time remaining", ": ", e$hours, " ",
      l() %a% "hours and", " ", e$minutes, " ", l() %a% "minutes", "\n"
    )
    custom_message(l() %a% "QUESTION", " ", e$ix, " / ", e$n_ex)
    if (!e$mutable[e$ix]) {
      translate_message("NOTE: You cannot return to this question after submitting your answer or skipping it.")
    }
    if (e$ix > e$max_ix) {
      e$max_ix <- e$ix
    }
    if (!e$submitted[e$ix]) {
      e$warn_submit <- TRUE
    }
  } else {
    custom_message(l() %a% "SECTION", " ", e$part, ": ", l() %a% "EXERCISE", " ", e$ix, "/", e$n_ex)
  }
  custom_message(repchar("-", e$con_width))
  translate_message(e$ex[[e$ix]]$question)
  custom_message(repchar("-", e$con_width))
  dat <- e$ex[[e$ix]]$data
  if (!is.null(dat)) {
    dat_names <- names(dat)
    sapply(seq_along(dat_names), function(x) {
      assign(dat_names[x], e$data[[dat[[x]]]], envir = globalenv())
    })
  }
  e$ask <- FALSE
}

process_completion <- function(e) {
  e$ix <- e$n_ex
  if (e$is_exam) {
    if (!e$completed) {
      e$completed <- TRUE
      e$warn_complete <- FALSE
      if (identical(e$inst, "1")) {
        custom_message(l() %a% "You have completed part", " ", e$part, " ", l() %a% "of the exam!")
      } else {
        translate_message("You have completed the final exam!")
      }
      translate_message("You can still go back and change your answers by navigating with go().")
      translate_message("Otherwise, you can now finish the exam by typing exit().")
    }
    return(TRUE)
  }
  cleanup(e)
  return(FALSE)
}

# A cleanup procedure when listener is destroyed
cleanup <- function(e) {
  if (e$is_exam) {
    rm(list = ls(envir = globalenv()), envir = globalenv())
  } else {
    custom_message(l() %a% "You have quit section", " ", e$part, ".")
    if (!e$init && e$check_answers) {
      comp <- sum(e$completed)
      total <- e$n_ex
      if (comp < total) {
        custom_message(
          l() %a% "You completed", " ", sum(e$completed), " ",
          l() %a% "out of", " ", e$n_ex, " ", l() %a% "exercises", "."
        )
      } else {
        translate_message("You have completed all exercises of this section!")
      }
    }
  }
  options(prompt = "> ")
}

# Saves the exam results as .RData for external verification
verification <- function(e, verbose = FALSE) {
  if (verbose) {
    translate_message("Compiling the verification file, please wait...")
    flush.console()
  }
  out <- list()
  keep_fields <- c(
    "num",
    "time_start",
    "answers",
    "times",
    "ex",
    "data",
    "code",
    "select",
    "inst",
    "part",
    "seed",
    "id"
  )
  # Note, e is an environment, so we cannot subset with e[keep_fields] directly
  for (field in keep_fields) {
    out[[field]] <- e[[field]]
  }
  out$version <- packageVersion("Rcourse")
  out_file <- paste0(
    getwd(), "/verification_", out$part, "_", out$num, "_", out$seed, ".rds",
    collapse = ""
  )
  saveRDS(object = out, file = out_file, compress = "xz")
  if (verbose) {
    translate_message("Verification file was successfully saved in the working directory!")
    message("*** ", out_file, " ***")
  }
}
