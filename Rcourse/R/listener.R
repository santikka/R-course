## A listener that uses the callback functionality to provide an interactive interface
listen <- function(...) UseMethod("listen")

listen.default <- function(e) {
    # Initialize the current section, verify the working directory and construct the exercises
    con_width <- 0.9 * getOption("width")
    if (e$init) {
        if (detect("exit", e$expr)) {
            return(FALSE)
        }
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
                options(prompt = paste0(l() %a% "section", e$part, "> "))
                exercises_[[e$part]](e)
            }
            e$n_ex <- length(e$ex)
            if (e$is_exam) {
                e$visited <- logical(e$n_ex)
                e$questions <- sapply(e$ex, "[[", "question")
                e$answers <- rep(list("This question was skipped by the student"), e$n_ex)
                e$submitted <- logical(e$n_ex)
                e$mutable <- sapply(e$ex, function(x) x$params$mutable)
                e$times <- integer(e$n_ex)
                e$current_start <- Sys.time()
            } else {
                e$completed <- logical(e$n_ex)
            }
            e$init <- FALSE
            e$ask <- TRUE
        }
    }
    # Update the remaining exam time
    if (e$is_exam) {
        time_remaining <- as.integer(difftime(e$time_start, Sys.time(), units = "mins")) + e$time_max
        if (time_remaining <= 0) {
            translate_message("The exam time has expired!")
            verification(e)
            cleanup(e)
            return(FALSE)
        }
        e$modulus <- time_remaining %% 60
        e$hours <- (time_remaining - e$modulus) / 60
        e$minutes <- floor(e$modulus)
    }
    # Go to a specific question
    if (detect("go", e$expr)) {
        if (is.null(e$val)) {
            if (e$is_exam) {
                translate_message("Please input the question number to go to.")
            } else {
                translate_message("Please input the exercise number to go to.")
            }
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
                if (!e$mutable[e$val]) {
                    custom_message(l() %a% "You cannot return to question number", " ", e$val, ".")
                    return(TRUE)
                }
            }
            e$ix <- e$val
            e$ask <- TRUE
        } else {
            custom_message(l() %a% "Please input a number between", " ", 1, " ", l() %a% "and", " ", e$n_ex)
            return(TRUE)
        }
    }
    # Skip the current problem
    if (detect("skip", e$expr)) {
        if (e$is_exam) {
            if (!e$submitted[e$ix] && e$warn_submit) {
                translate_message("You have not submitted an answer for this problem, are you sure you want to skip? (type skip() again to confirm)")
                e$warn_submit <- FALSE
                return(TRUE)
            }
            e$times[e$ix] <- as.integer(difftime(Sys.time(), e$current_start, units = "secs"))
            e$visited[e$ix] <- TRUE
            e$ix <- e$ix + 1
            e$ask <- TRUE
            while (!e$mutable[e$ix] && e$ix <= e$n_ex && e$visited[e$ix]) {
                e$ix <- e$ix + 1
            }
        } else {
            e$ix <- e$ix + 1
            e$ask <- TRUE
        }
    }
    # Check the user's submitted answer and react accordingly
    if (detect("submit", e$expr)) {
        result <- NULL
        if (e$is_exam) {
            e$times[e$ix] <- as.integer(difftime(Sys.time(), e$current_start, units = "secs"))
            e$visited[e$ix] <- TRUE
            e$submitted[e$ix] <- TRUE
            if (!is.null(e$val)) {
                e$answers[[e$ix]] <- e$val
            }
            e$ix <- e$ix + 1
            e$ask <- TRUE
            while (!e$mutable[e$ix] && e$ix <= e$n_ex && e$visited[e$ix]) {
                e$ix <- e$ix + 1
            }
        } else {
            if (e$check_answers) {
                result <- try(evaluate_submission(e$ex[[e$ix]], e$val), silent = TRUE)
                if (!isTRUE(result)) {
                    translate_message("Try again! If you want to skip this exercise, type skip().")
                    return(TRUE)
                } else {
                    e$completed[e$ix] <- TRUE
                    translate_message("Correct!")
                    custom_message("")
                }
            }
            e$ix <- e$ix + 1
            e$ask <- TRUE
        }
    }
    # Show the exercise description again
    if (detect("ask", e$expr)) {
        custom_message(repchar("-", con_width))
        translate_message(e$ex[[e$ix]]$question)
        custom_message(repchar("-", con_width))
        return(TRUE)
    }
    # Show the correct solution (the final object/function body/plot)
    if (detect("solution", e$expr)) {
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
    # Show the code used to produce the correct solution
    if (detect("code", e$expr)) {
        if (e$is_exam) {
            translate_message("Code cannot be shown for the exam.")
        } else {
            cat(e$ex[[e$ix]]$code, "\n")
        }
    }
    # Exit the current section/exam
    if (detect("exit", e$expr)) {
        if (e$is_exam) {
            if (!e$completed && e$warn_complete) {
                translate_message("You have not yet submitted answers to every exam question.")
                translate_message("If you exit now, your results will be saved, but you cannot continue the exam afterwards.")
                translate_message("Are you sure you want to exit? (type exit() again to confirm).")
                e$warn_complete <- FALSE
                return(TRUE)
            }
            if (!e$test_mode) {
                verification(e)
            }
        }
        cleanup(e)
        return(FALSE)
    }
    # A (correct) answer was submitted, present the next exercise
    if (e$ask && e$ix <= e$n_ex) {
        if (e$is_exam) {
            e$current_start <- Sys.time()
            custom_message(l() %a% "Time remaining", ": ", e$hours, " ", 
                           l() %a% "hours and", " ", e$minutes, " ", l() %a% "minutes", "\n")
            custom_message(l() %a% "QUESTION", " ", e$ix, " / ", e$n_ex)
            if (!e$mutable[e$ix]) {
                translate_message("NOTE: You cannot return to this question after submitting your answer.")
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
        custom_message(repchar("-", con_width))
        translate_message(e$ex[[e$ix]]$question)
        custom_message(repchar("-", con_width))
        dat <- e$ex[[e$ix]]$data
        if (!is.null(dat)) {
            dat_names <- names(dat)
            sapply(1:length(dat_names), function(x) {
                assign(dat_names[x], e$data[[dat[[x]]]], envir = globalenv())
            })
        }
        e$ask <- FALSE
        return(TRUE)
    }
    # Last problem is completed
    if (e$ix > e$n_ex) {
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
                return(TRUE)
            }
        } else {
            cleanup(e)
            return(FALSE)
        }
    }
    return(TRUE)
}
