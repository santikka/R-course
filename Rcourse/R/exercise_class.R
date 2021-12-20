# Implements a class for constructing exercises
# @param question A character string describing the problem formulation.
# @param solution An R object representing the correct solution (or an R expression for plotting problems).
# @param alts An optional list of R objects that are also accepted as solutions.
# @param data A list or R objects provided to the student to solve this problem.
# @param code A character string representation of the R code that produces the correct solution.
# @param params A list of parameters that control the correctness of submitted solutions
#  -- is_function Is the correct answer an R function?
#  -- test_input Test input to check that the a function submitted by the student produces the same output as solution
#  -- graphical Does the problem require producing a plot?
#  -- mutable Can the student return to this problem after submitting an answer (exams only)?
exercise <- function(question = NULL, solution = NULL, alts = list(), 
                     data = NULL, code = NULL, params = list()) 
{
    if (is.null(params$is_function)) {
        params$is_function <- FALSE
    }
    if (is.null(params$graphical)) {
        params$graphical <- FALSE
    }
    if (is.null(params$mutable)) {
        params$mutable <- TRUE
    }
    ex <- list(question = question,
               solution = solution,
               alts = alts,
               data = data,
               code = code,
               params = params)
    class(ex) <- "exercise"
    ex
}

# Verifies if 'x' is the correct answer for the exercise object 'obj'
evaluate_submission <- function(x, obj) {
    if (is.null(x)) {
        return(FALSE)
    }
    if (obj$params$is_function) {
        if (!is.function(x)) {
            return(FALSE)
        }
        tests <- obj$params$test_input
        result <- TRUE
        x_oldform <- formals(x)
        obj_oldform <- formals(obj$solution)
        for (i in 1:length(tests)) {
            if (!try_true({
                res <- TRUE
                formals(x) <- tests[[i]]
                formals(obj$solution) <- tests[[i]]
                if (!isTRUE(all.equal(obj$solution(), x(), tolerance = 0.01, check.attributes = TRUE))) {
                    res <- FALSE
                }
                formals(x) <- x_oldform
                formals(obj$solution) <- obj_oldform
                res
            })) {
                result <- FALSE
                break
            }
        }
        return(result)
    } else {
        if (is.function(x)) {
            return(FALSE)
        }
        all_solutions <- c(list(obj$solution), obj$alts)
        correct <- sapply(all_solutions, function(y) {
            try_true({
                isTRUE(all.equal(x, y, tolerance = 0.01, check.attributes = TRUE))
            })
        })
        if (any(correct)) {
            return(TRUE)
        }
        return(FALSE)
    }
}
