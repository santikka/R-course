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
exercise <- function(question = NULL, solution = NULL,
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
                test_default <- length(tests[[i]]) == 0
                if (test_default) {
                    res <- compare(x(), obj$solution())
                } else {
                    formals(x) <- tests[[i]]
                    formals(obj$solution) <- tests[[i]]
                    invisible(capture.output(x_eval <- x()))
                    res <- compare(x_eval, obj$solution())
                    formals(x) <- x_oldform
                    formals(obj$solution) <- obj_oldform
                }
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
        if (compare(x, obj$solution)) {
            return(TRUE)
        }
        return(FALSE)
    }
}

# Compare objects for non-strict equality
compare <- function(a, b) {
    base_attrs <- c("class", "dim", "names", "row.names")
    if (is.atomic(b)) {
        names(a) <- NULL
        names(b) <- NULL
    }
    if (is.data.frame(a) && is.data.frame(b)) {
        ia <- sapply(a, is.factor)
        if (any(ia)) {
            a[, ia] <- lapply(a[, ia, drop = FALSE], as.character)
        }
        ib <- sapply(b, is.factor)
        if (any(ib)) {
            b[, ib] <- lapply(b[, ib, drop = FALSE], as.character)
        }
    }
    for (att in names(attributes(a))) {
        if (!att %in% base_attrs) {
            attr(a, att) <- NULL
        }
    }
    for (att in names(attributes(b))) {
        if (!att %in% base_attrs) {
            attr(b, att) <- NULL
        }
    }
    same <- try_true(isTRUE(all.equal(a, b, tolerance = 0.01, check.attributes = TRUE)))
    if (!same) {
        if (identical(class(a), class(b))) {
            return(FALSE)
        }
        if (is.atomic(b) && is.vector(b)) {
            a_coerced <- try({
                suppressWarnings(do.call(what = paste0("as.", class(b)[1]), args = list(a)))
            }, silent = TRUE)
            if (!"try_error" %in% class(a_coerced)) {
                return(compare(a_coerced, b))
            } else {
                return(FALSE)
            }
        } else {
            return(FALSE)
        }
    }
    TRUE
}
