## Functions for navigation

#' Exit the current section/exam
#' @name exit
#' @usage exit()
#' @export
exit <- function() invisible()

#' Repeat the current problem formulation
#' @name ask
#' @usage ask()
#' @export
ask <- function() invisible()

#' Skip the current problem
#' @name skip
#' @usage skip()
#' @export
skip <- function() invisible()

#' Submit an answer to be checked
#' @name submit
#' @usage submit(x)
#' @param x an R object to be submitted as the answer
#' @export
submit <- function(x) {
    if (missing(x)) {
        invisible(NULL) 
    } else {
        invisible(deparse(substitute(x)))
    }
}

#' Show the solution to the current problem
#' @name solution
#' @usage solution()
#' @export
solution <- function() invisible()

#' Show the code for the solution to the current problem
#' @name code
#' @usage code()
#' @export
code <- function() invisible()

#' Navigate to specific problem
#' @name go
#' @usage go(x)
#' @param x the exercise number to go to
#' @export
go <- function(x) if (missing(x)) invisible(NULL) else invisible(x)

## Internal functions

# Internal info function
info_ <- function() {
    translate_message("To start the exercises, type section(x), where 'x' is a number between 1 and 11 e.g., section(1) starts the first exercise section.")
    translate_message("To start the final exam, type exam(x), where 'x' is your date of birth in the format 'dd/mm/yyyy'.")
    info_rows <- list()
    info_rows$text <- c(
        paste0(l() %a% "info", "() : "),
        paste0(l() %a% "section", "(x) : "),
        paste0(l() %a% "exam", "(x) : "),
        paste0(l() %a% "submit", "(x) : "),
        paste0(l() %a% "skip", "() : "),
        paste0(l() %a% "exit", "() : "),
        paste0(l() %a% "solution", "() : "),
        paste0(l() %a% "code", "() : "),
        paste0(l() %a% "go", "(x) : "),
        paste0(l() %a% "ask", "() : "),
        paste0(l() %a% "select_language", "(x) : ")
    )
    nr <- length(info_rows$text)
    info_rows$len <- sapply(info_rows$text, nchar)
    instr <- c(
        "show these instructions.",
        "start exercise section number 'x'.",
        "start the final exam ('x' is your date of birth).",
        "submit 'x' as the answer to the current exercise.",
        "skip the current exercise.",
        "quit the current section/exam.",
        "show an example solution for the current exercise.",
        "show the code for the example solution.",
        "go to exercise number 'x'.",
        "show the problem description for the current exercise."
    )
    translate_message("The following special functions are available when using the package:")
    for (i in 1:(nr - 1)) {
        custom_message(" -- ", info_rows$text[i],  repchar("-", info_rows$len[nr] - info_rows$len[i]  - 3), " : ", l() %a% instr[i])
    }
    custom_message(" -- ", info_rows$text[nr], l() %a% "change the language of the package to 'x'", ",") 
    custom_message(l() %a% "currently supports 'english' and 'finnish'.", indent = info_rows$len[nr] + 3)
    invisible()
}

# Internal language selection function
select_language_ <- function(language = c("english", "finnish"), save_selection = FALSE) {
    if (length(getTaskCallbackNames())) {
        translate_message("Please complete the current section or exam before changing the language.")
        return(invisible(NULL))
    }
    lang_opt <- getOption("Rcourse_language")
    language <- try(match.arg(language), silent = TRUE)
    if ("try-error" %in% class(language)) {
        translate_message("Invalid language selected: switching language.")
        if (is.null(lang_opt)) {
            language <- "finnish"
        } else {
            if (identical(lang_opt, "english")) {
                language <- "finnish"
            }
            if (identical(lang_opt, "finnish")) {
                language <- "english"
            }
        }
    }
    options(Rcourse_language = language)
    custom_message(l() %a% "Language selected:", " ", l() %a% language)
    if (save_selection) {
        writable <- TRUE
        if (!file.exists(file.path("~", ".Rprofile"))) {
            if (file.access(file.path("~"), mode = 2)) {
                file.create(file.path("~", ".Rprofile"))
            } else {
                translate_message("Unable to save language selection due to insufficient access priviledges.")
                writable <- FALSE
            }
            rprofile <- character(0)
        } else {
            rprofile <- readLines(file.path("~", ".Rprofile"))
            if (!file.access(file.path("~"), mode = 2)) {
                translate_message("Unable to save language selection due to insufficient access priviledges.")
                writable <- FALSE
            }
        }
        if (writable) {
            opt_rcourse <- grepl("options\\(Rcourse_language = .*", rprofile)
            opts <- paste0("options(Rcourse_language = '", language, "')")
            if (any(opt_rcourse)) {
                rprofile[which(opt_rcourse)[1]] <- opts
            } else {
                rprofile <- c(rprofile, opts)
            }
            writeLines(rprofile, con = file.path("~", ".Rprofile"))
        }
    }
    translate_message("Type info() to begin.")
}

# 'apply' f as an operator, if f(y) is NULL, returns just y
"%a%" <- function(f, y) {
    result <- f(y)
    if (is.null(result)){
        y
    } else {
        result
    }
}

# Check whether the user tried to use the specific function named 'what' in 'expr'
detect <- function(what, expr) {
    if (leaf(expr)) {
        return(FALSE)
    }
    flat_expr <- flatten(expr)
    whats <- sapply(what, function(x) l() %a% x)
    whats %in% flat_expr[1]
}

# Flatten potentially complicated expression 'expr' for checking
flatten <- function(expr) {
    if (leaf(expr)) {
        expr
    } else {
        unlist(lapply(expr, flatten))
    }
}

# Essentially confirm whether 'x' is a name
leaf <- function(x) !(is.call(x) || is.expression(x))

# A wrapper to send instructive messages to the user
custom_message <- function(..., skip_pre = FALSE, skip_post = TRUE, width = NULL, indent = 0) {
    if (is.null(width)) {
        width <- 0.9 * getOption("width")
    }
    m <- sapply(strwrap(paste0(...), width = width, indent, simplify = FALSE), paste, collapse = "\n")
    if (skip_pre) {
        m <- paste0("\n", m)
    }
    if (skip_post) {
        m <- paste0(m, "\n")
    }
    Encoding(m) <- "UTF-8"
    message(m, appendLF = FALSE)
}

# A wrapper for translating custom messages automatically
translate_message <- function(...) {
    m <- list(...)
    m_translate <- lapply(m, function(y) l() %a% y)
    do.call(custom_message, args = m_translate)
}

# Translate 'submit' in code() output
translate_code <- function(x) {
    gsub(pattern = "submit", replacement = l() %a% "submit", x)
}

# Saves the exam results as .RData for external verification
verification <- function(e) {
    translate_message("Compiling the verification file, please wait...")
    flush.console()
    out <- list()
    keep_fields <- c(
        "num",
        "time_start",
        "answers",
        "times",
        "ex",
        "data",
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
    out_file <- paste0(getwd(), "/verification_", out$part, "_", out$num, "_", 
                       out$seed, ".RData", collapse = "")
    save(out, file = out_file)
    translate_message("Verification file was successfully saved in the working directory!")
    message("*** ", out_file, " ***")
}

# RNG initialization for the final exam
# Set all PRNG kinds manually to for backward (pre R-3.6.0) and forward compatibility
initialize <- function(seed) {
    set.seed(seed = seed,
             kind = "Mersenne-Twister",
             normal.kind = "Inversion",
             sample.kind = "Rejection")
}

# Compute sample central moments of 'x' up to order 'k' as a vector
cent_moments <- function(x, k) {
    n <- length(x)
    xm <- mean(x)
    m <- numeric(k)
    for (i in 1:k) {
        m[i] <- sum((x - xm)^i)
    }
    m / n
}

# Replicate a list as a list 'nrep' times
replist <- function(..., nrep = 1) {
    rep(list(list(...)), nrep)
}

# Create a string with a single character 'x' replicated 'nrep' times
repchar <- function(x, nrep) {
    paste0(rep(x, nrep), collapse = "")
}

# A cleanup procedure when listener is destroyed
cleanup <- function(e) {
    if (e$is_exam) {
        rm(list = ls(envir = globalenv()), envir = globalenv())
    } else {
        custom_message(l() %a% "You have quit section", " ", e$part, ".")
        if (e$check_answers) {
            comp <- sum(e$completed)
            total <- e$n_ex
            if (comp < total) {
                custom_message(l() %a% "You completed", " ", sum(e$completed), " ", 
                               l() %a% "out of", " ", e$n_ex, " ", l() %a% "exercises", ".")
            } else {
                translate_message("You have completed all exercises of this section!")
            }
        }
    }
    options(prompt = "> ")
}

# Convert a list to string element-wise (keeps function formatting)
list_to_string <- function(x) {
    sapply(x, function(y) {
        if (is.function(y)) {
            deparse1(y, collapse = "\n")
        } else {
            as.character(y)
        }
    })
}

# Tries to evaluate a logical expression 'x' and return FALSE if evaluation fails
try_true <- function(x) {
    y <- try(x, silent = TRUE)
    if ("try-error" %in% class(y)) {
        return(FALSE)
    }
    return(y)
}

# Compare objects for non-strict equality
compare <- function(a, b) {
    base_attrs <- c("class", "dim", "names")
    if (is.atomic(b)) {
        names(a) <- NULL
        names(b) <- NULL
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
        a_coerce <- try({
            suppressWarnings(do.call(what = paste0("as.", class(b)[1]), args = list(a)))
        }, silent = TRUE)
        if (!"try_error" %in% class(a_coerce)) {
            same <- try_true(isTRUE(all.equal(a_coerce, b, tolerance = 0.01, check.attributes = TRUE)))
        }
    }
    return(same)
}
