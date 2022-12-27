## Functions and data for localization

#' @include utilities.R

# An alias for the internal translation function
l <- function() {
  l_internal
}

# A function that translates strings in package messages
l_internal <- function(x) {
  if (identical(x, " ")) {
    return(x)
  }
  lang_obj <- get(use_language())
  lang_obj[[x]]
}

# Determines the current language, defaults to English if not set
use_language <- function() {
  lang <- getOption("Rcourse_language")
  langs <- c("english", "finnish")
  if (is.null(lang) || !(lang %in% langs)) {
    options(Rcourse_language = "english")
    "english"
  } else {
    lang
  }
}

## Function aliases (listener pseudo-functions)

#' @rdname exit
#' @export
lopeta <- exit

#' @rdname ask
#' @export
kysy <- ask

#' @rdname skip
#' @export
ohita <- skip

#' @rdname submit
#' @export
vastaa <- submit

#' @rdname solution
#' @export
ratkaisu <- solution

#' @rdname code
#' @export
koodi <- code

#' @rdname go
#' @export
mene <- go

## Function aliases (actual)

#' Instructions for using the section and exam functions
#' @name info
#' @usage info()
#' @export
info <- function() {
  if (!identical(use_language(), "english")) {
    return(invisible())
  } else {
    info_()
  }
}

#' @rdname info
#' @export
ohje <- function() {
  if (!identical(use_language(), "finnish")) {
    invisible()
  } else {
    info_()
  }
}

#' Set the language used in the section and exam functions.
#' If no language is specified, the language is switched between English and Finnish.
#' @name select_language
#' @usage select_language(language, save_selection)
#' @param language a character string describing the language to be used (supports "english" or "finnish")
#' @param save_selection a logical value that indicates whether the selection should be saved to .Rprofile
#' @export
select_language <- function(language = c("english", "finnish"), save_selection = FALSE) {
  if (!identical(use_language(), "english")) {
    invisible()
  } else {
    select_language_(language, save_selection)
  }
}

#' @rdname select_language
#' @export
valitse_kieli <- function(language = c("english", "finnish"), save_selection = FALSE) {
  if (!identical(use_language(), "finnish")) {
    invisible()
  } else {
    select_language_(language, save_selection)
  }
}

#' The final exam for the R-course
#' @name exam
#' @usage exam(dob, ...)
#' @param dob date of birth as a character string "dd/mm/yyyy".
#' @param ... additional parameters for debugging.
#' @export
exam <- function(dob, ...) {
  if (!identical(use_language(), "english")) {
    invisible()
  } else {
    exam_(dob, ...)
  }
}

#' @rdname exam
#' @export
loppukoe <- function(dob, ...) {
  if (!identical(use_language(), "finnish")) {
    invisible()
  } else {
    exam_(dob, ...)
  }
}

#' Select an exercise section
#' @name section
#' @usage section(x)
#' @param x an integer value between 1 and 11 to select the section to start.
#' @export
section <- function(x) {
  if (!identical(use_language(), "english")) {
    invisible()
  } else {
    section_(x)
  }
}

#' @rdname section
#' @export
osio <- function(x) {
  if (!identical(use_language(), "finnish")) {
    invisible()
  } else {
    section_(x)
  }
}
