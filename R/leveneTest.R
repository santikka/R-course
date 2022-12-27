# Borrowed from car 27 March 2017

# moved from Rcmdr 13 July 2004

# levene.test.default function slightly modified and generalized from Brian Ripley via R-help
# the original generic version was contributed by Derek Ogle
# last modified 28 January 2010 by J. Fox

#' Computes Levene's test for homogeneity of variance across groups.
#'
#' @name leveneTest
#' @title Levene's Test
#' @usage leveneTest(y, ...)
#' @param y response variable for the default method, or a \code{lm} or
#'  \code{formula} object. If \code{y} is a linear-model object or a formula,
#'  the variables on the right-hand-side of the model must all be factors and
#'  must be completely crossed.
#' @param group factor defining groups.
#' @param center The name of a function to compute the center of each group;
#'  \code{mean} gives the original Levene's test; the default, \code{median},
#'  provides a more robust test.
#' @param data a data frame for evaluating the \code{formula}.
#' @param ... arguments to be passed down, e.g., \code{data} for the
#'  \code{formula} and \code{lm} methods; can also be used to pass arguments to
#'  the function given by \code{center} (e.g., \code{center=mean} and
#'  \code{trim=0.1} specify the 10\% trimmed mean).
#' @return returns an object meant to be printed showing the results of the test.
#' @references   Fox, J. (2016)
#'  \emph{Applied Regression Analysis and Generalized Linear Models},
#'  Third Edition. Sage.
#'
#'  Fox, J. and Weisberg, S. (2019)
#'  \emph{An R Companion to Applied Regression}, Third Edition, Sage.
#' @export
leveneTest <- function(y, ...) {
  UseMethod("leveneTest")
}

#' @rdname leveneTest
#' @importFrom stats lm
#' @export
leveneTest.default <- function(y, group, center = median, ...) { # original levene.test
  if (!is.numeric(y)) {
    stop(deparse(substitute(y)), " is not a numeric variable")
  }
  if (!is.factor(group)) {
    warning(deparse(substitute(group)), " coerced to factor.")
    group <- as.factor(group)
  }
  valid <- complete.cases(y, group)
  meds <- tapply(y[valid], group[valid], center, ...)
  resp <- abs(y - meds[group])
  table <- anova(lm(resp ~ group))[, c(1, 4, 5)]
  rownames(table)[2] <- " "
  dots <- deparse(substitute(...))
  attr(table, "heading") <- paste("Levene's Test for Homogeneity of Variance (center = ",
    deparse(substitute(center)), if (!(dots == "NULL")) paste(":", dots), ")",
    sep = ""
  )
  table
}

#' @rdname leveneTest
#' @export
leveneTest.formula <- function(y, data, ...) {
  form <- y
  mf <- if (missing(data)) model.frame(form) else model.frame(form, data)
  if (any(sapply(2:dim(mf)[2], function(j) is.numeric(mf[[j]])))) {
    stop("Levene's test is not appropriate with quantitative explanatory variables.")
  }
  y <- mf[, 1]
  if (dim(mf)[2] == 2) {
    group <- mf[, 2]
  } else {
    if (length(grep("\\+ | \\| | \\^ | \\:", form)) > 0) stop("Model must be completely crossed formula only.")
    group <- interaction(mf[, 2:dim(mf)[2]])
  }
  leveneTest.default(y = y, group = group, ...)
}

#' @rdname leveneTest
#' @importFrom stats formula
#' @export
leveneTest.lm <- function(y, ...) {
  leveneTest.formula(formula(y), data = model.frame(y), ...)
}
