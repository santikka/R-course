# Creates exercise objects from argument lists, replicating arguments of length one when applicable
compile <- function(questions, solutions, alts, data, code, params) {
    q <- length(questions)
    exercises <- vector(mode = "list", length = q)
    if (missing(alts)) {
        alts <- replicate(q, list())
    }
    if (missing(data)) {
        data <- replicate(q, NULL)
    } else if (length(data) == 1) {
        data <- replicate(q, data)
    }
    if (missing(code)) {
        code <- replicate(q, NULL)
    }
    if (missing(params)) {
        params <- replicate(q, list())
    } else if (length(params) == 1) {
        params <- replicate(q, list(params))
    }
    for (i in 1:q) {
        exercises[[i]] <- exercise(questions[[i]],
                                   solutions[[i]],
                                   alts[[i]],
                                   data[[i]],
                                   code[[i]],
                                   params[[i]])
    }
    return(exercises)
}
