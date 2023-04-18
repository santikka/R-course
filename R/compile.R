# Creates exercise objects from argument lists,
# replicating arguments of length one when applicable
compile <- function(questions, solutions, data, code, params) {
  q <- length(questions)
  exercises <- vector(mode = "list", length = q)
  if (missing(data)) {
    data <- replicate(q, NULL)
  } else if (length(data) == 1L) {
    data <- replicate(q, data)
  }
  if (missing(code)) {
    code <- replicate(q, NULL)
  }
  if (missing(params)) {
    params <- replicate(q, list())
  } else if (length(params) == 1L) {
    params <- replicate(q, list(params))
  }
  for (i in seq_len(q)) {
    exercises[[i]] <- exercise(
      question = questions[[i]],
      solution = solutions[[i]],
      data = data[[i]],
      code = code[[i]],
      params = params[[i]]
    )
  }
  exercises
}
