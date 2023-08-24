## Exams

# Internal exam function
exam_ <- function(dob, ...) {
  if ("listener" %in% getTaskCallbackNames()) {
    translate_message("Please exit or finish the current section/exam before starting a new one.")
    return(invisible())
  }
  # Verify DOB
  if (missing(dob)) {
    translate_message("Please input your date of birth as a string of the form 'dd/mm/yyyy'.")
    return(invisible())
  } else {
    # Check that 'dob' is actually a date in the required format
    if (!grepl("^(3[01]|[12][0-9]|0[1-9])/(1[0-2]|0[1-9])/[0-9]{4}$", dob)) {
      translate_message("Please input your date of birth as a string of the form 'dd/mm/yyyy'.")
      return(invisible())
    }
  }
  dots <- list(...)
  con_width <- 0.9 * getOption("width")
  e <- new.env(globalenv())
  class(e) <- c("environment", "default")
  num <- as.numeric(gsub("/", "", dob))
  part <- NULL
  inst <- NULL
  id <- ""
  e$ix <- 0L
  e$max_ix <- 1L
  e$init <- TRUE
  e$ask <- FALSE
  if (is.null(dots$test_inst)) {
    translate_message("Please select your institution.")
    translate_message("If you're attending University of Jyväskylä (JYU), type 1.")
    translate_message("If you're attending University of Eastern Finland (UEF), type 2.")
    translate_message("To quit, type q.")
    inst <- readline(prompt = "exam> ")
    while (!inst %in% c("1", "2", "q")) {
      translate_message("Please type either 1, 2, or q.")
      inst <- readline(prompt = "exam> ")
    }
  } else {
    if (dots$test_inst %in% c("1", "2")) {
      inst <- dots$test_inst
    } else {
      stop("Invalid `test_inst` value!")
    }
  }
  # Specific functionality based on the university
  if (identical(inst, "1")) {
    # JYU
    translate_message("Welcome to the final exam of the R-course!")
    custom_message(repchar("-", con_width))
    translate_message(
      "The exam consists of two parts.", " ",
      "To obtain one credit, it is sufficient to complete only the first part.", " ",
      "For two credits, you must complete both parts.", " ",
      "There are 24 questions in the first part, and 12 questions in the second part.", " ",
      "The duration of the exam is 4 hours for each part (8 hours in total)."
    )
    custom_message("")
    translate_message(
      "The first part of the exam involves the PISA data, so make sure you have downloaded and extracted the required datasets (datasets.zip) from the course webpage before starting.", " ",
      "Some questions will ask you to modify the PISA data. Make sure to always use the latest version of the data when answering the following questions that involve it."
    )
    custom_message("")
    translate_message(
      "Unlike in the exercises, the program will not tell you whether your answer was correct or not after each question.", " ",
      "The program may sometimes add objects to your working environment that are needed to answer some of the questions."
    )
    custom_message("")
    translate_message(
      "Some questions of the exam are dependent on one another, for example those that deal with the PISA-data.", " ",
      "In such instances, the correct answer of a previous question is required to answer the followup questions.", " ",
      "The correct data from the previous question is always created for you in your working environment, so you can attempt to answer the followup question even if you did not manage to answer the previous question."
    )
    custom_message("")
    translate_message(
      "You can return to previous questions using go(), but this is possible only for some questions.", " ",
      "The program will always inform you if a specific question cannot be returned to afterwards."
    )
    custom_message("")
    translate_message("When the exam ends (either by exit(), after having answered all questions, or if the time runs out) a verification file is created in your working directory which you will need to submit to the course webpage for grading.")
    custom_message(repchar("-", con_width))
    if (is.null(dots$test_part)) {
      translate_message("Please select which part of the exam you want to complete.")
      translate_message("If you want to complete part one, type 1.")
      translate_message("If you want to complete part two, type 2.")
      translate_message("To quit, type q.")
      part <- readline(prompt = "exam> ")
      while (!part %in% c("1", "2", "q")) {
        translate_message("Please type either 1, 2, or q.")
        part <- readline(prompt = "exam> ")
      }
      if (identical(part, "q")) {
        translate_message("The exam was cancelled.")
        return(invisible())
      }
    } else {
      if (dots$test_part %in% c("1", "2")) {
        part <- dots$test_part
      } else {
        stop("Invalid `test_part` value!")
      }
    }
  } else if (identical(inst, "2")) {
    # UEF
    if (is.null(dots$test_id)) {
      translate_message("Please input your Peppi student number.")
      translate_message("To quit, type q.")
      id <- readline(prompt = "exam> ")
      while (!grepl("^[0-9]{7,8}$|q$", id)) {
        translate_message("Invalid student number.", " ", "Please input your Peppi student number.")
        translate_message("To quit, type q.")
        id <- readline(prompt = "exam> ")
      }
      if (identical(id, "q")) {
        translate_message("The exam was cancelled.")
        return(invisible())
      }
    } else {
      id <- dots$test_id
    }
    translate_message("Welcome to the final exam of the R-course!")
    custom_message(repchar("-", con_width))
    translate_message(
      "There are 30 questions in the exam.", " ",
      "The duration of the exam is 4 hours."
    )
    custom_message("")
    translate_message(
      "Many questions of the exam are related to the PISA data, so make sure you have downloaded and exctracted the required datasets (datasets.zip) from the course webpage before starting.", " ",
      "Some questions will ask you to modify the PISA data. Make sure to always use the latest version of the data when answering the following questions that involve it."
    )
    custom_message("")
    translate_message(
      "Unlike in the exercises, the program will not tell you whether your answer was correct or not after each question.", " ",
      "The program may sometimes add objects to your working environment that are needed to answer some of the questions."
    )
    custom_message("")
    translate_message(
      "Some questions of the exam are dependent on one another, for example those that deal with the PISA-data.", " ",
      "In such instances, the correct answer of a previous question is required to answer the followup questions.", " ",
      "The correct data from the previous question is always created for you in your working environment, so you can attempt to answer the followup question even if you did not manage to answer the previous question."
    )
    custom_message("")
    translate_message(
      "You can return to previous questions using go(), but this is possible only for some questions.", " ",
      "The program will always inform you if a specific question cannot be returned to afterwards."
    )
    custom_message("")
    translate_message("When the exam ends (either by exit(), after having answered all questions, or if the time runs out) a verification file is created in your working directory which you will need to submit to the course webpage for grading.")
    custom_message(repchar("-", con_width))
    if (is.null(dots$test_part)) {
      translate_message("Do you want to start the exam? (y/n)")
      part <- readline(prompt = "exam> ")
      while (!part %in% c("y", "n")) {
        translate_message("Please type either y or n.")
        part <- readline(prompt = "exam> ")
      }
      if (identical(part, "n")) {
        translate_message("The exam was cancelled.")
        return(invisible())
      }
      part <- "1"
    } else {
      if (identical(dots$test_part, "1")) {
        part <- dots$test_part
      } else {
        stop("Invalid `test_part` value!")
      }
    }
  } else {
    translate_message("The exam was cancelled.")
    options(prompt = "> ")
    return(invisible())
  }
  # Initialize the exam environment
  e$is_exam <- TRUE
  e$record <- FALSE
  e$incoming <- FALSE
  e$check_answers <- TRUE
  e$require_wd <- FALSE
  e$test_mode <- if (is.null(dots$test_mode)) FALSE else dots$test_mode
  e$part <- as.integer(part)
  e$inst <- inst
  e$id <- id
  e$num <- num
  e$completed <- FALSE
  e$warn_complete <- TRUE
  e$warn_submit <- TRUE
  e$time_start <- Sys.time()
  e$time_max <- 4 * 60
  e$seed <- num + as.integer(e$time_start) %% 1e7
  # Set PRNG seed
  if (is.null(dots$override)) {
    initialize(e$seed)
  } else {
    initialize(dots$override)
  }
  cb <- function(expr, val, ok, vis, data = e) {
    e$expr <- expr
    e$val <- val
    e$ok <- ok
    e$vis <- vis
    return(listen(e))
  }
  addTaskCallback(cb, name = "listener")
  invisible()
}

# A function that performs the random selection of exam problems from the candidates
randomize_selection <- function(e, eq, continue = character(0L), data, params) {
  e$ex <- list()
  e$select <- list()
  en <- names(eq)
  n_eq <- length(eq)
  ix <- 0L
  sel <- c()
  sel_prev <- c()
  if (missing(data)) {
    data <- replicate(n_eq, NULL)
  }
  if (missing(params)) {
    params <- replicate(n_eq, NULL)
  }
  for (i in seq_along(eq)) {
    j <- as.numeric(strsplit(en[i], ",")[[1L]])
    if (en[i] %in% continue) {
      sel <- sel_prev
    } else {
      sel <- sample.int(length(eq[[i]]), length(j))
    }
    sel_prev <- sel
    for (k in sel) {
      ix <- ix + 1L
      e$ex[[ix]] <- exercise(
        question = eq[[i]][[k]],
        data = if (is.null(data[[i]])) NULL else data[[i]][[k]],
        params = if (is.null(params[[i]])) list() else params[[i]][[k]]
      )
      e$select[[ix]] <- c(i = i, k = k)
    }
  }
}

exams_ <- list()
exams_$JYU <- list()

exams_$JYU[[1L]] <- function(e) {
  define_data <- function() {
    ind <- sample.int(nrow(pisa), 4300, replace = FALSE)
    ST <- paste("ST27Q", sprintf("%02d", sample.int(12, 2)), sep = "")
    mut <- c(
      "ST04Q01", "age", "urban", "region", "readscore",
      "mathscore", "sciescore", "natlang", "math", ST
    )
    pisa_1 <- pisa
    pisa_2 <- pisa_1[ind, ]
    pisa_3 <- pisa_2[, mut]
    pisa_4 <- pisa_3[complete.cases(pisa_3), ]
    ind2 <- sample.int(nrow(pisa_4), 100)
    pisa_5 <- pisa_4
    pisa_5$fgender <- factor(pisa_5$ST04Q01, labels = c("female", "male"))
    pisa_5$furban <- factor(pisa_5$urban, labels = c("city", "countryside"))
    fregion <- factor(pisa_5$region, levels = 1:5)
    levels(fregion) <- c(
      "Southern Finland", "Western Finland", "Eastern Finland",
      "Northern Finland", "\u00C5land"
    )
    pisa_5$fregion <- fregion
    tab_1 <- table(pisa_5[, ST[1L]], pisa_5[, ST[2L]])
    tab_2 <- table(pisa_5$ST04Q01, pisa_5$urban)
    list(
      ind = ind,
      ST = ST,
      mut = mut,
      pisa_1 = pisa_1,
      pisa_2 = pisa_2,
      pisa_3 = pisa_3,
      pisa_4 = pisa_4,
      pisa_5 = pisa_5,
      ind2 = ind2,
      tab_1 = tab_1,
      tab_2 = tab_2
    )
  }
  e$data <- define_data()
  data <- list(
    `1` = NULL,
    `2` = replist(ind = "ind", pisa = "pisa_1"),
    `3` = replist(mut = "mut", pisa = "pisa_2"),
    `4` = replist(pisa = "pisa_3"),
    `5` = NULL,
    `6` = list(
      list(ind2 = "ind2", pisa = "pisa_4"),
      list(pisa = "pisa_4"),
      list(pisa = "pisa_4")
    ),
    `7,8,9` = replist(pisa = "pisa_4", nrep = 6),
    `10` = replist(pisa = "pisa_4", nrep = 3),
    `11` = replist(pisa = "pisa_4", nrep = 3),
    `12` = replist(pisa = "pisa_4"),
    `13` = replist(pisa = "pisa_5", nrep = 4),
    `14` = replist(pisa = "pisa_5", nrep = 3),
    `15` = replist(pisa = "pisa_5", nrep = 4),
    `16` = NULL,
    `17` = replist(pisa = "pisa_5", nrep = 2),
    `18` = replist(pisa = "pisa_5", nrep = 3),
    `19` = replist(pisa = "pisa_5", nrep = 3),
    `20` = list(
      list(ST = "ST", pisa = "pisa_5"),
      list(pisa = "pisa_5")
    ),
    `21` = list(
      list(tab = "tab_1"),
      list(tab = "tab_2")
    ),
    `22` = list(
      list(tab = "tab_1"),
      list(tab = "tab_2")
    ),
    `23` = replist(pisa = "pisa_5", nrep = 2),
    `24` = replist(pisa = "pisa_5", nrep = 4)
  )
  params <- list(
    `1` = replist(mutable = FALSE),
    `2` = replist(mutable = FALSE),
    `3` = replist(mutable = FALSE),
    `4` = replist(mutable = FALSE),
    `5` = NULL,
    `6` = NULL,
    `7,8,9` = NULL,
    `10` = NULL,
    `11` = NULL,
    `12` = replist(mutable = FALSE),
    `13` = NULL,
    `14` = NULL,
    `15` = NULL,
    `16` = NULL,
    `17` = NULL,
    `18` = NULL,
    `19` = NULL,
    `20` = replist(mutable = FALSE, nrep = 2),
    `21` = NULL,
    `22` = NULL,
    `23` = NULL,
    `24` = NULL
  )
  e$ex <- vector(mode = "list", length = 24L)
  e$select <- list()
  randomize_selection(
    e = e,
    eq = exam_questions_jyu[[1]],
    continue = c("22", "23"),
    data = data,
    params = params
  )
}

exams_$JYU[[2L]] <- function(e) {
  df_remove1 <- data.frame(c1 = runif(1000, -5, 5), c2 = runif(1000, 0, 2))
  df_remove2 <- data.frame(c1 = runif(1000, -5, 5), c2 = runif(1000, 0, 2))
  df_1 <- data.frame(abc = 1, yt = 1, vac = 1)
  df_2 <- data.frame(z = 1, g = 3, h = 1)
  df_3 <- data.frame(c = 1, b = 1, d = 1)
  x <- c(runif(100, -5, 5), 1, 1, -1, -1)
  x_2 <- rnorm(1000, 2, 0.5)
  x_3 <- rt(1000, 20)
  y <- c(runif(100, -5, 5), 1, -1, 1, -1)
  c_input <- lapply(1:100, function(i) list(x = x[i], y = y[i]))
  sigma <- sample(1:25, 1)
  mu <- runif(1, -100, 100)
  obs <- rnorm(100, 6, 4)
  params <- list(
    `1` = NULL,
    `2` = NULL,
    `3` = NULL,
    `4` = NULL,
    `5` = NULL,
    `6` = NULL,
    `7` = NULL,
    `8` = NULL,
    `9` = list(
      `1` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = df_remove1, k = 3),
          p2 = list(x = df_remove2, k = 7)
        )
      ),
      `2` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = df_1),
          p2 = list(x = df_2),
          p3 = list(x = df_3)
        )
      )
    ),
    `10` = list(
      `1` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(a = 1, b = -3),
          p2 = list(a = 0, b = 0),
          p3 = list(a = -2, b = 1000)
        )
      ),
      `2` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = x_2),
          p2 = list(x = x_3)
        )
      ),
      `3` = list(
        is_function = TRUE,
        test_input = c_input
      )
    ),
    `11` = list(
      `1` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = obs)
        )
      ),
      `2` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = obs)
        )
      ),
      `3` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = obs, mu = mu, sigma = sigma)
        )
      )
    ),
    `12` = list(
      `1` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = obs, k = 1),
          p2 = list(x = obs, k = 2),
          p3 = list(x = obs, k = 7)
        )
      ),
      `2` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = x_2),
          p2 = list(x = x_3)
        )
      )
    )
  )
  randomize_selection(
    e = e,
    eq = exam_questions_jyu[[2]],
    params = params
  )
}

exams_$UEF <- function(e) {
  define_data <- function() {
    ind <- sample.int(nrow(pisa), 4300, replace = FALSE)
    ST <- paste("ST27Q", sprintf("%02d", sample.int(12, 2)), sep = "")
    mut <- c(
      "ST04Q01", "age", "urban", "region", "readscore",
      "mathscore", "sciescore", "natlang", "math", ST
    )
    pisa_1 <- pisa
    pisa_2 <- pisa_1[ind, ]
    pisa_3 <- pisa_2[, mut]
    pisa_4 <- pisa_3[complete.cases(pisa_3), ]
    ind2 <- sample.int(nrow(pisa_4), 100)
    pisa_5 <- pisa_4
    pisa_5$fgender <- factor(pisa_5$ST04Q01, labels = c("female", "male"))
    pisa_5$furban <- factor(pisa_5$urban, labels = c("city", "countryside"))
    fregion <- factor(pisa_5$region, levels = 1:5)
    levels(fregion) <- c(
      "Southern Finland", "Western Finland", "Eastern Finland",
      "Northern Finland", "\u00C5land"
    )
    pisa_5$fregion <- fregion
    tab_1 <- table(pisa_5[, ST[1]], pisa_5[, ST[2]])
    tab_2 <- table(pisa_5$ST04Q01, pisa_5$urban)
    list(
      ind = ind,
      ST = ST,
      mut = mut,
      pisa_1 = pisa_1,
      pisa_2 = pisa_2,
      pisa_3 = pisa_3,
      pisa_4 = pisa_4,
      pisa_5 = pisa_5,
      ind2 = ind2,
      tab_1 = tab_1,
      tab_2 = tab_2
    )
  }
  e$data <- define_data()
  df_remove1 <- data.frame(c1 = runif(1000, -5, 5), c2 = runif(1000, 0, 2))
  df_remove2 <- data.frame(c1 = runif(1000, -5, 5), c2 = runif(1000, 0, 2))
  df_1 <- data.frame(abc = 1, yt = 1, vac = 1)
  df_2 <- data.frame(z = 1, g = 3, h = 1)
  df_3 <- data.frame(c = 1, b = 1, d = 1)
  x <- c(runif(100, -5, 5), 1, 1, -1, -1)
  x_2 <- rnorm(1000, 2, 0.5)
  x_3 <- rt(1000, 20)
  y <- c(runif(100, -5, 5), 1, -1, 1, -1)
  obs <- rnorm(100, 6, 4)
  mu <- runif(1, -100, 100)
  sigma <- sample(1:25, 1)
  data <- list(
    `1` = NULL,
    `2` = replist(ind = "ind", pisa = "pisa_1"),
    `3` = replist(mut = "mut", pisa = "pisa_2"),
    `4` = replist(pisa = "pisa_3"),
    `5` = NULL,
    `6` = list(
      list(ind2 = "ind2", pisa = "pisa_4"),
      list(pisa = "pisa_4"),
      list(pisa = "pisa_4")
    ),
    `7,8,9` = replist(pisa = "pisa_4", nrep = 6),
    `10` = replist(pisa = "pisa_4", nrep = 3),
    `11` = replist(pisa = "pisa_4", nrep = 3),
    `12` = replist(pisa = "pisa_4"),
    `13` = replist(pisa = "pisa_5", nrep = 4),
    `14` = replist(pisa = "pisa_5", nrep = 3),
    `15` = replist(pisa = "pisa_5", nrep = 4),
    `16` = NULL,
    `17` = replist(pisa = "pisa_5", nrep = 2),
    `18` = replist(pisa = "pisa_5", nrep = 3),
    `19` = replist(pisa = "pisa_5", nrep = 3),
    `20` = list(
      list(ST = "ST", pisa = "pisa_5"),
      list(pisa = "pisa_5")
    ),
    `21` = list(
      list(tab = "tab_1"),
      list(tab = "tab_2")
    ),
    `22` = list(
      list(tab = "tab_1"),
      list(tab = "tab_2")
    ),
    `23` = replist(pisa = "pisa_5", nrep = 2),
    `24` = replist(pisa = "pisa_5", nrep = 4),
    `25` = NULL,
    `26` = NULL,
    `27` = NULL,
    `28` = NULL,
    `29` = NULL,
    `30` = NULL
  )
  params <- list(
    `1` = replist(mutable = FALSE),
    `2` = replist(mutable = FALSE),
    `3` = replist(mutable = FALSE),
    `4` = replist(mutable = FALSE),
    `5` = NULL,
    `6` = NULL,
    `7,8,9` = NULL,
    `10` = NULL,
    `11` = NULL,
    `12` = replist(mutable = FALSE),
    `13` = NULL,
    `14` = NULL,
    `15` = NULL,
    `16` = NULL,
    `17` = NULL,
    `18` = NULL,
    `19` = NULL,
    `20` = replist(mutable = FALSE, nrep = 2),
    `21` = NULL,
    `22` = NULL,
    `23` = NULL,
    `24` = NULL,
    `25` = NULL,
    `26` = NULL,
    `27` = list(
      `1` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = df_remove1, k = 3),
          p2 = list(x = df_remove2, k = 7)
        )
      ),
      `2` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = df_1),
          p2 = list(x = df_2),
          p3 = list(x = df_3)
        )
      )
    ),
    `28` = list(
      `1` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = x_2),
          p2 = list(x = x_3)
        )
      ),
      `2` = list(
        is_function = TRUE,
        test_input = lapply(1:100, function(i) list(x = x[i], y = y[i]))
      ),
      `3` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = x_2),
          p2 = list(x = x_3)
        )
      )
    ),
    `29` = list(
      `1` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = obs)
        )
      ),
      `2` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = obs)
        )
      )
    ),
    `30` = list(
      `1` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = obs, mu = mu, sigma = sigma)
        )
      ),
      `2` = list(
        is_function = TRUE,
        test_input = list(
          p1 = list(x = obs, k = 1),
          p2 = list(x = obs, k = 2),
          p3 = list(x = obs, k = 7)
        )
      )
    )
  )
  randomize_selection(
    e = e,
    eq = exam_questions_uef,
    continue = c("22", "23"),
    data = data,
    params = params
  )
}
