# Constructs a listener for a specific exercise section
# @param part number of the section
# @param require_wd does this section use the datasets?
# @param check_answers should submitted answers be checked?
create_section <- function(part, require_wd = FALSE, check_answers = TRUE, extra_info = list()) {
    force(part)
    force(require_wd)
    force(check_answers)
    function() {
        if ("listener" %in% getTaskCallbackNames()) {
            translate_message("Please exit or finish the current section/exam before starting a new one.")
            return(invisible())
        }
        e <- new.env(globalenv())
        class(e) <- c("environment", "default")
        e$is_exam <- FALSE
        e$part <- part
        e$check_answers <- check_answers
        e$require_wd <- require_wd
        e$test_mode <- FALSE
        e$ix <- 1
        e$init <- TRUE
        e$ask <- FALSE
        e$data <- list()
        custom_message(l() %a% "Welcome to section", " ", part, l() %a% " of the R-course!")
        if (check_answers) {
            translate_message("The program will automatically check your answers to each problem. If your answer is not correct, the program will ask whether you want to try again or continue to the next problem.")
        } else {
            translate_message("The answers of this section will not be checked.")
        }
        for (i in seq_along(extra_info)) {
            translate_message(extra_info[[i]])
        }
        if (require_wd) {
            e$file <- "/Children2007.dat"
            file_path <- paste0(getwd(), e$file)
            if (!file.exists(file_path)) {
                e$init_warn <- TRUE
                translate_message("Please change your working directory to where you unzipped the required data files (datasets.zip).")
                translate_message("You can view the location of the current working directory with the command getwd().")
                custom_message(l() %a% "You can change the working directory with the command setwd(path), where path is the location of the data files, e.g.",
                               " setwd('U:/My Documents/')")
            } else {
                e$require_wd <- FALSE
            }
        }
        custom_message("\n")
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

}
