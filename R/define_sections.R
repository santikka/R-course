# Define the sections
sections_ <- vector(mode = "list", length = 7L)
sections_[[1L]] <- create_section(1L)
sections_[[2L]] <- create_section(2L)
sections_[[3L]] <- create_section(3L, require_wd = TRUE)
sections_[[4L]] <- create_section(4L, check_answers = FALSE, extra_info = "Compare your plots with the ones shown by solution() instead.")
sections_[[5L]] <- create_section(5L, require_wd = TRUE)
sections_[[6L]] <- create_section(6L, require_wd = TRUE)
sections_[[7L]] <- create_section(7L)

# Internal section function (selector)
section_ <- function(x) {
  if (!is.numeric(x)) {
    translate_message("Invalid section number!")
  } else if (!x %in% seq_len(11L)) {
    translate_message("Invalid section number!")
  } else {
    sections_[[x]]()
  }
}
