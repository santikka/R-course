# Define the sections
sections_ <- vector(mode = "list", length = 11L)
sections_[[1]] <- create_section(1)
sections_[[2]] <- create_section(2)
sections_[[3]] <- create_section(3, require_wd = TRUE)
sections_[[4]] <- create_section(4, check_answers = FALSE, extra_info = "Compare your plots with the ones shown by solution() instead.")
sections_[[5]] <- create_section(5, require_wd = TRUE)
sections_[[6]] <- create_section(6, require_wd = TRUE)
sections_[[7]] <- create_section(7)
sections_[[8]] <- create_section(8)
sections_[[9]] <- create_section(9)
sections_[[10]] <- create_section(10, require_wd = TRUE)
sections_[[11]] <- create_section(11)

# Internal section function (selector)
section_ <- function(x) {
  if (!is.numeric(x)) {
    translate_message("Invalid section number!")
  } else if (!x %in% 1:11) {
    translate_message("Invalid section number!")
  } else {
    sections_[[x]]()
  }
}
