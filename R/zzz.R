#' @importFrom utils packageVersion
#' @noRd
.onAttach <- function(...) {
    packageStartupMessage(paste0(l() %a% "R-course package version", " ", utils::packageVersion("Rcourse"), "\n",
                                 l() %a% "Please read the instructions carefully at the course webpage.", "\n" ,
                                 l() %a% "Type info() to begin."))
    invisible()
}
