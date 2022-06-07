# Compiles all internal data for the Rcourse package

# Construct the interface elements
english <- yaml::read_yaml("./translations/Rcourse_interface_en.yaml", fileEncoding = "UTF-8")
finnish <- yaml::read_yaml("./translations/Rcourse_interface_fi.yaml", fileEncoding = "UTF-8")

# Questions for exercise sections and exams
english_questions <- yaml::read_yaml("./translations/Rcourse_questions_en.yaml", fileEncoding = "UTF-8")
finnish_questions <- yaml::read_yaml("./translations/Rcourse_questions_fi.yaml", fileEncoding = "UTF-8")

# Exercise questions
n_section <- 11L
section_questions <- vector(mode = "list", length = n_section)
for (i in 1:n_section) {
    finnish_temp <- finnish_questions[[i]]
    english_temp <- english_questions[[i]]
    section_questions[[i]] <- unlist(english_temp)
    names(finnish_temp) <- names(english_temp) <- section_questions[[i]]
    english <- c(english, english_temp)
    finnish <- c(finnish, finnish_temp)
}

# Exam questions (JYU)
n_parts <- 2L
exam_questions_jyu <- vector(mode = "list", length = n_parts)
for (i in 1:n_parts) {
    exam <- paste0("Exam ", i, " JYU")
    finnish_temp <- as.list(unique(unlist(finnish_questions[[exam]])))
    english_temp <- as.list(unique(unlist(english_questions[[exam]])))
    names(finnish_temp) <- names(english_temp) <- unlist(english_temp)
    english <- c(english, english_temp)
    finnish <- c(finnish, finnish_temp)
    exam_questions_jyu[[i]] <- english_questions[[exam]]
}

# Exam question (UEF)
finnish_temp <- as.list(unique(unlist(finnish_questions$`Exam UEF`)))
english_temp <- as.list(unique(unlist(english_questions$`Exam UEF`)))
names(finnish_temp) <- names(english_temp) <- unlist(english_temp)
english <- c(english, english_temp)
finnish <- c(finnish, finnish_temp)
exam_questions_uef <- english_questions$`Exam UEF`

# Check that all element fields match and are not NULL
en_names <- names(english)
fi_names <- names(finnish)
lang_errors <- FALSE
if (!identical(en_names, fi_names)) {
    mis <- which(!en_names %in% fi_names | !fi_names %in% en_names |
                 is.null(fi_names) | is.null(en_names))
    # Report all mismatches
    if (length(mis)) {
        lang_errors <- TRUE
    }
    for (i in seq_along(mis)) {
        message("Mismatch at index i = ", mis[i], " >>> EN: ", en_names[mis[i]], " --- FI: ", fi_names[mis[i]])
    }
}
temp_english <- english
names(temp_english) <- NULL
if (!identical(en_names, temp_english)) {
    mis <- which(!en_names %in% temp_english | !temp_english %in% en_names)
    if (length(mis)) lang_errors <- TRUE
    for (i in seq_along(mis)) {
        message("Mismatch found: FIELD: ", en_names[mis[i]], " --- EN: ", temp_english[mis[i]])
    }
}
if (lang_errors) {
    stop("Language object element fields do not match!")
}

# Eliminate potential duplicates
english <- english[unique(names(english))]
finnish <- finnish[unique(names(finnish))]

# Internal data for the package
data("CO2", package = "datasets", envir = environment())
carbon <- CO2
carbon$Plant <- as.character(carbon$Plant)
carbon$Type <- as.character(carbon$Type)
carbon$Treatment <- as.character(carbon$Treatment)
carbon$conc <- as.integer(carbon$conc)
class(carbon) <- "data.frame"
attr(carbon, "formula") <- NULL
attr(carbon, "outer") <- NULL
attr(carbon, "labels") <- NULL
attr(carbon, "units") <- NULL
attr(carbon, "row.names") <- as.character(row.names(carbon))

data("women", package = "datasets", envir = environment())
heightweight <- women
heightweight$height <- as.integer(heightweight$height)
heightweight$weight <- as.integer(heightweight$weight)
attr(heightweight, "row.names") <- as.character(row.names(heightweight))

data("mtcars", package = "datasets", envir = environment())
auto <- subset(mtcars, select = c("hp", "wt", "am"))
auto$hp <- as.integer(auto$hp)
auto$am <- as.integer(auto$am)
attr(auto, "row.names") <- as.character(row.names(auto))

children <- read.table("./data/Children2007.dat", header = TRUE, fileEncoding = "UTF-8")
children$Gender2 <- factor(children$Gender, labels = c("Boy", "Girl"))
children2 <- children[-1,]
tomato <- read.table("./data/tomato.dat", header = TRUE, fileEncoding = "UTF-8")
ftomato <- tomato
ftomato$fSTRAIN <- factor(ftomato$STRAIN,
                          labels = c("STRAIN1", "STRAIN2", "STRAIN3"))
pigs <- read.csv2("./data/pigs.csv", fileEncoding = "UTF-8")
pisa <- read.table("./data/PISA_value.txt", header = TRUE, fileEncoding = "UTF-8")

rm(CO2, english_temp, finnish_temp, mtcars, women, temp_english,
   en_names, fi_names, i, lang_errors, n_parts, n_section, exam, mis)
