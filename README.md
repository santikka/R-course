# R-course
An interactive R-course by University of Jyväskylä and University of Eastern Finland.

## Acknowledgements
The following people have contributed to this package: Santtu Tikka, Juho Kopra, Kasperi Kansanen.

## Contents
This repository contains the data used by the course package, 
additional documentation, the Rcourse package source code, 
internal `sysdata.rda` generation, final exam tests and 
Finnish and English localization.

### data
Data sets used in the exercise problems and the PISA data 
for the final exam. The file `datasets.zip` is available to the enrolled students.

### docs
Source materials for the additional documentation of the final 
exam of the course. The formulas used in the final exam are available for the 
enrolled students via `formulas.pdf` and the PISA data description is available 
via `PISA_variables.pdf`. Also contains documentation for the Rcourse package.

### Rcourse
The Rcourse package directory. Contains the source code of the package and 
various tests.

### sysdata
Contains the script `sysdata.R` which implements the function `make_sysdata()`.
This function creates the file `Rcourse/R/sysdata.rda` based on the files in the 
translations directory.

### translations
Provides the character strings for the interactive package UI in both 
English and Finnish as the files `Rcourse_interface_en.yaml` and
`Rcource_interface_fi.yaml`, respectively. The translations of the exercise
problems and the final exam can be found in the file 
`Rcourse_questions_en.yaml` and `Rcourse_questions_fi` for their respective languages. 
If any of these files are changed, `sysdata.rda` has to be recreated for the package, and the package rebuilt.
