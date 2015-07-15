source("C:/latex-utils/tex-report-get-tables.r")
require(dplyr)
require(magrittr)
shkdir <- "C:/Projects/SHK-indicators-2015/"
tbldir <- paste0(shkdir, "Tables-in-report")
make.latex.table(tbldir, fname="/report-tables.tex")
