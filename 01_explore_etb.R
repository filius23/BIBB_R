if(Sys.getenv("USERNAME") == "Filser" ) .libPaths("D:/R-library4")
knitr::opts_chunk$set(collapse = F)


bib <- haven::read_dta("D:/Datenspeicher/BIBB_BAuA/BIBBBAuA_2018_suf1.0.dta")


table(bib$m1202)






