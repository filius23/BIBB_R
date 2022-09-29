
knitr::purl(input = "06_data_wrangle2.qmd",
            output = "./prog_prelim/06_data_wrangle2.R",documentation = 0)

rstudioapi::navigateToFile("./prog_prelim/06_data_wrangle2.R")



if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 