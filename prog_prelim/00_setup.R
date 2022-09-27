
dir.create("./prog_prelim")
knitr::purl(input = "03_desc.qmd",
            output = "./prog_prelim/03_desc.R",documentation = 0)

rstudioapi::navigateToFile("./prog_prelim/03_desc.R")



if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 