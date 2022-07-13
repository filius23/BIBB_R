


if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 
library(tidyverse)


# https://datasciencebook.ca/
# https://towardsdatascience.com/r-programming-a-study-guide-for-beginners-732de9542aa8?gi=dce1e602d5ab 

path1 <- "D:/oCloud/RFS/images/"
path2 <- here::here("pic") %>% paste0(.,"/")

file.copy(from = paste0(path1,"102_Dateipfad_Win.png"),to = paste0(path2,"102_Dateipfad_Win.png"))
file.copy(from = "D:/oCloud/RFS/images/rstudio-icon.png",to = paste0(path2,"rstudio-icon.png"))


myimages2 <- c(paste0(path1,"/101_engine_R.png"),paste0(path1,"/101_cockpit_rstudio2.png"))
myimages3 <- c(paste0(path2,"/101_engine_R.png"),paste0(path2,"/101_cockpit_rstudio2.png"))


file.copy(from = myimages2[[1]],to = myimages3[[1]])
dir.create(here::here("pic"))


quarto::quarto_render("01_intro.qmd")
quarto::quarto_preview("references.qmd")