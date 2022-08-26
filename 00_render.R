
# knitr::purl(input = "09_reg.qmd",output= "09_reg.R",documentation = 0)


if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 
library(tidyverse)


# https://datasciencebook.ca/
# https://towardsdatascience.com/r-programming-a-study-guide-for-beginners-732de9542aa8?gi=dce1e602d5ab 
# https://datacarpentry.org/r-socialsci/
# https://sta-112-f22.github.io/website/schedule.html
# https://github.com/mattansb/Practical-Applications-in-R-for-Psychologists/

# https://github.com/andrewheiss/2021-seacen
# https://crimebythenumbers.com/

# count() als Haupt table-funktion (die andern nur kurz zeigen)
# [] am Ende zeigen -> "übrigens"
# summarise auf Grafik einbauen?
# slice()/pull() einbauen ?
# Kennzahlenkapitel eindampfen -> Lagemaße usw nicht groß unterscheiden
# summarise -funktionen aus SMART-Syntax verwenden -> summary selbst bauen
# Variationskoeffizient function selber bauen als Beispiel





# - export vs save
# - factor Beispiel (as.character…)
# - ersetzen durch [ ]
# - Bedingungen Hintergrund: Auswahl mit T/F, dann logische Vektoren, zusammen führen
# - file_path
# <-
#   readClipboard()
# #
# - Histogramm, Boxplot aus https://modernstatisticswithr.com/thebasics.html#plotting-numerical-data
# - Fehlermeldungen aus https://modernstatisticswithr.com/thebasics.html#troubleshooting
# 


# 
# path1 <- "D:/oCloud/RFS/images/"
# path2 <- here::here("pic") %>% paste0(.,"/")
# 
# file.copy(from = paste0(path1,"102_Dateipfad_Win.png"),to = paste0(path2,"102_Dateipfad_Win.png"))
# file.copy(from = "D:/oCloud/RFS/images/rstudio-icon.png",to = paste0(path2,"rstudio-icon.png"))


quarto::quarto_render("01_intro.qmd")
quarto::quarto_render("03_desc.qmd")
quarto::quarto_preview("03_desc.qmd")