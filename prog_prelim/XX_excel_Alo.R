if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4")

library(tidyverse)
library(janitor)
# einlesen
alo_df <- readxl::read_xlsx("./data/ALO_Stat_AI008-1.xlsx",col_names = F)
alo_df



# alo_17_2 <- 
  alo_df %>% 
    slice(-c(1:7)) %>% # obere zeilen weg
    setNames(c("land_num","land","alo","alo_ausländer","alo_schwerbeh","alo_15_20","alo_15_25","alo_55_65","langzeit")) # variablennamen setzen








list.files(pattern = "lock",recursive = T)