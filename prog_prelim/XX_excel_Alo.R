if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4")

library(tidyverse)
library(janitor)
# einlesen
alo_df <- readxl::read_xlsx("./data/ALO_Stat_AI008-1_full.xlsx",col_names = F)
alo_df


# spaltennamen erstellen ------ 
names_vector <- 
  alo_df %>% 
  slice(3) %>% 
  unlist() %>%  # in vector umformatieren
  janitor::make_clean_names() %>% 
  str_replace(.,"anteil_","pct_") %>% 
  str_replace(.,"arbeitslosen|arbeitslose","alo") %>% 
  str_replace(.,"kreise_und_kreisfreie_stadte","ags") %>% 
  str_remove(.,"_an_arbeitslosen_insgesamt") %>% 
  str_remove(.,"_jahre") %>% 
  ifelse(.=="na","name",.)


# Kopfzeilen raus, Spaltennamen vergeben ------
alo_df2 <- 
  alo_df %>% 
    slice(-c(1:6)) %>% # obere zeilen weg
    setNames(names_vector) %>%  # variablennamen setzen
  mutate(jahr = as.numeric(duplicated(ags))+2017) 

alo_df2 %>% count(jahr) # check

# Bundesland Angaben -----------------
alo_df_bula <- alo_df2 %>% filter(nchar(ags)==2 & ags != "DG") %>% 
  select(1:3,jahr)

saveRDS(alo_df_bula,file = "./data/alo_bula.Rdata")


# pivot aufgabe
alo_df_bula %>% filter(grepl("05|03",ags)) %>% pivot_wider(names_from = jahr,names_prefix = "alo_",values_from = aloquote) %>% 
  saveRDS(.,file = "./data/alo_bula2.Rdata")
  


alo_df_bula %>% 
  filter(jahr == 2017, grepl("05|03",ags)) %>% 
  mutate(ags = as.numeric(ags)) %>% 
  select(-jahr) %>% 
  saveRDS(.,file = "./data/alo_bula1.Rdata")

alo_df_bula %>% 
  filter(grepl("05|03",ags)) %>% 
  mutate(ags = as.numeric(ags)) %>% 
  saveRDS(.,file = "./data/alo_bula1_jahr.Rdata")