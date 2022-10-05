if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 
# library(extrafont)
# font_import(pattern = "Roboto",prompt = F)
# fonts()
knitr::opts_chunk$set(
  collapse = F,
  comment = "#>",
  echo = T,
  cache = T,
  warning = FALSE,
  message = FALSE
)
library(systemfonts)
# system_fonts() %>% filter(grepl("Roboto",name)) %>% select(family,1:3)

windowsFonts(mono=windowsFont("FiraMono"))
windowsFonts(Roboto=windowsFont("Roboto"))

source("./tidyexplain/00_base_join.R")
df_names <- tibble(
  .x = c(1.5, 4.5), .y = 0.25,
  value = c("dat1", "dat2"),
  size = 20,
  color = "black"
)

dat1 <- x
dat2 <- y


g <- plot_data(initial_join_dfs) +
  geom_text(data = df_names, family = "Roboto", size = 24) 
g

source("tidyexplain/inner_join.R")
# source("tidyexplain/left_join.R")
source("tidyexplain/left_join_extra.R")
# source("tidyexplain/right_join.R")
source("tidyexplain/full_join.R")

etb18_int_bl <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                                col_select = c("intnr","Bula") # mit col_select() können Variablen ausgewählt werden
                                )

etb_ids <-  etb18_int_bl %>% slice(c(1,125,1230,21010,8722) )

set.seed(90459)
alo_bula <- data.frame(bundesland = seq(1:8),
                       Werte = sample(letters,size = 8) # mit sample() kann eine zufällige Auswahl getroffen werden 
                       )

etb_ids
alo_bula
etb_ids %>% left_join(alo_bula,by = c("Bula"="bundesland"))

table(etb_ids$Bula %in% alo_bula$bundesland)

bsp_df <- 
  data.frame(
    bula    = c("NRW","NDS"),
    alo2018 = c(2,2),
    alo2017 = c(1,1)
    )

bsp_df

bsp_df %>% pivot_longer(cols = c("alo2018","alo2017"),names_to = "year",values_to = "alo")

bsp_df %>% pivot_longer(cols = c("alo2018","alo2017"),names_to = "year",values_to = "alo",names_prefix = "alo")

bsp_df2 <- 
  data.frame(land = c("NRW","NDS","NRW","NDS"),
             alo = c(2.1,1.8,2.4,2.2),
             alter = c("age_1825","age_1825","age_2630","age_2630"))
bsp_df2

bsp_df2 %>% pivot_wider(names_from = alter,values_from = alo)

etb_ue11 <- haven::read_dta("D:/Datenspeicher/BIBB_BAuA/BIBBBAuA_2018_suf1.0.dta",
                       col_select = c("intnr","int_jahr","Bula")) %>% 
  slice(34:35,68:69,62,687,625,684,599:600)
etb_ue11

alo <- readRDS(file = "./data/alo_bula1.Rdata")
alo

alo_j <- readRDS(file = "./data/alo_bula1_jahr.Rdata")
alo_j

alo_wide <- readRDS(file = "./data/alo_bula2.Rdata")
alo_wide

alo_wide %>% pivot_longer(cols = 3:4,names_to = "jahr",values_to = "alo_quote",names_prefix = "alo_")
