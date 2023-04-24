# ------------------- #
# Kapitel 11: Data Wrangle
# merge/join & reshape
# ------------------- #
library(tidyverse)

# Daten einlesen -----
etb18_int_bl <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                                col_select = c("intnr","Bula") # mit col_select() können Variablen ausgewählt werden
                                )

# join -----------

# Beispieldaten 
  etb_ids <-  etb18_int_bl %>% slice(c(1,125,1230,21010,8722:8725) )
  set.seed(90459)
  alo_bula <- data.frame(bundesland = seq(1:8),
                         Werte = sample(letters,size = 8) # mit sample() kann eine zufällige Auswahl getroffen werden 
                         )

etb_ids
alo_bula

etb_ids %>% left_join(alo_bula,by = c("Bula"="bundesland"))


etb_ids %>% inner_join(alo_bula,by = c("Bula"="bundesland"))
# alle zeilen aus beiden datensätzen behalten
etb_ids %>% full_join(alo_bula,by = c("Bula"="bundesland"))
# check: welche Zeilen aus dem ursprungs-data.frame werden nicht verknüpft?
etb_ids %>% anti_join(alo_bula,by = c("Bula"="bundesland"))



# check: gibt es Entsprechungen?
etb_ids$Bula %in% alo_bula$bundesland

table(etb_ids$Bula %in% alo_bula$bundesland)


# 1:1 oder 1_m wird nicht ab
merge_ergebnis <- etb_ids %>% left_join(alo_bula,by = c("Bula"="bundesland"))
nrow(etb_ids) == nrow(merge_ergebnis)

## bind_rows() -----
etb_ids
etb_ids2 

bind_rows(etb_ids,etb_ids2)

# pivot/reshape -------

# beispiel
    bsp_df <- 
      data.frame(
        bula    = c("NRW","NDS"),
        alo2018 = c(3,2.5),
        alo2017 = c(1,1.24)
        )
    
bsp_df

# ins long shape: pivot_longer--------
# library(tidyr) # teil des tidyverse

bsp_df %>% 
  pivot_longer(cols = c("alo2018","alo2017"),names_to = "year",values_to = "alq")

bsp_df %>% 
  pivot_longer(cols = c("alo2018","alo2017"),names_to = "jahr",values_to = "ALQ")

# names_prefix 
bsp_df %>% 
  pivot_longer(cols = c("alo2018","alo2017"),
               names_to = "year",values_to = "alo",
               names_prefix = "alo")

bsp_df %>% 
  pivot_longer(cols = c("alo2018","alo2017"),
               names_to = "year",values_to = "alo",
               names_prefix = "alo") %>% 
  ggplot(.,aes(x=alo , y = bula, color = year)) + 
  geom_point(size = 9)





# ins wide shape: pivot_wider -------
bsp_df2 <- 
  data.frame(land = c("NRW","NDS","NRW","NDS"),
             alo = c(2.1,1.8,2.4,2.2),
             alter = c("age_1825","age_1825","age_2630","age_2630"))
bsp_df2

bsp_df2 %>% pivot_wider(names_from = alter,values_from = alo)


