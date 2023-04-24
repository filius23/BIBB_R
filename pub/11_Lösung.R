# ------------------- #
# Kapitel 11: Data Wrangling 3
# Lösung
# ------------------- #

library(tidyverse)

# Übung 1 -----
# Verknüpfen Sie die ausgewählten Beobachtungen der ETB 2018 mit [Arbeitsmarktstatistiken](https://www.regionalstatistik.de/genesis//online?operation=table&code=AI008-1) von Destatis.

etb_ue11 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                            col_select = c("intnr","int_jahr","Bula")) %>% 
  slice(34:35,68:69,62,687,625,684,599:600)
etb_ue11

# So können Sie die aufbereiteten Arbeitslosendaten einlesen (die Zahlencodes in beiden Datensätzen stimmen überein):
alo <- readRDS(file = "./data/alo_bula1.Rdata")
alo

## Lösung --------------
etb_ue11 %>% left_join(alo,by = c("Bula"="ags"))
etb_ue11 %>% inner_join(alo,by = c("Bula"="ags"))

## wie finden wir die Fälle, die nicht gemergt werden?
table(etb_ue11$Bula %in% alo$ags)
etb_ue11 %>% filter(!(Bula %in% alo$ags))

etb_ue11 %>% anti_join(alo,by = c("Bula"="ags"))

# etb_ue11$Bula[etb_ue11$Bula %in% alo$ags == FALSE]


# Wie müssten Sie vorgehen, wenn Sie nun jahresgenaue Angaben haben und dementsprechend zusätzlich auch nach dem Jahr mergen möchten?
alo_j <- readRDS(file = "./data/alo_bula1_jahr.Rdata")
alo_j

etb_ue11 %>% left_join(alo_j,by = c("Bula"="ags","int_jahr"="jahr"))


# Übung 2  -------------

alo_wide <- readRDS(file = "./data/alo_bula2.Rdata")
alo_wide

## Bringen Sie `alo_wide` in das long shape ---------
alo_wide %>% pivot_longer(cols = matches("alo"),names_to = "year",values_to = "alq")
alo_wide %>% pivot_longer(cols = 3:4,names_to = "year",values_to = "alq")
alo_wide %>% pivot_longer(cols = starts_with("alo"),names_to = "year",values_to = "alq")
alo_wide %>% pivot_longer(cols = contains("alo"),names_to = "year",values_to = "alq")
# ?select_helpers

alo_wide %>% pivot_longer(cols = 3:4,names_to = "jahr",values_to = "alo_quote",
                          names_prefix = "alo_")

alo_long <- 
alo_wide %>% 
  pivot_longer(cols = 3:4,names_to = "jahr",values_to = "alo_quote",
               names_prefix = "alo_") %>% 
  mutate(jahr = as.numeric(jahr))



