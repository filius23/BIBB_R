# ------------------- #
# Kapitel 5: labels
# Lösung
# ------------------- #

library(haven)
library(tidyverse)


etb18_ue5 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                               col_select = c("intnr", "S1", "m1202", "Bula","F411_01")) %>% 
  filter(F411_01<9)


# Bearbeiten Sie die labels dieses Diagramms:

etb18_ue5 %>% 
  count(F411_01) %>%
  ggplot(data = ., aes(x = F411_01, y = n)) +
  geom_col(fill = "steelblue3")

# Erstellen Sie dafür zunächst einen `data.frame` mit der Auszählung durch `count()` und legen diese als Objekt ab. ------
tab_druck <- 
  etb18_ue5 %>% 
  count(F411_01) 

## mit as_factor
as_factor(tab_druck$F411_01)
tab_druck$F411_01_fct <- as_factor(tab_druck$F411_01)

tab_druck
class(tab_druck$F411_01_fct)

tab_druck %>% 
  ggplot(data = ., aes(x = F411_01_fct, y = n)) +
  geom_col(fill = "coral")

## Erstellen Sie einen `factor` mit den gewünschten Beschriftungen. ------
tab_druck$F411_01_fct2 <- factor(tab_druck$F411_01,
                                 levels = 1:4,
                                 labels = c("Frequently","Sometimes","Rarely","Never"))

tab_druck

tab_druck %>% 
  ggplot(data = ., aes(x = F411_01_fct2, y = n)) +
  geom_col(fill = "orange")


