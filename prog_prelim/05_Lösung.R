# ------------------- #
# Kapitel 5: labels
# Lösung
# ------------------- #

library(haven)
library(tidyverse)


etb18_small <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                               col_select = c("intnr", "S1", "m1202", "Bula","F411_01")) %>% 
  filter(F411_01<9)


# Bearbeiten Sie die labels dieses Diagramms:

etb18_small %>% 
  count(F411_01) %>%
  ggplot(data = ., aes(x = F411_01, y = n)) +
  geom_col(fill = "steelblue3")

# Erstellen Sie dafür zunächst einen `data.frame` mit der Auszählung durch `count()` und legen diese als Objekt ab. ------
tab_druck <- 
  etb18_small %>% 
  count(F411_01) 

tab_druck

# 1. Labeln mit `factor` ------

## Nutzen Sie `as_factor()` aus `{haven}`, um die Labels in eine separate Variable abzulegen.------
tab_druck$F411_lab <- as_factor(tab_druck$F411_01)
tab_druck

## Verändern Sie die labels nach Ihrem Geschmack: kürzen Sie, ändern Sie Bezeichungen mit `fct_recode()` -----
tab_druck$F411_lab2 <- fct_recode(tab_druck$F411_lab,
                                    Häufig = "häufig",
                                    Manchmal  = "manchmal",
                                    `eigentlich nicht` = "selten"
                                    )
tab_druck %>% 
  ggplot(data = ., aes(x = F411_lab2, y = n)) +
  geom_col(fill = "orange")


# 2. Label-`data.frame` -------

tab_druck2 <- 
  etb18_small %>% 
  count(F411_01) 


## Erstellen Sie einen Label-`data.frame` mit den Ausprägungen und den gewünschten Beschriftungen. ------
lab_df <- data.frame(F411_01 = 1:4,
                     labels = factor(c("Frequently","Sometimes","Rarely","Never")))

lab_df

## Spielen Sie mit Hilfe von `left_join()` diesen Label-`data.frame` an den `data.frame` mit den Auszählungen ran.------
tab_druck2_lab <- tab_druck2 %>% left_join(lab_df,by = "F411_01")

tab_druck2_lab

tab_druck2_lab %>% 
  ggplot(data = ., aes(x = labels, y = n)) +
  geom_col(fill = "orange")

# Profi-Aufgabe: zwei Variablen-----

etb18_small %>% 
  count(F411_01,S1) %>%
  ggplot(data = ., aes(x = F411_01, y = n, fill = factor(S1)) ) +
  geom_col(position = position_dodge())

# auszählung speichern:
tab_drk_gend <- 
 etb18_small %>% count(F411_01,S1)

tab_drk_gend$F411_01_fct <- as_factor(tab_drk_gend$F411_01)
tab_drk_gend$S1_fct <- as_factor(tab_drk_gend$S1)


tab_drk_gend %>% 
  ggplot(data = ., aes(x = F411_01_fct, y = n, fill = S1_fct ) ) +
    geom_col(position = position_dodge())
