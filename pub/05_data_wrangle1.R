# ------------------- #
# Kapitel 5: labels
# Lösung
# ------------------- #
library(haven)
library(tidyverse)

etb18_kap5 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                               col_select = c("intnr", "S1", "m1202", "Bula","F411_01","az","zpalter")) %>% 
  filter(F411_01<9, zpalter < 100)

# attributes -------------
# Labels aus Stata-Datensatz werden als attributes() angezeigt:

View(etb18_kap5)
etb18_kap5 %>% count(S1)

attributes(etb18_kap5$S1)

# leider führt das immer wieder zu Problemen
library(ggplot2)
ggplot(data = etb18_kap5, aes(x = zpalter, y = az, color = S1 )) + 
  geom_point()

# as_factor() aus haven ---------------

etb18_kap5$S1_fct <- as_factor(etb18_kap5$S1)

# ansehen:
etb18_kap5 %>% select(contains("S1")) %>% head()

# in ggplot() verwenden:
ggplot(data = etb18_kap5, aes(x = zpalter, y = az, color = S1_fct )) + 
  geom_point()

# `factor` selbst erstellen oder bearbeiten --------------

# labels für m1202 sind sehr lang:
tab1 <- etb18_kap5 %>% count(m1202) %>% mutate(m1202_fct = as_factor(m1202))
tab1

##  factor() selber erstellen ------
tab1$m1202_fct2 <- factor(tab1$m1202, 
                        levels = c(1,2,3,4), 
                        labels = c("ohne Abs.", "dual/schul.", "Aufstiegsfortb.", "FH/Uni"))
tab1

## fct_recode aus {forcats} -----------------
levels(tab1$m1202_fct)[3] # vollständiges, langes label

tab1$m1202_fct3 <- fct_recode(tab1$m1202_fct,
  `duale / schulische Ausb.` = "duale o. schulische Berufsausbildung/einf.,mittl. Beamte", # bei Leerzeichen `` um die Wörter
  )

# vergleich der factor-Varianten
tab1 %>% select(m1202_fct,m1202_fct2,m1202_fct3)

# Anhang -----------

## label-Data.frame mergen -----
tab2 <- etb18_kap5 %>% count(m1202)
lab_df <- data.frame(m1202=1:4)
lab_df
lab_df$m1202_lab <- factor(lab_df$m1202,levels = 1:4,
                           labels = c("ohne Abs.", "dual/schul.", "Aufstiegsfortb.", "FH/Uni"))
lab_df

tab2 %>% 
  left_join(lab_df,by = "m1202")

tab2 %>% 
  left_join(lab_df,by = "m1202") %>% 
  ggplot(data = ., aes(x = m1202_lab, y = n)) +
  geom_col(position=position_dodge(), fill = "turquoise3")

## labels für Stata setzen -----

library(labelled)

etb18_kap5$S1_num2 <- as.numeric(etb18_kap5$S1)
attributes(etb18_kap5$S1_num2)
val_labels(etb18_kap5$S1_num2) <- c("Mann"=1,"Frau"=2)
attributes(etb18_kap5$S1_num2)
etb18_kap5 %>% count(S1_num2)
etb18_kap5 %>% 
  select(S1_num2) %>% 
  haven::write_dta(.,path = "./data/etb18_kap5.dta")

