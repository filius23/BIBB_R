# ------------------- #
# Kapitel 7: t-Test, Korrelation 
# Lösung
# ------------------- #
library(tidyverse)
etb18_ue7 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                             n_max = 1000,
                             col_select = c("intnr","S1","az","F518_SUF","m1202","F411_01","gew2018")) %>% 
  mutate(across(matches("m1202|F411_01"), ~ifelse(.x > 4|.x<0,NA,.x))) # missings in m1202 mit NA überschreiben


# Übung 1 ---------
# Unterschied in der Arbeitszeit (`az`) zwischen Männern und Frauen besteht (`S1`)
t.test(etb18_ue7$az~etb18_ue7$S1)


# Berechnen Sie das Cohen's d für diesen Zusammenhang.
library(effectsize)
cohens_d(etb18_ue7$az~etb18_ue7$S1)

# Übung 2 ---------

## Korrelation az & F518_SUF ----
etb18_ue7$F518_SUF[etb18_ue7$F518_SUF>99990] <- NA

cor.test(etb18_ue7$F518_SUF,etb18_ue7$az,method = "pearson", use = "pairwise.complete.obs")

## Rangkorrelation starker Termin- oder Leistungsdruck F411_01 und der Ausbildungsvariable m1202 -------
cor.test(etb18_ue7$m1202,etb18_ue7$F411_01,method = "spearman", use = "pairwise.complete.obs")


effectsize::cramers_v(etb18_ue7$m1202,etb18_ue7$F411_01)


## Bonus: Korrelation nach Geschlechtern
library(correlation)
etb18_ue7 %>%
  group_by(S1) %>%
  select(F518_SUF,F411_01,az) %>% 
  correlation() 

# Übung 3 ----------

library(survey)
etb18_ue7$F518_SUF[etb18_ue7$F518_SUF>99990] <- NA

etb18_ue7_weighted <- svydesign(id      = ~intnr,
                            weights = ~gew2018,
                            data    = etb18_ue7)

svymean(~F518_SUF, etb18_ue7_weighted, na.rm = TRUE)
mean(etb18_ue7$F518_SUF, na.rm = TRUE) # zum Vergleich


# Bonus: Kreuztabelle mit Gewichtung
svytable(~m1202+F411_01,etb18_ue7_weighted) # syntax wie xtabs()
xtabs(~m1202+F411_01,etb18_ue7) # zum Vergleich
