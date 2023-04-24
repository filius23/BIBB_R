# ------------------- #
# Kapitel 7: t-Test, Korrelation 
# Lösung
# ------------------- #
library(tidyverse)
etb18_ue7 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                             n_max = 1000,
                             col_select = c("intnr","S1","az","F518_SUF","m1202","F411_01","gew2018")) %>% 
  mutate(across(matches("m1202|F411_01"), ~ifelse(.x > 4|.x<0,NA,.x)),
         F518_SUF = ifelse(F518_SUF>99990,NA,F518_SUF)) # missings in m1202 mit NA überschreiben

# Übung 1 ---------
# Unterschied in der Arbeitszeit (`az`) zwischen Männern und Frauen besteht (`S1`)

t.test(etb18_ue7$az ~ etb18_ue7$S1, alternative = "two.sided")
t.test(etb18_ue7$az ~ etb18_ue7$S1, alternative = "two.sided",conf.level = 0.99)

t_test2 <- t.test(etb18_ue7$az ~ etb18_ue7$S1, alternative = "two.sided")
t_test2$null.value



# Berechnen Sie das Cohen's d für diesen Zusammenhang.
# install.packages("effectsize")
library(effectsize)
cohens_d(etb18_ue7$az ~ etb18_ue7$S1)

# gepaired t-Test mit paired = TRUE
t.test(etb18_ue7$az ~ etb18_ue7$az,paired = TRUE) # mach hier keinen SInn


# Übung 2 ---------

## Korrelation az & F518_SUF ----
summary(etb18_ue7$F518_SUF)
summary(etb18_ue7$az)

cor.test(etb18_ue7$F518_SUF,etb18_ue7$az,
         method = "pearson")

## Rangkorrelation starker Termin- oder Leistungsdruck F411_01 und der Ausbildungsvariable m1202 -------
cor.test(etb18_ue7$m1202,etb18_ue7$F411_01,method = "spearman")

library(effectsize)
cramers_v(etb18_ue7$m1202,etb18_ue7$F411_01)

# Übung 3 ----------
library(survey)
etb18_ue7_weighted <- svydesign(id      = ~intnr,   # id angeben 
                                weights = ~gew2018, # gewichtungsvariable
                                data    = etb18_ue7)#aus welchem Datensatz kommt das ganze?

View(etb18_ue7_weighted)

svymean(~F518_SUF, design = etb18_ue7_weighted, na.rm = TRUE)

mean(etb18_ue7$F518_SUF, na.rm = TRUE) # zum Vergleich
mean(etb18_ue7_weighted$variables$F518_SUF, na.rm = TRUE) # zum Vergleich


# Bonus: Kreuztabelle mit Gewichtung
svytable(~m1202+F411_01,etb18_ue7_weighted) # syntax wie xtabs()
xtabs(~m1202+F411_01,etb18_ue7) # zum Vergleich
