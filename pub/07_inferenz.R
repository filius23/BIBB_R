# ------------------- #
# Kapitel 7: t-Test, Korrelation 
# ------------------- #

library(tidyverse)
etb18_kap7 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                              n_max = 1000,
                              col_select = c("intnr","zpalter","S1","F231","az","F600_12",
                                             "m1202","F204","gew2018_hr17","gew2018","F518_SUF",
                                             "F411_01","S3","Mig")) %>% 
  mutate(F600_12 = ifelse(F600_12 > 4,NA,F600_12),
         zpalter = ifelse(zpalter>100,NA,zpalter),
         m1202 = ifelse(m1202 > 4,NA,m1202))

## install.packages("broom") # Ergebnisse in data.frame umwandeln
## install.packages("effectsize") # u.a. Cohen's D berechnen
## install.packages("survey") # Gewichtung

# t.test ------------

t.test(etb18_kap7$zpalter,mu = 45)

t.test(etb18_kap7$zpalter ~ etb18_kap7$S1)
t.test(etb18_kap7$zpalter ~ etb18_kap7$S1,alternative = "two.sided")

t.test(etb18_kap7$zpalter~etb18_kap7$S1,alternative = "less")
t.test(etb18_kap7$zpalter~etb18_kap7$S1,alternative = "greater")

# testergebnisse als objekt
t_test <-  t.test(etb18_kap7$zpalter~etb18_kap7$S1,alternative = "two.sided")

t_test$estimate <- NULL

library(effectsize)
cohens_d(etb18_kap7$zpalter~etb18_kap7$S1)

## die Gruppenvariable darf nur 2 Ausprägungen haben ---------------------
table(etb18_kap7$m1202)
t.test(etb18_kap7$zpalter ~ etb18_kap7$m1202)

### Variante a): ad-hoc Dummy m1202 = 1 vs. alle anderen Ausprägungen ----
t.test(etb18_kap7$zpalter ~ etb18_kap7$m1202 == 1)

### Variante b): mit data-Argument filtern auf zwei Ausprägungen ---------
t.test(zpalter ~ m1202, data = etb18_kap7 %>% filter(m1202 %in% c(3,4) ) )

# Übung


# Korrelation -----
attributes(etb18_kap7$F231)$label
etb18_kap7$F231[etb18_kap7$F231>990] <- NA

cor.test(etb18_kap7$zpalter , etb18_kap7$F231, method = "pearson")

cor.test(etb18_kap7$zpalter , etb18_kap7$F231, method = "pearson")
c1 <- cor.test(etb18_kap7$zpalter,etb18_kap7$F231,method = "pearson")

c1$method

# Übung -----

# Korrelation ------------
## install.packages("correlation")

library(correlation)
etb18_kap7 %>% 
  select(zpalter,F231,az) %>% 
  correlation() %>% 
  summary(.)

## group_by wird in correlation  berücksichtigt -----
etb18_kap7 %>%
  group_by(S1) %>%
  select(zpalter,F231,az) %>% 
  correlation()

corr_df <- 
  etb18_kap7 %>%
  group_by(S1) %>%
  select(zpalter,F231,az) %>% 
  correlation()
data.frame(corr_df)


## Rangkorrelation ----
cor.test(etb18_kap7$m1202,etb18_kap7$F600_12,method = "spearman")
cor.test(etb18_kap7$m1202,etb18_kap7$F600_12, method = "kendall")

etb18_kap7$F204[etb18_kap7$F204>4] <- NA
etb18_kap7 %>% filter(!is.na(F204)) %>% count(F204,S1)
attributes(etb18_kap7$F204)$label

## Chi² ------------

xtabs(~ F204 + S1, data = etb18_kap7)


tab1 <- xtabs(~ F204 + S1, data = etb18_kap7)
chisq.test(tab1)

chisq.test(xtabs(~ F204 + S1, data = etb18_kap7))

xtabs(~ F204 + S1, data = etb18_kap7) %>%  chisq.test(.)

effectsize::cramers_v(etb18_kap7$F204,etb18_kap7$S1)

# Tests als objekte ------
corr1 <- cor.test(etb18_kap7$zpalter,etb18_kap7$F231,method = "pearson")
View(corr1)

# Übung ------

# Gewichtung ----------

## mit dplyr/tidyverse Funktionen ----
etb18_kap7 %>% 
  count(S1,m1202,wt = gew2018_hr17)
etb18_kap7 %>% summarise(mean = weighted.mean(zpalter,w = gew2018))

etb18_kap7 %>% 
  count(S1,m1202,wt = gew2018_hr17)
etb18_kap7 %>% 
  count(S1,m1202)


## mit {survey} -----
library(survey)

etb18_weighted <- svydesign(id      = ~intnr,
                            weights = ~gew2018,
                            data    = etb18_kap7)


# Mittelwert
svymean(~zpalter, etb18_weighted, na.rm = TRUE)
mean(etb18_kap7$zpalter, na.rm = TRUE)


# Häufigkeitstabelle
svytable(~S1+m1202,etb18_weighted) # syntax wie xtabs()
xtabs(~S1+m1202,etb18_kap7)

?survey::svytable()
