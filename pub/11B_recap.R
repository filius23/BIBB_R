
library(tidyverse)

etb18_kap7 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                              n_max = 1000,
                              col_select = c("intnr","zpalter","S1","F231","az","F600_12",
                                             "m1202","F204","gew2018_hr17","gew2018","F518_SUF",
                                             "F411_01","S3","Mig")) %>% 
  mutate(F600_12 = ifelse(F600_12 > 4,NA,F600_12),
         zpalter = ifelse(zpalter>100,NA,zpalter),
         mig_stat = case_when(Mig == 0 ~ 0,
                              Mig %in% 1:2 ~ 1),
         m1202 = ifelse(m1202 > 4,NA,m1202))

# Zusammenhänge ---------

# t.test 
t.test(etb18_kap7$az~etb18_kap7$S1)
t.test(az~S1,data = etb18_kap7)

# Regressionsmodelle in R ------------

## lm()
lm(F518_SUF ~ zpalter, data = etb18_kap7)
lm(F518_SUF ~ zpalter + az, data = etb18_kap7) # mehrere Variablen

## summary() fü eine einfache Regressionstabelle in der Console
m1 <- lm(F518_SUF ~ zpalter + az, data = etb18_kap7)
summary(m1)

## kategoriale UVs
etb18_kap7$S1_fct <- factor(etb18_kap7$S1,levels = 1:2, labels = c("Männer","Frauen"))
m2 <- lm(F518_SUF ~ zpalter + az + S1_fct, data = etb18_kap7)
summary(m2)

## quadratische Terme 
m3 <- lm(F518_SUF ~ zpalter + I(zpalter^2)  + az + S1_fct, data = etb18_kap7)
## Interaktionen
m4 <- lm(F518_SUF ~ zpalter + az * S1_fct, data = etb18_kap7)


## Darstellen ----
library(modelsummary)

modelsummary(list("Modell1"=m1,m2,m3,m4))
modelplot(list(m1,m2,m3,m4), coef_omit = "Intercept")

# log-Reg ----- 
log1 <- glm(mig_stat ~ zpalter, family = "binomial", data = etb18_kap7) # für log Regression immer family =
summary(log1)
library(marginaleffects)
avg_slopes(log1) # average marginal effects mit avg_slopes


# wichtig: ohne family = binomial entspricht glm einem lm()-Modell!
glm(mig_stat ~ zpalter, data = etb18_kap7) 
 lm(mig_stat ~ zpalter, data = etb18_kap7) 
