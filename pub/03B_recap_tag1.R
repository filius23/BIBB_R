studs <-  c(19173,5333,15643)

# data frames -----
dat1 <- data.frame(studs = c(19173,5333,15643), 
                   profs = c(322,67,210),
                   nach1970 =  c(TRUE,FALSE,TRUE),
                   gegr  = c(1971,1830,1973),
                   uni = c("Uni Bremen","Uni Vechta", "Uni Oldenburg"),
                   uni_fct = factor(c("Uni Bremen","Uni Vechta", "Uni Oldenburg"), 
                                    levels = c("Uni Oldenburg", "Uni Bremen", "Uni Vechta"))) 
dat1    # zeigt den kompletten Datensatz an


# numeric
class(dat1$studs)
# double integer 

# character
class(dat1$uni)
dat1$uni

# logical
class(dat1$nach1970)
dat1$nach1970

# factor
class(dat1$uni_fct)

dat1$uni_fct




# Datensatz einlesen ----
library(haven) # datenimport für stata-datensätze
library(tidyverse) # tidyverse
etb18 <- read_dta("./data/BIBBBAuA_2018_suf1.0.dta")

# auszählungen
etb18 %>% 
  count(m1202)

tabelle <- etb18 %>% 
  count(m1202)
class(tabelle)

etb18 %>% 
  count(m1202) %>% 
  mutate(pct = prop.table(n)*100,
         Cum = cumsum(pct)) 

# lage & konzentrationsmaße
library(ineq)

etb18$F518_SUF[etb18$F518_SUF >= 99998] <- NA  # missings setzen

etb18 %>% summarise(Minimum = min(F518_SUF,na.rm = T),
                    Median = median(F518_SUF,na.rm = T),
                    Mittelwert = mean(F518_SUF,na.rm = T),
                    Maximum = max(F518_SUF,na.rm = T),
                    Gini = Gini(F518_SUF))

