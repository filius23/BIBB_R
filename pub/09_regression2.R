# ------------------- #
# Kapitel 9: Regreession - Erweiterungen
# ------------------- #

library(tidyverse)
library(modelsummary)
library(marginaleffects)


dat1 <- data.frame(id   = 1:8,
                   var1 = c(2,1,2,5,7, 8, 9,5),
                   var2 = c(2,2,1,9,7, 4,25,3),
                   educ = c(3,1,2,2,1, 3, 2,-1),
                   gend = c(2,1,1,2,1,2,1,2),
                   x    = c(2,1,2,4,1,NA,NA,NA) )
dat1$ed_fct <- factor(dat1$educ, levels = 1:3,
                        labels = c("basic","medium","high"))
dat1

# Modelle vergleichbar machen: gleiche fallzahlen -------
m1 <- lm(var2~ var1, data = dat1)  
m4 <- lm(var2 ~ ed_fct  + var1, dat1)
modelsummary(list("m1"=m1,"m4"=m4),gof_omit = "IC|RM|Log|F")
# unterschiedliche N in m1 und m4 -> missings

dat1


analytical_sample <- dat1 %>% filter(!is.na(ed_fct), !is.na(var2))

## vollständige Zeilen markieren mit complete.cases() ------
dat1 %>% select(var1,var2) %>% complete.cases(.) 
## variablen für m1
dat1$compl_m1 <- dat1 %>% select(var1,var2) %>% complete.cases(.)  # indikator as variable ablegen
dat1

# modell m4
dat1$compl_m4 <- dat1 %>% select(var1,var2,ed_fct) %>% complete.cases(.)
dat1

dat1 %>% filter(compl_m1 == T & compl_m4 == F) 
# jetzt modell m1 nur mit Zeilen berechnen, die auch für m4 zur Verfügung stehen:
m1_m4vars <- lm(var2 ~ var1     , data = filter(dat1,compl_m4 == T))
# Vergleich mit modelsummary 
modelsummary(list("m1"=m1,"m1 mit m4vars"=m1_m4vars,"m4"=m4),gof_omit = "IC|RM|Log|F")


# Interaktionsterme ---------
# Interaktionen  können mit * erstellt werden
    
    dat1$g_fct <- factor(dat1$gend,levels = 1:2,labels = c("women","men"))

m5 <- lm(var2 ~ var1 * g_fct, dat1)
summary(m5)

## install.packages("ggeffects")
library(ggeffects)
# in ggeffects die variablen und ggf. in [] die einzusetzenden Werte angeben:
ggpredict(m5, terms = c("var1","g_fct[women,men]")) %>% plot()
# oder nebeneinander:
ggpredict(m5, terms = c("var1","g_fct[women,men]")) %>% plot(facet=TRUE)
# mit ggplot2-Befehlen anpassen:
ggpredict(m5, terms = c("var1","g_fct[women,men]")) %>% plot() + 
  scale_color_viridis_d(breaks = c("women","men"),labels=c("Frauen","Männer")) +
  scale_fill_viridis_d(breaks = c("women","men"),labels=c("Frauen","Männer")) +
  labs(title = "Vorhergesagte Werte für var2",
       color = "Gender",
       x = "Werte für var1",
       y = "Vorhergesagte Werte für var1")

# Quadratische Terme/Polynome -------
# ...mit I(..^2) erstellbar:
m6 <- lm(var2 ~ var1 + I(var1^2), dat1 %>% filter(id != 7))
summary(m6)
# wieder hilft ggpredict:
ggpredict(m6, terms = c("var1")) %>% plot()

# geht auch mehr polynomen:
m7 <- lm(var2 ~ var1 + I(var1^2) + I(var1^3), dat1 %>% filter(id != 7))
summary(m7)
ggpredict(m7, terms = c("var1")) %>% plot()


# gewichtetes Regressionsmodell -------------
library(survey)
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",col_select = c("F518_SUF","zpalter","gew2018","intnr","Bula")) 

etb18_k9 <- etb18 %>% filter(F518_SUF < 99998, zpalter < 100)
modx <- lm(F518_SUF ~ zpalter + I(zpalter^2),data=etb18_k9)

etb18_weighted <- svydesign(id      = ~intnr,
                            weights = ~gew2018,
                            data    = etb18_k9)
# family = gaussian() bekommen wir ein lineares Regressionsmodell, wie bei lm() - mit gewichtet
survey_modx <- svyglm(F518_SUF ~ zpalter + I(zpalter^2), 
                    family = gaussian(), data = etb18,design = etb18_weighted)

modelsummary(list("lm()"=modx,"svyglm()"= survey_modx),gof_omit = "RM|IC|Log")

# Robuste Standardfehler ------
## beispielmodel---
mod1 <- lm(F518_SUF ~ zpalter + I(zpalter^2) , data = etb18_k9)

## standardfehler können in modelsummary angepasst/"korrigiert" werden:
library(modelsummary)
modelsummary(list(mod1,mod1,mod1,mod1),
             vcov = c("classical","HC","HC2","stata"),gof_omit = "RM|IC|Log")

# Fixed Effects ----------------
# am einfachsten mit fixest:
# install.packages("fixest")
library(fixest)
fe_mod1 <- feols(F518_SUF ~ zpalter + I(zpalter^2) | Bula, data = etb18_k9)
fe_mod1

summary(fe_mod1, se = 'standard')
summary(fe_mod1, cluster = ~Bula)

modelsummary(list(fe_mod1),gof_omit = "R|IC|Log|F")

# Mehrebenenmodelle ---------------
library(lme4)
ml_m3 <- lmer(F518_SUF ~ zpalter + I(zpalter^2) + ( 1 | Bula), data=etb18_k9)

modelsummary(list(ml_m3),gof_omit = "R|IC|Log|F")
