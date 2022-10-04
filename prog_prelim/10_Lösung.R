# ------------------- #
# Kapitel 7: logistische Regression / AME
# Lösung
# ------------------- #


library(tidyverse)
library(marginaleffects)


## Daten einlesen ----------
etb_ue10 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                            col_select = c("F1605e","m1202")) %>% 
  filter(F1605e < 4, !is.na(F1605e), m1202 %in% 1:4) %>% 
  mutate(fam_beruf = 2-F1605e,
         m1202 = factor(m1202))

## auszählen zur Kontrolle ----------
etb_ue10 %>% count(F1605e,fam_beruf,m1202)

## Modell schätzen ----------
log_mod1 <- glm(fam_beruf ~  m1202,family = "binomial" ,etb_ue10)
summary(log_mod1)

## AME berechnen ----------
mfx <- marginaleffects(log_mod1)

##AME anzeigen lassen ----------
summary(mfx)

## Bonus: plot ------------
summary(mfx) %>% 
  data.frame()


summary(mfx) %>% 
  data.frame() %>% 
  ggplot(aes(x = estimate, y = contrast)) + 
  geom_vline(aes(xintercept = 0 ), linetype = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax= conf.high), color = "lightskyblue4",height = .1) + 
  geom_point(color = "lightskyblue4") +
  scale_y_discrete(breaks = c("2 - 1", "3 - 1", "4 - 1"),
                   labels= c("duale o. schulische vs. keine",
                             "Aufstiegsfort. vs. keine",
                             "FH/Uni vs. keine"))



