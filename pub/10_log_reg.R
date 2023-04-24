# ------------------- #
# Kapitel 10: logistische Regression / AME
# ------------------- #
library(tidyverse)

# Daten einlesen -----
m_etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                           col_select = c("S1","F605","F518_SUF","m1202","zpalter","Bula")) %>% 
  filter(F605 < 9, F518_SUF < 99998, zpalter < 100, S1==1) %>% 
  mutate(inc100 = F518_SUF/100,
         outside = 2-F605)

## Überblick 
attributes(m_etb18$F605)
m_etb18 %>% count(outside,F605)
summary(m_etb18$inc100)

# logistisches Regressionsmodell ------
# lm()
m2 <- glm(outside ~ inc100, family = "binomial", data = m_etb18)
summary(m2)

#  family = "binomial" ist essentiell! 
 m_glm <- glm(outside ~ inc100, family = "binomial", data = m_etb18)
 
 m_glm_ohne <- glm(outside ~ inc100, data = m_etb18)
 m_glm_gaus <- glm(outside ~ inc100, family = gaussian(), data = m_etb18)
 m_lm <- lm(outside ~ inc100, data = m_etb18)
 
 
 modelsummary::modelsummary(list("glm binomial"=m_glm,
                                 "glm ohne"=m_glm_ohne,
                                 "glm gaussian"=m_glm_gaus,
                                 "lm" = m_lm),fmt = "%5f",gof_omit = "IC|Log|F|R")
    
 
# average marginal effects --------

# install.packages("marginaleffects") # nur einmal nötig
library(marginaleffects)
avg_slopes(m2) ##  margins, dydx(inc100)

# vorhergesagte Werte mit predictions aus marginaleffects
predictions(m2, 
            newdata = data.frame(inc100  = 1:5), # einzusetzende Werte
            type = "response" # vorhergesagte Wkt als Einheit (statt logits)
            ) 
# margins, at(inc100 = (1(1)5) )


predictions(m2, 
            newdata = data.frame(inc100  = 1:75), # einzusetzende Werte
            type = "response" # vorhergesagte Wkt als Einheit (statt logits)
            ) %>% 
  data.frame() %>% 
  ggplot(aes(y = estimate , x = inc100)) + 
  geom_errorbar(aes(ymin = conf.low, ymax= conf.high), color = "slateblue",width = .1) + # konfidenzintervalle
  geom_point(color = "slateblue") + # punktschätzer
  theme_minimal()

# glm FE --------
library(fixest)
m_etb18 <-
  m_etb18 %>%
  mutate(m1202_fct = factor(m1202,levels = c(-1,1:4),
                            labels = names(attributes(m1202)$labels) %>%
                              substr(.,1,5) %>% str_squish()),
         m1202_fct = fct_relevel(m1202_fct,"Ohne"))

# fe-logit Modell mit feglm
feglm(outside ~ m1202_fct*zpalter |Bula, data = m_etb18, family = binomial)


fe_log1 <- feglm(outside ~ m1202_fct + zpalter |Bula , data = m_etb18, family = binomial)

# AMEs mit avg-slopes
avg_slopes(fe_log1, variables = "m1202_fct")
avg_slopes(fe_log1, variables = "zpalter")

avg_slopes(fe_log1, variables = c("zpalter","m1202_fct")) # :-( -> vermutlich bug der neuesten Version





