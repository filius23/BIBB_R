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
         #,levels = 1:4,labels = c("Ohne","duale","Aufst","FH/Uni")) )

## auszählen zur Kontrolle ----------
etb_ue10 %>% count(F1605e,fam_beruf,m1202)

## Modell schätzen ----------
log_mod1 <- glm(fam_beruf ~  m1202,family = "binomial" ,data = etb_ue10)
summary(log_mod1)

## AME berechnen ----------
library(marginaleffects)
avg_slopes(log_mod1)
ame_mod1 <- avg_slopes(log_mod1)

##AME anzeigen lassen ----------
ame_mod1

## Bonus: plot ------------
ame_mod1 %>% 
  data.frame() %>% 
  ggplot(aes(x = estimate, y = contrast)) + 
  geom_vline(aes(xintercept = 0 ), linetype = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax= conf.high), color = "orange",height = .1) + 
  geom_point(color = "lightskyblue4") +
  scale_y_discrete(breaks = c("2 - 1", "3 - 1", "4 - 1"),
                   labels= c("duale o. schulische vs. keine",
                             "Aufstiegsfort. vs. keine",
                             "FH/Uni vs. keine")) +
  labs(x = "Average marginal effect")



# adjusted predictions ----
# margins, at(....)
library(marginaleffects)
pred_df <- 
  predictions(log_mod1, 
              newdata = data.frame(m1202  = c("1", "2", "3", "4")), # einzusetzende Werte
              type = "response" # vorhergesagte Wkt als Einheit (statt logits)
              ) 


head(data.frame(pred_df))

pred_df %>% 
  data.frame() %>% 
  ggplot(aes(x = estimate , y = rowid)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax= conf.high), color = "orange",height = .1) + 
  geom_point(color = "slateblue") +
  scale_y_continuous(breaks = c(1:4),
                   labels= c("keine",
                             "duale o. schulische",
                             "Aufstiegsfort.",
                             "FH/Uni")) +
  theme_grey(base_size = 25)

