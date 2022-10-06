
library(tidyverse)

etb <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                       n_max = 1000,
                       col_select = c("F518_SUF","S1","gkpol",
                                      "m1202","az","zpalter",
                                      "F1450_01","F1450_02",
                                      "F1450_03","F1450_04","F1450_05","F1450_06")) 


# NA setzen -----
attributes(etb$m1202)
etb$m1202[etb$m1202 == -1]  <- NA

attributes(etb$zpalter)
etb$zpalter[etb$zpalter>99]  <- NA

# Visualisierung ....
etb %>% 
  ggplot(aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                     breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(color = "Geschlecht", y = "Arbeitszeit/Woche",
       x = "Alter",
       title = "Arbeitszeit und Alter",
       subtitle = "Nach Geschlecht",
       caption = "Quelle: ETB 2018"
  ) 

# count -----
etb %>% 
  filter(!is.na(m1202)) %>% 
  count(m1202,S1) %>% 
  mutate(pct= prop.table(n)*100) 
# summarise -----
etb %>% 
  group_by(S1) %>% 
  summarise(mean = mean(F1450_01,na.rm=T))
# across ----
etb %>% 
  group_by(S1) %>% 
  summarise(across(matches("F1450_(01|02|05)"),
                   list(mean = ~mean(.x,na.rm=T),
                        N = ~n(),
                        nNA = ~sum(!is.na(.x))),
                   .names = "{.fn}.{.col}") )

