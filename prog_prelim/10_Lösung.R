library(tidyverse)
library(marginaleffects)



etb_ue10 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                            col_select = c("F1605e","m1202")) %>% 
  filter(F1605e < 4, !is.na(F1605e), m1202 %in% 1:4) %>% 
  mutate(fam_beruf = 2-F1605e,
         m1202 = factor(m1202))


etb_ue10 %>% count(F1605e,fam_beruf,m1202)

log_mod1 <- glm(fam_beruf ~  gender,family = "binomial" ,etb_ue10)
summary(log_mod1)
mfx <- marginaleffects(log_mod1)
summary(mfx)





