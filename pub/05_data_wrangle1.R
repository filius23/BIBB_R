# ------------------- #
# Kapitel 5: labels
# Lösung
# ------------------- #

library(tidyverse)
etb18_kap5 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                               col_select = c("intnr", "S1", "m1202", "Bula"))


# Labels ------
## labels in View() und count() -----
View(etb18_kap5)
etb18_kap5 %>% count(S1)

attributes(etb18_kap5$S1)

# labels machen in R oft Probleme -----
library(ggplot2)
library(ggeasy)

etb18_kap5 %>% 
  count(S1_num) %>% 
  ggplot(.,aes(x=S1_num,y=n, color = S1_num)) +
  geom_col()+ 
  easy_labs()

# Labeln mit factor ----
tab_dat1 <- etb18_kap5 %>% count(m1202)
tab_dat1

## character ------
tab_dat1$m1202_chr <- c("k.A.","ohne Abs.", "dual/schul.", "Aufstiegsfortb.", "FH/Uni")
tab_dat1

## factor -----
tab_dat1$educ_fct <- factor(tab_dat1$educ, 
                        levels = c(1,2,3,4), 
                        labels = c("ohne Abs.", "dual/schul.", "Aufstiegsfortb.", "FH/Uni"))

# m1202_chr sieht gleich aus wie educ_chr
tab_dat1

levels(tab_dat1$m1202_chr) # levels macht den Unterschied -> es gibt eine Reihenfolge

tab_dat1$m1202_chr2 <- factor(tab_dat1$educ, levels = c(4,3,2,1), 
                   labels = c("FH/Uni","Aufstiegsfortb.","dual/schul.", "ohne Abs."))
levels(tab_dat1$m1202_chr2)
tab_dat1

# factor-Reihenfolge wird auch in plots beachtet:
tab_dat1 %>% 
  ggplot(data = ., aes(x = m1202_chr, y = n)) +
  geom_col(position=position_dodge(), fill = "steelblue4") 

tab_dat1 %>% 
  ggplot(data = ., aes(x = educ_chr, y = n)) +
  geom_col(position=position_dodge(), fill = "mediumturquoise")

# left_join(): labels selbst ranspielen ------------
tab_ausb2 <- etb18 %>% count(m1202)
tab_ausb2

## label data frame erstellen -----
lab_df <- data.frame(m1202=1:4)
lab_df
lab_df$m1202_lab <- factor(lab_df$m1202,levels = 1:4,
                           labels = c("ohne Abs.", "dual/schul.", "Aufstiegsfortb.", "FH/Uni"))
lab_df

## ranspielen mit left_join() ----
tab_ausb2 %>% 
  left_join(lab_df,by = "m1202")

## plotten ----
tab_ausb2 %>% 
  left_join(lab_df,by = "m1202") %>% 
  ggplot(data = ., aes(x = m1202_lab, y = n)) +
  geom_col(position=position_dodge(), fill = "turquoise3")
