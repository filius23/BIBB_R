# ------------------- #
# Kapitel 5: labels
# Lösung
# ------------------- #

library(tidyverse)
etb18_small <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                               col_select = c("intnr", "S1", "m1202", "Bula"))

# Labels ------
## labels in View() und count() -----
View(etb18_small)
etb18_small %>% count(S1)

attributes(etb18_small$S1)

## labels sind sehr flüchtige Eigenschaften -----
etb18_small$S1_num <- as.numeric(etb18_small$S1)
head(etb18_small)
attributes(etb18_small$S1_num)

## attributes kopieren ----
attributes(etb18_small$S1_num) <- attributes(etb18_small$S1)
attributes(etb18_small$S1_num)
class(etb18_small$S1_num)
etb18_small %>% count(S1_num)


# labels machen in R oft Probleme -----
library(ggplot2)
library(ggeasy)

etb18_small %>% 
  count(S1_num) %>% 
  ggplot(.,aes(x=S1_num,y=n, color = S1_num)) +
  geom_col()+ 
  easy_labs()

# labeln mit labelled ------
 # install.packages("labelled")
library(labelled)

etb18_small$S1_num2 <- as.numeric(etb18_small$S1)
attributes(etb18_small$S1_num2)
val_labels(etb18_small$S1_num2) <- c("Mann"=1,"Frau"=2)
attributes(etb18_small$S1_num2)
etb18_small %>% count(S1_num2)

# attributes-label sind in stata sichtbar ----
# etb18_small %>%
#  select(S1_num2) %>%
#  haven::write_dta(.,path = "./data/etb18_small.dta")

# Labeln mit factor ----
dat1 <- data.frame(educ=c(3, 3, -1, 1, 4, 3, 1, 4, 2))
tab_dat1 <- dat1 %>% count(educ)
tab_dat1

## character ------
tab_dat1$educ_chr <- c("k.A.","ohne Abs.", "dual/schul.", "Aufstiegsfortb.", "FH/Uni")
tab_dat1

## factor -----
tab_dat1$educ_fct <- factor(tab_dat1$educ, 
                        levels = c(1,2,3,4), 
                        labels = c("ohne Abs.", "dual/schul.", "Aufstiegsfortb.", "FH/Uni"))

# educ_fct sieht gleich aus wie educ_chr
tab_dat1

levels(tab_dat1$educ_fct) # levels macht den Unterschied -> es gibt eine Reihenfolge

tab_dat1$educ_fct2 <- factor(tab_dat1$educ, levels = c(4,3,2,1), 
                   labels = c("FH/Uni","Aufstiegsfortb.","dual/schul.", "ohne Abs."))
levels(tab_dat1$educ_fct2)
tab_dat1

# factor-Reihenfolge wird auch in plots beachtet:
tab_dat1 %>% 
  ggplot(data = ., aes(x = educ_fct, y = n)) +
  geom_col(position=position_dodge(), fill = "steelblue4") 

tab_dat1 %>% 
  ggplot(data = ., aes(x = educ_chr, y = n)) +
  geom_col(position=position_dodge(), fill = "mediumturquoise")


# as_factor() ------------
library(haven)
tab_ausb <- etb18_small %>% filter(m1202 != -1) %>% count(m1202)
tab_ausb
tab_ausb$m1202_fct <- as_factor(tab_ausb$m1202)
tab_ausb
tab_ausb$m1202 <- as.numeric(tab_ausb$m1202) # labels aus m1202 löschen (numeric ist nicht gelabelt)

tab_ausb %>% 
  ggplot(data = ., aes(x = m1202_fct, y = n)) +
  geom_col(position=position_dodge(), fill = "sienna1")


# factor Variablen bearbeiten -----
tab_ausb
tab_ausb$m1202_fct[2] <- "duale/schul. Berufsausb."
tab_ausb

# hier hilft forcats
library(forcats) ## nicht nötig wenn bereits tidyverse geladen
levels(tab_ausb$m1202_fct)

## fct_expand ----
tab_ausb$m1202_fct <- fct_expand(tab_ausb$m1202_fct,"duale/schul. Berufsausb.") # neues level hinzufügen
levels(tab_ausb$m1202_fct)
tab_ausb$m1202_fct <- fct_relevel(tab_ausb$m1202_fct,"duale/schul. Berufsausb.",after = 1)
levels(tab_ausb$m1202_fct)

tab_ausb
tab_ausb$m1202_fct[2] <- "duale/schul. Berufsausb."
tab_ausb

levels(tab_ausb$m1202_fct)
length(levels(tab_ausb$m1202_fct))

## überzähligr factor levels droppen:
tab_ausb$m1202_fct <- fct_drop(tab_ausb$m1202_fct)
levels(tab_ausb$m1202_fct)
length(levels(tab_ausb$m1202_fct))

## fct_recode -----
tab_ausb$m1202_fct2 <- fct_recode(tab_ausb$m1202_fct,
                               Aufstiegsfortbildung = "Aufstiegsfortbildung (Meister, Techniker, kfm. AFB u.ä.)",
                               "Uni/FH"  = "Fachhochschule, Universität/ geh., höhere Beamte",
                               `kein Abs.` = "Ohne Berufsabschluss" # bei Leerzeichen `` um die Wörter
                               )
tab_ausb

## fct_reorder -----
tab_ausb$m1202_fct3 <- fct_reorder(tab_ausb$m1202_fct2,tab_ausb$n)
levels(tab_ausb$m1202_fct2)
levels(tab_ausb$m1202_fct3)

tab_ausb %>% 
  ggplot(data = ., aes(x = m1202_fct2, y = n)) +
  geom_col(position=position_dodge(), fill = "steelblue")

tab_ausb %>% 
  ggplot(data = ., aes(x = m1202_fct3, y = n)) +
  geom_col(position=position_dodge(), fill = "turquoise3")

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
