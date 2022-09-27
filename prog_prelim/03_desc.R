if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 
if(Sys.getenv("USERNAME") == "filse" ) path <- "D:/oCloud/RFS/"
knitr::opts_chunk$set(collapse = F)
library(tidyverse)
 a14 <- readr::read_delim(paste0(path,"allbus_kumuliert.csv"), delim = ";", col_types = cols(.default = col_double())) %>% 
   filter(year == 2014) # für Gini Illustration
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")
library(Statamarkdown)

## install.packages("haven") # falls nicht schon installiert

## library(haven) # datenimport für stata-datensätze
## library(tidyverse) # tidyverse
## etb18 <- read_dta("./data/BIBBBAuA_2018_suf1.0.dta")

table(etb18$m1202)

xtabs(~m1202,data=etb18)

## install.packages("labelled") # nur einmal nötig
## library(labelled)
## val_labels(etb18$m1202)

library(labelled)
val_labels(etb18$m1202)
t2 <- xtabs(~m1202,data=etb18)

etb18 %>% count(m1202)

t1 <- xtabs(~m1202,etb18)
t2 <- etb18 %>% count(m1202)

class(t1)
class(t2)

xtabs(~m1202,data=etb18) %>% prop.table(.) 

xtabs(~m1202,data=etb18) %>% cumsum(.)
ct2 <- xtabs(~m1202,data=etb18) %>% cumsum(.)

xtabs(~m1202,data=etb18) %>% prop.table() %>% cumsum()
t2x <- cumsum(prop.table(xtabs(~m1202,data=etb18)))

table(etb18$S1, etb18$m1202)
xtabs(~S1+m1202, data = etb18)

etb18$m1202[etb18$m1202 == -1] # nur m1202 = -1 aufrufen

etb18$m1202[etb18$m1202 == -1]  <- NA

xtabs(~m1202,data=etb18)

xtabs(~m1202,data=etb18,addNA = T)

etb18 %>% count(m1202)

etb18 %>% filter(!is.na(m1202)) %>% count(m1202)

## set linesize 80

## qui use "D:\Datenspeicher\BIBB_BAuA/BIBBBAuA_2018_suf1.0.dta", clear

## qui mvdecode m1202 F100_kldb2010_BOF F1609_kldb2010_BOF F1610_kldb2010_BOF, mv(-1)

## qui mvdecode F100_wib1, mv(-4/-1)

## tab m1202


knitr::include_graphics("./pic/103_mutate.png")


etb18 %>% 
   count(m1202) # ausgangsbefehl

etb18 %>% 
   count(m1202) %>% 
   mutate(pct= prop.table(n)*100) # erweitert um pct

etb18 %>% 
   count(m1202) %>% 
   mutate(pct= prop.table(n)*100,
          Cum = cumsum(pct)) 

etb18 %>% 
  filter(!is.na(m1202)) %>% 
   count(m1202) %>% 
   mutate(pct= prop.table(n)*100,
          Cum = cumsum(pct)) 

etb18 %>% 
  filter(!is.na(m1202)) %>% 
   count(m1202,S1)

etb18 %>% 
  filter(!is.na(m1202)) %>% 
  count(m1202,S1) %>% 
  mutate(pct= prop.table(n)*100) 

etb18 %>% 
   filter(!is.na(m1202)) %>% 
   count(m1202,S1) %>% 
   group_by(S1) %>% 
   mutate(pct_gender = prop.table(n)) 

etb18 %>% 
  filter(!is.na(m1202)) %>% 
   count(m1202,S1) %>% 
   group_by(S1) %>% 
   mutate(pct_gender = prop.table(n)) %>% 
  filter(m1202 == 2)

tab_aus_gender <- 
      etb18 %>% 
        filter(!is.na(m1202)) %>% 
         count(m1202,S1) %>% 
         group_by(S1) %>% 
         mutate(pct_gender = prop.table(n))
class(tab_aus_gender)

tab_aus_gender %>%  filter(m1202 == 3)

## etb18 %>% count(Bula,S1) # wird abgeschnitten
## etb18 %>% count(Bula,S1) %>% print(n=Inf) # alle Werte werden gezeigt

summary(etb18$F518_SUF)

etb18$F518_SUF[etb18$F518_SUF %in% 99998:99999] <- NA # missings überschreiben

summary(etb18$F518_SUF)

mean(etb18$F518_SUF)

mean(etb18$F518_SUF,na.rm = T)

quantile(etb18$F518_SUF,probs = .4, na.rm = T)

## install.packages("ineq") # einmal installieren

library(ineq) # ineq laden
Gini(etb18$F518_SUF)

etb18 %>% summarise(Minimum = min(F518_SUF,na.rm = T),
                    Median = median(F518_SUF,na.rm = T),
                    Mittelwert = mean(F518_SUF,na.rm = T),
                    Maximum = max(F518_SUF,na.rm = T),
                    Gini = Gini(F518_SUF))

nrw_nds <- mean(etb18$F518_SUF[etb18$Bula == 5], na.rm = T) -
  mean(etb18$F518_SUF[etb18$Bula == 3], na.rm = T) # Niedersachsen

etb18 %>% 
  group_by(Bula) %>% 
  summarise(mean_inc = mean(F518_SUF, na.rm = T) )
etb18 %>% 
  group_by(Bula) %>% 
  summarise(mean_inc = mean(F518_SUF, na.rm = T),
            median_inc = median(F518_SUF, na.rm = T))

etb18 %>% 
  filter(Bula %in% c(3,5)) %>% 
  group_by(Bula) %>% 
  summarise(mean_inc = mean(F518_SUF, na.rm = T) )

## etb18 <- read_dta("./data/BIBBBAuA_2018_suf1.0.dta")

library(gt)
etb18 %>% count(gkpol) %>% pull(gkpol) %>% labelled::val_labels() %>% enframe(name = "label") %>% gt() %>% tab_options(  table.font.size = 9)
# %>% kable(.) %>% 
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>% 
#   row_spec(0, color = "white",font_size = 0)

## etb18$zpalter[etb18$zpalter>100] <- NA

round(21.12121123,digits = 3)
round(21.12121123,digits = 5)
round(21.12121123,digits = 0)

xtabs(~S1+m1202, data = etb18) %>% 
  prop.table(.,margin = 1) %>% 
  round(.,3)

t4 <- table(etb18$zpalter)
t4[which(t4 == max(t4))] # Modus
