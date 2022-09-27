# ------------------- #
# Kapitel 3: Tabellen 
# ------------------- #

library(tidyverse)
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")


# table & xtabs -------------
table(etb18$m1202)
xtabs(~m1202,data=etb18)

## labels anzeigen -----
## install.packages("labelled") # nur einmal nötig
library(labelled)
val_labels(etb18$m1202)


## count() zeigt labels: ------
etb18 %>% count(m1202)


# count/table als Objekt --------
t1 <- xtabs(~m1202,etb18)
t2 <- etb18 %>% count(m1202)

## unterschiedliche Objekte: ------
class(t1)
class(t2)

# andere Werte ------------
# relative Häufigkeiten
xtabs(~m1202,data=etb18) %>% prop.table(.) 

# kumulierte Werte
xtabs(~m1202,data=etb18) %>% cumsum(.)

# kumulierte relative Werte
xtabs(~m1202,data=etb18) %>% prop.table() %>% cumsum()

# Kreuztabellen 
table(etb18$S1, etb18$m1202)
xtabs(~S1+m1202, data = etb18)

## Übung ------

# Missings: NA ----------
etb18$m1202[etb18$m1202 == -1] # nur m1202 = -1 aufrufen

# mit NA überschreiben:
etb18$m1202[etb18$m1202 == -1]  <- NA
etb18 %>% select(m1202) %>% slice(70:80)
etb18 %>% select(m1202) %>% filter(is.na(m1202))



# xtabs() übergeht NA:
xtabs(~m1202,data=etb18)
xtabs(~m1202,data=etb18) %>% prop.table() %>% cumsum()

xtabs(~m1202,data=etb18,addNA = T)

#  count() zählt auch NA aus:
etb18 %>% count(m1202)
etb18 %>% filter(!is.na(m1202)) %>% count(m1202)


## Übung ------


# mehrere Kennzahlen in einer Tabelle -----


etb18 %>% 
   count(m1202) # ausgangsbefehl


# weitere Werte als Variablen mit mutate() anfügen
etb18 %>% 
   count(m1202) %>% 
   mutate(pct= prop.table(n)*100) # erweitert um pct

# können auch noch mehr Variablen anfügen
etb18 %>% 
   count(m1202) %>% 
   mutate(pct= prop.table(n)*100,
          Cum = cumsum(pct)) 

# NAs ausschließen mit filter()
etb18 %>% 
  filter(!is.na(m1202)) %>% 
   count(m1202) %>% 
   mutate(pct= prop.table(n)*100,
          Cum = cumsum(pct)) 

## Zwei Variablen ----
etb18 %>% 
  filter(!is.na(m1202)) %>% 
   count(m1202,S1)

## relative Anteile -----
etb18 %>% 
  filter(!is.na(m1202)) %>% 
  count(m1202,S1) %>% 
  mutate(pct= prop.table(n)*100) 

## relative Anteile nach Gruppe -----
etb18 %>% 
   filter(!is.na(m1202)) %>% 
   count(m1202,S1) %>% 
   group_by(S1) %>% 
   mutate(pct_gender = prop.table(n)) 

# bestimmte Werte nachsehen
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

# print() bei langen tibbles:
etb18 %>% count(Bula,S1) # wird abgeschnitten
etb18 %>% count(Bula,S1) %>% print(n=Inf) # alle Werte werden gezeigt
etb18 %>% count(Bula,S1) %>% print(n=2) # alle Werte werden gezeigt
etb18 %>% count(Bula,S1) %>% print(n=12) # alle Werte werden gezeigt


## Übung ------


# Lage- & Konzentrationsmaße -------

summary(etb18$F518_SUF)
etb18$F518_SUF[etb18$F518_SUF %in% 99998:99999] <- NA # missings überschreiben
summary(etb18$F518_SUF)

mean(etb18$F518_SUF)
mean(etb18$F518_SUF,na.rm = T)
quantile(etb18$F518_SUF,probs = .4, na.rm = T)

## install.packages("ineq") # einmal installieren

library(ineq) # ineq laden
Gini(etb18$F518_SUF)

## summarise -----
etb18 %>% summarise(Minimum = min(F518_SUF,na.rm = T),
                    Median = median(F518_SUF,na.rm = T),
                    Mittelwert = mean(F518_SUF,na.rm = T),
                    Maximum = max(F518_SUF,na.rm = T),
                    Gini = Gini(F518_SUF))

## group_by() ------
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

