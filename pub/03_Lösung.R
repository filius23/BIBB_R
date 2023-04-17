# ------------------- #
# Kapitel 3: Tabellen
# Lösung
# ------------------- #
library(tidyverse)
library(haven)
etb18 <- haven::read_dta(file = "./data/BIBBBAuA_2018_suf1.0.dta",
                         col_select = c("gkpol","S1","S3","zpalter") # optional: nur nötige Variablen einlesen
                         )

#  Übung 1------------

# Lassen Sie sich eine Tabelle mit den absoluten Häufigkeiten anzeigen, 
library(tidyverse)

table(etb18$gkpol)
etb18$gkpol %>% table()

xtabs(~gkpol,etb18)


etb18 %>% count(gkpol)

# Lassen Sie sich der relativen Häufigkeiten (Anteile) ausgeben (nutzen sie entweder table() oder xtabs())
xtabs(~gkpol,etb18) %>% prop.table(.)
table(etb18$gkpol) %>% prop.table(.)

# Erstellen Sie eine Kontingenztabelle, indem Sie neben gkpol auch das Geschlecht S1 (2 = Frauen, 1 = Männer) mit einbeziehen
xtabs(~gkpol+S1,etb18)
table(etb18$gkpol, etb18$S1)

etb18 %>% count(gkpol,S1)

# Wie viel Prozent der Befragten sind Frauen, die in einer Gemeinde mit unter 2000 Einwohnern leben?
# Berechnen Sie die relativen Häufigkeiten.
xtabs(~gkpol+S1,etb18) %>% prop.table()

#  Übung 2-------------

# Wir interessieren uns für die Variable S3 
# Erstellen Sie mit Hilfe von count() eine Tabelle mit absoluten, relativen und kumulierten relativen Häufigkeiten.
etb18 %>% count(S3)
countS3 <- etb18 %>% count(S3)
countS3

# 99 mit NA überschreiben
etb18$S3[etb18$S3==99] <- NA
etb18 %>% count(S3)
table(etb18$S3)

countS3


# Übung 3 -------


# Erstellen Sie eine vollständige Häufigkeitstabelle für die Variable `gkpol` und das Geschlecht (`S1`)

# Verwenden Sie die Befehle aus Übung 2- was müssen Sie anpassen, 
# um die Tabelle für `gkpol`
etb18 %>% 
  count(gkpol) %>% 
  mutate(pct = prop.table(n)*100,
         cum_pct = cumsum(pct))

# Erweitern Sie jetzt die Auszählung um `S1`
etb18 %>% 
  count(gkpol, S1) %>% 
  mutate(pct = prop.table(n)*100,
         cum_pct = cumsum(pct))

# Berechnen Sie die Anteile von `gkpol` innerhalb von Männern und Frauen.

etb18 %>% 
  count(gkpol, S1) %>% 
  group_by(S1) %>% 
  mutate(pct = prop.table(n)*100,
         cum_pct = cumsum(pct)) 

etb18 %>% 
  count(S1,gkpol) %>% 
  group_by(S1) %>% 
  mutate(pct = prop.table(n)*100,
         cum_pct = cumsum(pct)) 


etb18 %>% 
  count(gkpol, S1) %>% 
  group_by(S1) %>% 
  mutate(pct = prop.table(n)*100,
         cum_pct = cumsum(pct)) %>% 
  arrange(S1,gkpol)

# Wie viel Prozent der Frauen leben in einer Gemeinde mit unter 2000 Einwohnern?  
etb18 %>% 
  count(gkpol, S1) %>% 
  group_by(S1) %>% 
  mutate(pct = prop.table(n)*100,
         cum_pct = cumsum(pct)) %>% 
  filter(S1 == 2, gkpol == 1)

# Übung 4 -------
library(tidyverse)
# Beschreiben Sie das Alter der Befragten (zpalter) mit summary 
# erstellen Sie selbst einen Überblick mit Hilfe von summarise(), der einen Vergleich des Befragtenalters nach Gemeindegrößen erlaubt.
summary(etb18$zpalter)

# Überschreiben Sie zunächst die Missings mit NA:
attributes(etb18$zpalter)
# etb18$zpalter[etb18$zpalter== 9999] <- NA
etb18$zpalter[etb18$zpalter>100] <- NA
  

# Erstellen Sie einen Überblick mit summary()
summary(etb18$zpalter)
# Erstellen Sie einen Überblick mit dem Minimum, Median, arith. Mittel, Varianz und Maximum der Alterswerte mit Hilfe von summarise()
etb18 %>% 
  summarise(
            min    = min(zpalter, na.rm = T),
            mean   = mean(zpalter, na.rm = T),
            median = median(zpalter, na.rm = T),
            max    = max(zpalter, na.rm = T),
            var    = var(zpalter, na.rm = T)
          )

# Erweitern Sie diesen Überblick dann so, dass sie einen Vergleich der Kennzahlen für die verschieden gkpol-Kategorien ausgegeben bekommen.
etb18 %>%
  group_by(gkpol) %>% ## entscheidend: group_by()
  summarise(
    min    = min(zpalter, na.rm = T),
    mean   = mean(zpalter, na.rm = T),
    median = median(zpalter, na.rm = T),
    max    = max(zpalter, na.rm = T),
    var    = var(zpalter, na.rm = T)
  )

# möglichst ohne pipe-----

etb18_grouped <-  group_by(.data = etb18, gkpol)
etb18_grouped
class(etb18_grouped)


summarise(.data = etb18_grouped,
    min    = min(zpalter, na.rm = T),
    mean   = mean(zpalter, na.rm = T),
    median = median(zpalter, na.rm = T),
    max    = max(zpalter, na.rm = T),
    var    = var(zpalter, na.rm = T)
  )
