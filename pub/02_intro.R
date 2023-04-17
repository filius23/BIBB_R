# ------------------- #
# Kapitel 2: data.frame
# ------------------- #

## funktion(objektname1,
##          option1 = sehr_lange_Auswahl_die_sehr_lang_ist,
##          option2 = noch_eine_Auswahl_Option2)

# data.frame -----------------
studs <- c(19173,5333,15643)    # Studierendenzahlen unter "studs" ablegen 
profs       <- c(322,67,210)    # Prof-Zahlen unter "profs" ablegen

# data.frame()
dat1_orig <- data.frame(studs, profs)
dat1_orig

dat1 <- data.frame(studs = c(19173,5333,15643), 
                   profs = c(322,67,210),
                   gegr  = c(1971,1830,1973)) # ohne zwischen-Objekte
dat1    # zeigt den kompletten Datensatz an

dat1$gegr

# data.frame beschreiben ----------------
colnames(dat1) ## Variablen-/Spaltennamen anzeigen
names(dat1) ## Variablen-/Spaltennamen anzeigen

ncol(dat1) ## Anzahl der Spalten/Variablen
nrow(dat1) ## Anzahl der Zeilen/Fälle

# neue Variable -------------
dat1$stu_prof <- dat1$studs/dat1$profs
dat1$stu_prof <- dat1_orig$studs/dat1_orig$profs
dat1$stu_prof <- dat1_orig$studs/dat1$profs

stu_profX <- dat1_orig$studs/dat1$profs

dat1_orig$studs/dat1$profs
dat1
dat1$stu_prof


## dat1 hat also nun eine Spalte mehr:
dat1
ncol(dat1) 

# character Variable ------
dat1$uni <- c("Uni Bremen","Uni Vechta", "Uni Oldenburg")
dat1

View(dat1)


# Variablen Klassen -------
class(dat1$profs)
class(dat1$uni)

is.numeric(dat1$profs)
is.character(dat1$profs)

as.character(dat1$profs) ## die "" zeigen an, dass die Variable als character definiert ist

class(dat1$profs)

dat1$profs <- as.character(dat1$profs)
dat1 # nicht zu sehen
dat1$profs 
class(dat1$profs)

dat1$profs / 2 

as.numeric(dat1$profs)
as.numeric(dat1$profs) / 2

dat1$profs <- as.numeric(dat1$profs)
dat1
as.numeric(dat1$uni)

## Übung 1  -----


# Pakete --------------
# install.packages("Paket") # auf eurem PC nur einmal nötig
# library(Paket) # nach jedem Neustart nötig

## paket::function()

# installiert die komplette Paketsammlung des tidyverse
#install.packages("tidyverse")


library(tidyverse) # nach einmaligem install.packages("tidyverse")

dat1
# slice ------
slice( dat1 , 1)

dat1_erstezeile <- slice( dat1 , 1)

2:3 # ergibt eine Zahlenfolge
slice(dat1,2:3)
slice(dat1,1:3)
c(1,3) # Vektor mit Werten
slice(dat1,c(1,3))

slice(dat1,c(1,3))

dat1[c(1,3),]

# filter ---------
dat1

filter(dat1,uni == "Uni Oldenburg" , studs > 1000)
filter(dat1,uni == "Uni Oldenburg" | studs > 1000) # | bedeutet oder
filter(dat1,uni == "Uni Oldenburg" & studs < 1000)


dat1

ueber_10tsd <- filter(dat1, studs > 10000)
ueber_10tsd
class(ueber_10tsd)

filter(dat1, studs >= 10000)
filter(dat1, studs <= 10000)
filter(dat1,studs > 10000 | profs < 200) # mehr als 10.000 Studierende *oder* weniger als 200 Professuren

filter(dat1, gegr %in% c(1971,1830)) # gegründet 1971 oder 1830

filter(dat1, between(gegr,1830,1971)) # gegründet zwischen 1971 und 1830 (einschließlich)

# Variablentyp logical ------
dat1$studs > 10000 # ist die Studi-Zahl größer 10000?

dat1$gegr %in% c(1971,1830)

dat1$more10k <-  dat1$studs > 10000 # ist die Studi-Zahl größer 10000?

dat1

filter(dat1,more10k)


# Select -----------------
dat1

select(dat1, studs,profs)

select(dat1, 1:3) # Spalte 1-3
select(dat1, !profs) # alles außer profs

dat_ohne_profs <- select(dat1, !profs) 
dat_ohne_profs

dat1_ohne_profs <- select(dat1, !profs) # alles außer profs

## # Spalten eines `data.frame`s auf Basis der `colnames` eines data.frames auswählen möchten:
col_auswahl <- colnames(dat1_orig)
col_auswahl
select(dat1, any_of(col_auswahl) )

# Oder wir wollen alle Variablen, die mit "s" beginnen:
select(dat1,starts_with("s")) # keep s* in stata 
select(dat1,contains("s"))     # keep *s* in stata 
select(dat1,ends_with("s"))     # keep *s in stata

select(dat1,matches("^s")) # gleiches Ergebnis mit regex

select(dat1,matches("s$")) # alle Spalten, die mit s enden
select(dat1,matches("\\$")) # nach wortwörtlichem $ suchen

## Übung ------

# Pipe %>% ------
select(filter(dat1,studs < 10000),uni)

filter(dat1,studs < 10000) %>% select(uni)

dat1 %>% filter(.,studs < 10000) %>% select(.,uni)

dat1 %>% filter(studs < 10000) %>% select(uni)

# sortieren mit arrange()
dat1 %>% arrange(studs)
dat1 %>% arrange(desc(studs)) # absteigend sortieren

dat1 %>% arrange(uni)

# factor -------------
dat1
factor(dat1$uni, levels = c("Uni Oldenburg", "Uni Bremen", "Uni Vechta"))

dat1$uni_fct <- factor(dat1$uni, 
                       levels = c("Uni Oldenburg", "Uni Bremen", "Uni Vechta"))

class(dat1$uni_fct)
class(dat1$uni)
dat1 %>% arrange(uni_fct)
dat1 %>% arrange(desc(uni_fct))
dat1 %>% arrange(desc(uni_fct), gegr, studs) 

# Übung -----

# Datenimport --------------

## getwd()
"D:/Kurse/R-Kurs"
etb <- read.table(file = "./data/BIBBBAuA_2018_small.csv", sep = ";", header = T)
etb
View(etb)

head(etb)
head(etb,n = 11)
tail(etb)
tail(etb,n = 2)

nrow(etb)
ncol(etb)

library(tidyverse)
senior <- etb %>% filter(S2_j < 1940)

senior$zpalter

nrow(senior)

# Import ------
# library(haven)
# etb18 <- haven::read_dta() # "" und dann tab für Pfad

# Rdata speichern
saveRDS(dat1,file = "./data/Dateiname.Rdata")
# Rdata einlesen
dat1a <- readRDS(file = "./data/Dateiname.Rdata")



## save(studs, file = "./data/stud_vektor.RData")
## rm(studs)
## load(file = "./data/stud_vektor.RData") # studs wieder mit selbem Namen zurück im environment

## save(studs,profs, file = "./data/meine_vektoren.RData")
## rm(studs,profs)
## load(file = "./data/meine_vektoren.RData") # studs & profs mit selbem Namen zurück im environment

# Hilfe -------------------
## Hilfe zu Paketen -------
help("dplyr")
help(package = "dplyr")
vignette("dplyr")
vignette(package = "dplyr")
vignette("rowwise")
## Hilfe zu Funktionen ----
?select()
