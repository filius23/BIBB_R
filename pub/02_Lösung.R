# ------------------- #
# Kapitel 2: data.frame
# Lösung
# ------------------- #

# Übung 1 -------------
dat2 <- data.frame(studs = c(14954,47269 ,23659,9415 ,38079),
                   profs = c(250,553,438 ,150,636),
                   prom_recht = c(FALSE,TRUE,TRUE,TRUE,FALSE),
                   gegr  = c(1971,1870,1457,1818,1995))

dat2
View(dat2) # für variablenexplorer

dat2$uni <- c("FH Aachen","RWTH Aachen","Uni Freiburg","Uni Bonn","FH Bonn-Rhein-Sieg")
dat2


dat2$studs / dat2$profs
dat2

dat2$studs_profs <- dat2$studs / dat2$profs
dat2 # neue variable studs_profs sichtbar

dat2$studs_profs <- "a"
dat2


dat2$studs_profs2 <- dat2$studs / dat2$profs
dat2


# Übung 2 -------------
# Installieren Sie die Pakete des tidyverse mit install.packages("tidyverse")
# install.packages("tidyverse") # nur einmal ausführen

dat2 # Ausgangslage

# Verwenden Sie wieder den data.frame dat2 aus Übung 1
# Nutzen Sie filter, um sich nur die Unis mit unter 10000 Studierenden anzeigen zu lassen. 
filter(dat2,studs < 10000)
# Lassen Sie sich nur die dritte Zeile von dat2 anzeigen.
slice(dat2,3)
# Lassen Sie sich nur die Spalte gegr anzeigen.
select(dat2,gegr)
# Lassen Sie sich nur Zeilen der Hochschulen mit Promotionsrecht anzeigen.
filter(dat2,prom_recht == TRUE)
filter(dat2,prom_recht)

filter(dat2,prom_recht != FALSE) 


# Übung 3 ------------


# Lassen Sie sich nur Hochschulen anzeigen, die 1971, 1457 oder 1995 gegründet wurden - 
# und für diese Fälle nur den Namen und das Gründungsjahr.

dat2 %>% filter(gegr == 1971 | gegr == 1457 | gegr == 1995)
dat2 %>% filter(gegr %in% c(1971, 1457, 1995))

dat2 %>% filter(between(gegr,1971,1995)) # range 

dat2 %>% filter(gegr %in% c(1971, 1457, 1995)) %>% select(uni,gegr)

# Sortieren Sie den Datensatz entsprechend dieser Reihenfolge:
dat2$uni_fct <- factor(dat2$uni, 
                       levels = c("RWTH Aachen",
                                  "Uni Freiburg","Uni Bonn","FH Aachen",
                                  "FH Bonn-Rhein-Sieg"))

levels(dat2$uni_fct)
dat2
dat2 %>% arrange(uni_fct)


# Übung 4 ------------
getwd() # arbeitsverzeichnis checken

etb_small <- read.table(file = "./data/BIBBBAuA_2018_small.csv", sep = ";", header = T)
head(etb_small)
View(etb_small)

nrow(etb_small)
names(etb_small)
dim(etb_small)



etb_small %>% 
  filter(intnr == 2781) %>% 
  select(intnr,zpalter)

etb_small %>% 
  slice(1:5) %>% 
  select(intnr,zpalter)




etb_small$alter <- 2022 - etb_small$zpalter


nach_1960 <- etb_small %>% filter(S2_j > 1960)
nrow(nach_1960)



