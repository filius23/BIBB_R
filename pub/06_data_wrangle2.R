# ------------------- #
# Kapitel 6: Data Wrangling 2
# ------------------- #

library(tidyverse)

# Beispieldatensätze von Anfang ---------------

dat1 <- data.frame(studs = c(19173,5333,15643), 
                   profs = c(322,67,210),
                   gegr  = c(1971,1830,1973),
                   prom_recht = rep(TRUE,3),
                   uni = c("Uni Bremen","Uni Vechta", "Uni Oldenburg"))
dat2 <- data.frame(studs = c(14954,47269 ,23659,9415 ,38079), 
                   profs = c(250,553,438 ,150,636),
                   prom_recht = c(FALSE,TRUE,TRUE,TRUE,FALSE),
                   gegr  = c(1971,1870,1457,1818,1995),
                   uni = c("FH Aachen","RWTH Aachen","Uni Freiburg","Uni Bonn","FH Bonn-Rhein-Sieg"))
dat1
dat2


# bind_rows() ------
dat3 <- bind_rows(dat1,dat2)
dat3

# 2 Arten eine Variable in einen data.frame() hinzuzufügen -----
dat3$studs_to_mean  <- dat3$studs - mean(dat3$studs)
dat3

## base: $ -----------

dat3$studs_to_mean  <-  NULL
dat3

## dplyr: mutate() -------
dat3 %>% mutate(studs_to_mean = studs-mean(studs))

dat3 %>% mutate(studs_to_mean = studs-mean(studs),
                profs_to_mean = profs-mean(profs))


dat3 %>% mutate(rel_to_mean = studs-mean(studs),
                above_mean  = rel_to_mean > 0)


dat3 # wird nicht in dat3 hinterlegt -> ablegen in neuem Objekt oder überschreiben

dat4 <-
  dat3 %>% 
  mutate(rel_to_mean = studs-mean(studs),
         above_mean = rel_to_mean > 0)

dat4


## Exkurs: Dummy-Variable aus logischer Variable -------
dat3 %>% 
  mutate(prom_dummy = as.numeric(prom_recht ) )

dat3 %>% 
  mutate(rel_to_mean = studs-mean(studs),
         above_mean = rel_to_mean > 0,
         above_mean_dummy = as.numeric(above_mean)) 
## Übung -----------

# group_by() / .by = -------

dat5 <- dat3 %>% 
  select(-uni,-gegr) # nur dass alles zu sehen ist


## group_by() hilft bei gruppierten Berechnungen
dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  group_by(prom_recht) %>% # innerhalb von prom_recht rechnen
  mutate(m_studs2 = mean(studs),
         m_profs2 = mean(profs))

## ungroup() hebt gruppierung auf
dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  group_by(prom_recht) %>%   # gruppierung nach prom_recht setzen
  mutate(m_studs2 = mean(studs)) %>% 
  ungroup() %>%  # mit ungroup() aufheben
  mutate(m_profs2 = mean(profs)) 

# achtung: gruppierung bleibt bestehen, wenn wir sie nicht aufheben
dat5_grp <- 
      dat5 %>%
        mutate(m_studs = mean(studs),
               m_profs = mean(profs)) %>% 
        group_by(prom_recht) %>%
        mutate(m_studs2 = mean(studs)) # ! Kein ungroup() am Ende!

dat5_grp %>%  mutate(profs = mean(profs))


## neu ab dplyr 1.1.1: .by = in mutate()
dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  mutate(m_studs2 = mean(studs),.by = prom_recht) %>% 
  mutate(m_profs2 = mean(profs))


## summarise statt mutate() behält nur bearbeitete und gruppierungsvariablen:
dat5 %>%
  group_by(prom_recht) %>%
  summarise(m_studs = mean(studs))

dat5 %>%
  summarise(m_studs = mean(studs),.by = prom_recht) 

# mehrere Gruppierungsvariablen
dat5 %>%
  summarise(m_studs = mean(studs),.by = c(prom_recht,profs)) 


## Übung -----------


# across ---------------

dat3 <- dat3 %>% select(any_of(colnames(dat1)))


dat3 %>%
  summarise(studs = mean(studs),
            profs = mean(profs))

## across hilft wiederholungen zu vermeiden/ funktionen schnell auf viele Variablen anzuwenden:
dat3 %>%
  summarise(across(.cols = matches("studs|profs"),.fns = ~mean(.x)))

# regular expressions
dat3 %>%
  summarise(across(.cols = matches("^s|^p"),.fns = ~mean(.x)))
  # summarise(across(.cols = matches("^F140|^F230"),.fns = ~mean(.x)))
  
# warum .cols?
# es geht auch positionsbezogen:
dat3 %>%
  summarise(across(matches("^s|^p"),~mean(.x)))


## auch mit group_by() ------
dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), ~mean(.x)))

dat3 %>%
  summarise(across(matches("studs|profs"), ~mean(.x)),.by = prom_recht)


## mehrere Werte mit list() ------
dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), list(mean = ~mean(.x), sd = ~sd(.x))))

dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), list(MeAn = ~mean(.x), sd = ~sd(.x))))


## list vorab definieren -------
wert_liste <- list(mean = ~mean(.x), sd = ~sd(.x), max = ~max(.x,na.rm = T))

dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), wert_liste))

## Variablennamen anpassen -------
dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), 
                   list(mean = ~mean(.x), sd = ~sd(.x)),
                   .names = "{.fn}_{.col}")) # fn -> Funktion , col -> Variablenname
# spaltennamen auch weiter anpassbar
dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), 
                   list(mean = ~mean(.x), sd = ~sd(.x)),
                   .names = "{.fn}_ABS_{.col}")) # fn -> Funktion , col -> Variablenname


dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), 
                   list(mean = ~mean(.x), sd = ~sd(.x)),
                   .names = "XX_{.fn}_{.col}")) # fn -> Funktion , col -> Variablenname


## auch mit mutate() -------
dat3 %>%
  mutate(across(matches("studs|profs"), ~mean(.x), .names = "m_{.col}"))


## Übung -----


# Eigene Funktionen ----------------


etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")

sat_small <- 
  etb18 %>% 
  select(F1450_04,F1450_05,F1450_06) %>% 
  slice(12:16) %>% 
  haven::zap_labels() %>% haven::zap_label() # labels entfernen
sat_small


# relativ zum Mittelwert
sat_small %>% 
  mutate(dmean_F1450_01 = F1450_01 - mean(F1450_01,na.rm = T),
         dmean_F1450_02 = F1450_02 - mean(F1450_02,na.rm = T))

## function() ------
dtomean <- function(x) {
  d_x <- x - mean(x,na.rm = T)
  return(d_x)
}
## function mit anderem platzhalter oder mehreren platzhaltern
  dtomean2 <- function(I) {
    d_x <- I - mean(I,na.rm = T)
    return(d_x)
  }

  teiler3 <- function(x,y) {
    d_x <- x / y
    return(d_x)
  }


var1 <- c(1,6,3,7,8,1,5)

mean(var1)

dtomean(var1)
dtomean2(var1)

var2 <- c(2.4,5,3,5,9,3,8)
teiler3(x = var1,y = var2)

# auf eine Variable aus sat_small anwenden
dtomean(sat_small$F1450_04)

#
sat_small %>% map(.f = ~dtomean(.x))

## kürzer dank function -----
sat_small %>% 
  mutate(std_F1450_01 = dtomean(F1450_01),
         std_F1450_02 = dtomean(F1450_02))

## noch kürzer mit across() ------
sat_small %>% 
  mutate(across(matches("F1450"),~dtomean(.x),.names = "dmean_{.col}"))



# Hilfsfunktionen ifelse() und case_when() ------------

## ifelse -----
x1 <- 1:10
x1
ifelse(x1 > 5,"groß","klein" )
ifelse(x1 %% 2 == 0,"gerade","ungerade")


dat3 %>% mutate(rel_to_mean = studs-mean(studs),
                ab_mean_lab = ifelse(rel_to_mean > 0,"darüber","darunter"))

## case_when --------
dat3 %>% mutate(alter = case_when(gegr < 1500 ~ "sehr alt",
                                  gegr < 1900 ~ "alt"))

dat3 %>% mutate(alter = case_when(gegr < 1500 ~ "sehr alt",
                                  gegr < 1900 ~ "alt",
                                  TRUE ~ "relativ neu"))

dat3 %>% mutate(alter = case_when(gegr < 1500 & prom_recht == T ~ "sehr alte Uni",
                                  gegr < 1900 & prom_recht == T ~ "alte Uni",
                                  gegr > 1900 & prom_recht == T ~ "junge Uni",
                                  gegr < 1900 & prom_recht == F ~ "alte Hochschule",
                                  gegr > 1900 & prom_recht == F ~ "junge Hochschule"))



# Übung ----------

# rename -------
sat_small %>% rename(neu=F1450_01)

sat_small %>% rename_with(~tolower(.))
sat_small %>% rename_with(~str_remove(.x,"1450_"))

