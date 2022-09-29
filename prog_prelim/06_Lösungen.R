# ------------------- #
# Kapitel 6: Data Wrangling 2
# Lösung
# ------------------- #


# Übung 1 mutate/$newvar -------


## Erstellen Sie dat3 wie oben gezeigt aus dat1 und dat2 -----
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
dat3 <- bind_rows(dat1,dat2)


## Berechnen Sie das Betreuungsverhältnis (Studierende pro Professur) und legen Sie die Ergebnisse in einer neuen Variable studprofs ab. -----
## Nutzen Sie dazu mutate() oder ...$newvar <- 
dat4 <- 
  dat3 %>% mutate(studprofs = studs/profs)
dat4

dat3$studprofs <- dat3$studs/dat3$profs
dat3

##   Berechnen Sie außerdem das Betreuungsverhältnis an den Hochschulen relativ zum Mittelwert des Betreuungsverhältnisses (rel_studprofs). -----
dat3$studprofs_rel_to_mean <- dat3$studprofs - mean(dat3$studprofs)


dat3 %>% mutate(studprofs = studs/profs,
                studprofs_rel_to_mean = studprofs - mean(studprofs))


## Liegt das Betreuungsverhältnis über oder unter dem Mittelwert? Wie können Sie den Befehl anpassen, sodass die Variable rel_studprofs lediglich TRUE oder FALSE enthält anstelle der Zahlenwerte. -----


dat3 %>% mutate(studprofs = studs/profs,
                studprofs_rel_to_mean = studprofs - mean(studprofs) > 0)

dat3$studprofs_rel_to_mean <- dat3$studprofs - mean(dat3$studprofs) > 0

## Wandeln Sie rel_studprofs in eine Dummy-Variable mit 0/1 als Werten statt TRUE/FALSE -----

dat3$studprofs_rel_to_mean <- as.numeric(dat3$studprofs - mean(dat3$studprofs) > 0)

dat3$studprofs_rel_to_mean <- dat3$studprofs - mean(dat3$studprofs) > 0 %>% as.numeric(.)

dat3 %>% mutate(studprofs = studs/profs,
                studprofs_rel_to_mean = as.numeric(studprofs - mean(studprofs) > 0))

dat3 %>% mutate(studprofs = studs/profs,
                studprofs_rel_to_mean = studprofs - mean(studprofs) > 0 %>% as.numeric(.))

# Übung 2 group_by() -------

# berechnen Sie das Betreuungsverhältnis (studprofs) an den Hochschulen/Unis mit und ohne Promotionsrecht und fügen Sie dieses als neue Spalte ein.

dat3 %>% group_by(prom_recht) %>% mutate(studprofs = studs/profs )

# Wie müssen Sie ihren Code ändern, wenn die neue Variable das studprof-Verhältnis der Hochschule relativ zum Mittelwert des Betreuungsverhältnisses innerhalb mit und ohne Promotionsrecht wiedergeben soll?
  
dat3 %>% group_by(prom_recht) %>% mutate(studprofs = studs/profs - mean(studs/profs) )


# Übung 3 `across` -------
etb18_small <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                               n_max = 10, # nur 10 Zeilen
                               col_select = c("zpalter","S1","F1450_01","F1450_02","F1450_03")
                               )

## Mittelwert für F1450 -----
etb18_small %>% 
  summarise(across(matches("F1450"),~mean(.x,na.rm=T)))

# eigentlich gefährlich weil es noch weitere F1450_XX Variablen gibt im "großen ETB18"-Datensatz, daher so sicherer:
etb18_small %>% 
  summarise(across(matches("F1450_(01|02|03)"),~mean(.x,na.rm=T)))

## F1450_(01|02|03) werden mit neuen Werten überschrieben

## nach Geschlecht ----
etb18_small %>% 
  group_by(S1) %>% 
  summarise(across(matches("F1450"),~mean(.x,na.rm=T)))

## var, N, und .names  -----
etb18_small %>% 
  group_by(S1) %>% 
  summarise(across(matches("F1450"),
                   list(mean = ~mean(.x,na.rm=T),
                        var = ~var(.x,na.rm=T),
                        nNA = ~sum(!is.na(.x))),
                   .names = "{.fn}.{.col}") )


# Übung 4: cut -----

etb18_small2 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                         col_select = c("zpalter","S1","F1450_01","F1450_02","F1450_03"))
etb18_small2 <- etb18_small2 %>% slice(5654:5666)
etb18_small2


## ü50/u50 -----
etb18_small2 %>% 
  mutate(over_under = ifelse(zpalter >= 50, "ü50", "u50"))

## u40 u55 ü55 ---------------
etb18_small2 %>% 
  mutate(age_cat = case_when(zpalter <= 40 ~ "u40",
                             zpalter <= 50 ~ "u50",
                             zpalter >  50 ~ "ü50"),
         age_cut = cut(zpalter,breaks = c(0,40,50,100)),
         age_cut_lab = cut(zpalter,breaks = c(0,40,50,100),
                           labels = c("u40","u50","ü50"))) 

# Kontrolle:
etb18_small2 %>% 
  mutate(age_cat = case_when(zpalter <= 40 ~ "u40",
                             zpalter <= 50 ~ "u50",
                             zpalter >  50 ~ "ü50"),
         age_cut = cut(zpalter,breaks = c(0,40,50,100)),
         age_cut_lab = cut(zpalter,breaks = c(0,40,50,100),
                           labels = c("u40","u50","ü50"))) %>% 
  count(age_cat,age_cut_lab)



## ifelse F1450_01 ----

etb18_small2 %>% mutate(F1450_01 = ifelse(F1450_01>4,NA,F1450_01))
## ifelse & `across()` auf F1450_01,F1450_02 und F1450_03 ------
etb18_small2 %>% mutate(across(matches("F1450_(01|02|03)"), ~ifelse(.x>4,NA,.x)))  




# function -----
stdize <- function(x){
  stdx <- (x - mean(x,na.rm = T))/sd(x,na.rm = T)
  return(stdx)
}

etb18_small2 %>% 
  mutate(across(matches("F1450"),~stdize(.x),.names = "std_{.col}"))