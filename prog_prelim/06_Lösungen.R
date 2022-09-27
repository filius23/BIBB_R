

# Übung 1 mutate/$newvar -------



# Übung 2 group_by() -------



# Übung 3 `across` -------
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")
etb18_small <- etb18 %>% slice(1:10) %>% select(zpalter,S1,F1450_01,F1450_02,F1450_03)
etb18_small 

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

etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")
etb18_small2 <- etb18 %>% slice(5654:5666) %>% select(zpalter,S1,F1450_01,F1450_02,F1450_03)
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