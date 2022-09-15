

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



