# ------------------- #
# Kapitel 11: Data Wrangle
# merge/join & reshape
# ------------------- #

library(tidyverse)
library(marginaleffects)

# Daten einlesen ------ 
etbx <-  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                         col_select = c("S1","F518_SUF","m1202","az","zpalter","F1605e")) %>% 
  filter(F518_SUF < 99998, m1202 %in% 1:4, zpalter < 9999 ) %>% 
  mutate(ausb = factor(m1202, levels = 1:4, labels = c("ohne","dual/schul.","Aufst.","FH/Uni")),
         S1 = factor(S1,levels = 1:2,labels =c("m","w")))


# Funktion für Modelle ----- 
mod_function <- function(modx){
  mx <- lm(formula = modx,data = etbx)
  return(mx)
}
 # anwenden 
mod_function("F518_SUF ~ az")

## Modell Liste ------
mlst <- list(
  "Modell 1" = "F518_SUF ~ az",
  "Modell 2" = "F518_SUF ~ az + S1",
  "Modell 3" = "F518_SUF ~ az + S1 + ausb",
  "Modell 4" = "F518_SUF ~ az + S1 + ausb + zpalter"
)

mlst[[4]]
mlst$`Modell 4`

## Modellliste anwenden: Modellserien erstellen -----
mods <- lapply(mlst,mod_function)
mods$`Modell 1`
mods$`Modell 2`

## Modelsummary ------
modelsummary::modelsummary(mods,stars = T,gof_omit = "IC|RM|Log")

# if/else -----------------

add_controls <- c("+ zpalter + I(zpalter^2)")

mod_function2 <- function(modx, add_age){
  if(add_age == T) {
    mx <- lm(formula = paste0(modx,add_controls),data = etbx)
  } else {
    mx <- lm(formula = paste0(modx),data = etbx)
  }
  return(mx)
}

mod_function2("F518_SUF ~ az",add_age=F)
mod_function2("F518_SUF ~ az",add_age=T)



## Regressionsergebnisse als data.frame -------
mod_function3 <- function(modx, tidy_mod = T){
  mx <- lm(formula = modx,data = etbx)
  if(tidy_mod == T) mx <- tidy(mx,conf.int = T)
  return(mx)
}

mod_function3("F518_SUF ~ az") # output ist jetzt ein data.frame
mod_function3("F518_SUF ~ az",tidy_mod = F)

# als Objekt ablegen 
mod_l3  <- lapply(mlst,mod_function3)
lapply(mod_l3 ,class)
bind_rows(mod_l3 ,.id="Mod_name")

# alles auf einmal:
lapply(mlst,mod_function3) %>% bind_rows(.id="Mod_name")

# mit purrr:
# map_dfr(mlst,mod_function3,.id="Mod_name")





# adhoc function ------
mods3 <- lapply(mlst2,function(modx){
  mx <- lm(formula = modx,data = etbx)
  return(mx)
})
mods3$`Modell 1`

# for loops ------
for(i in 1:8){
  print(i)
}

## beispiel 2
for(i in 1:8){
 etbx %>% slice(i) %>% print()
}

etb_ue12 <- 
  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                  col_select = c("S1","F518_SUF","m1202","az","zpalter","F1605e")) %>% 
  filter(F518_SUF < 99998, m1202 %in% 1:4, zpalter < 9999 ) %>% 
  mutate(ausb = factor(m1202, levels = 1:4, labels = c("ohne","dual/schul.","Aufst.","FH/Uni")),
         S1 = factor(S1,levels = 1:2,labels =c("m","w")))


# map = lapply ------
mods <- map(mlst2,~lm(formula = .x,data = etbx))

# _dfr = "data.frame" mit row_bind()
map_dfr(mlst2,~lm(formula = .x,data = etbx) %>% tidy(.),.id = "mod")


# Anhang: loop über Variablen --------
for(v in c("ausb","S1","F1605e")){
  etbx %>% count(v) %>% print()
}

for(v in c("ausb","S1","F1605e")){
  etbx %>% count(!!v) %>% print()
}
