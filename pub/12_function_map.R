# ------------------- #
# Kapitel 12: Funktionen/Schleifen
# ------------------- #

library(tidyverse)
library(marginaleffects)
library(broom)

etb_k12 <-  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                         col_select = c("S1","F518_SUF","m1202","az","zpalter","F1605e","Bula")) %>% 
  filter(F518_SUF < 99998, m1202 %in% 1:4, zpalter < 9999 ) %>% 
  mutate(m1202_fct = factor(m1202, levels = 1:4, labels = c("ohne","dual/schul.","Aufst.","FH/Uni")),
         S1_fct = factor(S1,levels = 1:2,labels =c("m","w")))


# Modellserien schätzen ----

mod1 <- lm(F518_SUF ~ az, data = etb_k12)
mod2 <- lm(F518_SUF ~ az + S1_fct, data = etb_k12)
mod3 <- lm(F518_SUF ~ az + S1_fct + m1202_fct, data = etb_k12)
mod4 <- lm(F518_SUF ~ az + S1_fct + m1202_fct + zpalter, data = etb_k12)

# Modellfunktion 
mod_function <- function(modx) {
  mx <- lm(formula = modx,data = etb_k12)
  return(mx) 
}
# test mit kurzem Modell
mod_function("F518_SUF ~ az") %>% summary() # sieht gut aus

# Modellserie als Vektor anlegen
mdls <- c(
  "Modell 1" = "F518_SUF ~ az",
  "Modell 2" = "F518_SUF ~ az + S1_fct",
  "Modell 3" = "F518_SUF ~ az + S1_fct + m1202_fct",
  "Modell 4" = "F518_SUF ~ az + S1_fct + m1202_fct + zpalter"
)
# ansehen:
mdls

# anwenden mit map:
map(.x = mdls,.f = mod_function)
mods <- map(.x = mdls,.f = mod_function)

# direkt in modelsummary :
library(modelsummary)
modelsummary(mods,stars = T,gof_omit = "IC|RM|Log",output = "flextable")

## Übung ----


# F-test mit anova()
anova(mods$`Modell 1`,mods$`Modell 2`,mods$`Modell 3`,mods$`Modell 4`)

# parallelisierung mit furrr
mdls2 <- c(
  "Modell 1" = "F518_SUF ~ az",
  "Modell 2" = "F518_SUF ~ az + S1_fct",
  "Modell 3" = "F518_SUF ~ az + S1_fct + m1202_fct",
  "Modell 4" = "F518_SUF ~ az + S1_fct + m1202_fct + zpalter",
  "Modell 5" = "F518_SUF ~ az + S1_fct + m1202_fct + zpalter + I(zpalter^2)",
  "Modell 6" = "F518_SUF ~ az + S1_fct + m1202_fct + poly(zpalter,8)",
  "Modell 7" = "F518_SUF ~ poly(az,8) + S1_fct + m1202_fct + poly(zpalter,8)"
)
mods2 <- map(.x = mdls2,.f = mod_function)
mods2 <- furrr::future_map(.x = mdls2,.f = mod_function)



# Ein Modell, mehrere Datensätze ---------

# nest() -> splitten nach Ost/West
etb_k12 %>%
   mutate(east = ifelse(Bula > 10,"east","west")) %>% # Berlin = east
   nest(.by = east)

etb_k12_ow <- 
  etb_k12 %>%
  mutate(east = ifelse(Bula > 10,"east","west")) %>% # Berlin = east
  nest(.by=east)

etb_k12_ow

#In der Spalte `data` sind jetzt also die Datensätze für Ost und West enthalten:
head(etb_k12_ow$data[[1]],n=3)
head(etb_k12_ow$data[[2]],n=3)

mod_ow <- 
  etb_k12 %>%
    mutate(east = ifelse(Bula > 10,"east","west")) %>% # Berlin = east
    nest(.by = east) %>% 
    mutate(model = map(data, function(data) {
      lm("F518_SUF ~ az + m1202_fct + zpalter + S1_fct", data = data) # ad-hoc function --> siehe tip)
    })) 

## map(input, function(x) {
##   ... # letzter Schritt in function wird das Ergebnis ausgegeben
## })



# Grafisch vergleichen
modelsummary::modelplot(mod_ow$model,coef_omit = "Intercept") +
  geom_vline(aes(xintercept = 0), linetype = 2, alpha = .5) +
  scale_color_manual(values = c("orange","navy"), breaks = c("(1)","(2)"), labels = c("West","Ost")) 
  
# Vergleich mit modelsummary
modelsummary(mod_ow$model,stars = T,gof_omit = "IC|RM|Log",output = "flextable")



## Übung ----

# if in function -----------

# zweiter Schritt in function:
mod_function2 <- function(modx){
  mx <- lm(formula = modx,data = etb_k12)
  mx <- tidy(mx,conf.int = T)
  return(mx)
}

mod_function2("F518_SUF ~ az")

# zweites Argument:
mod_function3 <- function(modx, dotidy){
  mx <- lm(formula = modx,data = etb_k12)
  if(dotidy == T) mx <- tidy(mx,conf.int = T) #  nur ausführen wenn dotidy als TRUE mitgegeben wird
  return(mx)
}

# tests:
mod_function3("F518_SUF ~ az",dotidy=T)
mod_function3("F518_SUF ~ az",dotidy=F)


# Beispiel für Standardwert für ein Argument - dotidy = T
mod_function4 <- function(modx, dotidy = T){
  mx <- lm(formula = modx,data = etb_k12)
  if(dotidy == T) mx <- tidy(mx,conf.int = T)
  return(mx)
}
mod_function4("F518_SUF ~ az")
mod_function4("F518_SUF ~ az",dotidy=F)
