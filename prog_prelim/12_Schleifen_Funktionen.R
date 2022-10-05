if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 
library(tidyverse)
library(marginaleffects)

etbx <-  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                         col_select = c("S1","F518_SUF","m1202","az","zpalter","F1605e")) %>% 
  filter(F518_SUF < 99998, m1202 %in% 1:4, zpalter < 9999 ) %>% 
  mutate(ausb = factor(m1202, levels = 1:4, labels = c("ohne","dual/schul.","Aufst.","FH/Uni")),
         S1 = factor(S1,levels = 1:2,labels =c("m","w")))

mod_function <- function(modx){
  mx <- lm(formula = modx,data = etbx)
  return(mx)
}

mod_function("F518_SUF ~ az")

mlst2 <- list(
  "Modell 1" = "F518_SUF ~ az",
  "Modell 2"  = "F518_SUF ~ az + S1",
  "Modell 3" = "F518_SUF ~ az + S1 + m1202",
  "Modell 4" = "F518_SUF ~ az + S1 + m1202 + zpalter"
)

mlst2[[4]]
mlst2$`Modell 4`

mods2 <- lapply(mlst2,mod_function)
mods2$`Modell 1`
mods2$`Modell 2`

modelsummary::modelsummary(mods2,stars = T,gof_omit = "IC|RM|Log")

mod_function2 <- function(modx, tidy_mod = T){
  mx <- lm(formula = modx,data = etbx)
  if(tidy_mod == T) mx <- tidy(mx,conf.int = T)
  return(mx)
}

mod_function2("F518_SUF ~ az")
mod_function2("F518_SUF ~ az",tidy_mod = F)

mod_l2 <- lapply(mlst2,mod_function2)
lapply(mod_l2,class)

## bind_rows(mod_l2,.id="Mod_name")

bind_rows(mod_l2,.id="Mod_name") %>% 
  rmarkdown::paged_table()

mods3 <- lapply(mlst2,function(modx){
  mx <- lm(formula = modx,data = etbx)
  return(mx)
})
mods3$`Modell 1`

for(i in 1:8){
  print(i)
}

## for(i in 1:8){
##   etbx %>% slice(i) %>% print()
## }

etb_ue12 <- 
  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                  col_select = c("S1","F518_SUF","m1202","az","zpalter","F1605e")) %>% 
  filter(F518_SUF < 99998, m1202 %in% 1:4, zpalter < 9999 ) %>% 
  mutate(ausb = factor(m1202, levels = 1:4, labels = c("ohne","dual/schul.","Aufst.","FH/Uni")),
         S1 = factor(S1,levels = 1:2,labels =c("m","w")))

mods <- map(mlst2,~lm(formula = .x,data = etbx))

for(v in c("ausb","S1","F1605e")){
  etbx %>% count(v) %>% print()
}

for(v in c("ausb","S1","F1605e")){
  etbx %>% count(!!v) %>% print()
}
