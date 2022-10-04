# ------------------- #
# Kapitel 12: Schleifen/Funktionen
# Lösung
# ------------------- #
etb_ue12 <- 
  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                  col_select = c("S1","F518_SUF","m1202","az","zpalter","F1605e")) %>% 
  filter(F518_SUF < 99998, m1202 %in% 1:4, zpalter < 9999 ) %>% 
  mutate(ausb = factor(m1202, levels = 1:4, labels = c("ohne","dual/schul.","Aufst.","FH/Uni")),
         S1 = factor(S1,levels = 1:2,labels =c("m","w")))


# Übung 1 Funktion -------

  mo_function <- function(modx){
    mx <- lm(formula = modx,data = etb_ue12)
    return(mx)
  }
  
  mo_function("az ~ S1 + ausb + zpalter")

# Übung 2 lapply ------

  mod_list <- list(
    modell1 = "az ~ S1",
    modell2 = "az ~ S1 + ausb",
    modell3 = "az ~ S1 + ausb + zpalter"
  )
  

  lapply(mod_list,mo_function)  
  lapply(mod_list,mo_function)  %>% modelsummary::modelsummary()
  
  
#  Übung 3: if Funktion  ------
  mo_function2 <- function(modx, women_only = T){
    if(women_only) etb_ue12 <- etb_ue12 %>% filter(S1 == "w" )
    mx <- lm(formula = modx,data = etb_ue12)
    return(mx)
  }

  mo_function2("az ~ ausb + zpalter",women_only = T)
  mo_function2("az ~ ausb + zpalter",women_only = F)
  
  