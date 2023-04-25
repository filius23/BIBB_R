# ------------------- #
# Kapitel 12: Schleifen/Funktionen
# Lösung
# ------------------- #
etb_ue12 <- 
  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                  col_select = c("S1","F518_SUF","m1202","az","zpalter","F1605e")) %>% 
  filter(F518_SUF < 99998, m1202 %in% 1:4, zpalter < 9999 ) %>% 
  mutate(m1202_fct = factor(m1202, levels = 1:4, labels = c("ohne","dual/schul.","Aufst.","FH/Uni")),
         S1_fct = factor(S1,levels = 1:2,labels =c("m","w"))) %>% 
  haven::zap_labels() %>% 
  haven::zap_label()


# Übung 1 Funktion -------

# funktion erstellen
  mo_function <- function(modx){
    mx <- lm(formula = modx,data = etb_ue12)
    return(mx)
  }
# testen 
  mo_function("az ~ S1 + m1202_fct + zpalter") %>% summary()

# Modell-Vektor
  modls_ue12 <- c(
    modell1 = "az ~ S1",
    modell2 = "az ~ S1 + m1202_fct",
    modell3 = "az ~ S1 + m1202_fct + zpalter"
  )
  
## Modell-Vektor anwenden mit map()
  map(modls_ue12,mo_function)  
  map(modls_ue12,mo_function) %>% modelsummary::modelsummary()
  
  
#  Übung 2: Nach Frau/Mann  ------
mod_u12_2 <- 
  etb_ue12 %>% 
    nest(.by=S1_fct) %>% 
    mutate(model = )
    map(., function(dat1){
      lm("az ~ m1202_fct + zpalter + I(zpalter^2)", data = dat1)
    })
  
library(modelsummary)
modelsummary(mod_u12_2)  
modelplot(mod_u12_2,coef_omit = "Intercept")

# Visualisierung der quad. Terme mit ggeffects
library(ggeffects)
ggplots <- map(mod_u12_2, ~ggpredict(.x, terms = "zpalter") %>% plot() ) 
ggplots$m + labs(title = "Männer")
ggplots$w + labs(title = "Frauen")
