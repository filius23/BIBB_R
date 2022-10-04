
# ------------------- #
# Kapitel 14: Tabellenexport
# Lösung
# ------------------- #


# daten einlesen -----
etb_ue14 <- 
  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",col_select = c("gkpol","az","zpalter","m1202"))%>% 
  filter(zpalter < 100, m1202 > 0) %>% 
  mutate(gkpol = factor(gkpol,levels = 1:7, labels = c("<2k", "2k bis <5k", "5k bis <20k", "20k bis <50k", "50k bis <100k", 
                                                       "100k bis <500k", "500k und mehr")),
         m1202 = factor(m1202, levels = 1:4,labels = c("ohne","dual/schul.","Aufst.","FH/Uni"))) 

## metrische Variablen ------
tab_df <- 
  etb_ue14 %>% 
    select(az,zpalter) %>% 
    pivot_longer(cols = everything(), names_to = "variable") %>% 
    group_by(variable) %>% 
    summarise(min = min(value,na.rm = T),
              mean = mean(value,na.rm = T),
              max = mean(value,na.rm = T))



tab_df %>% 
  flextable() %>% 
  autofit()

## kategoriale Variablen --------------
tab_kat <- 
  etb_ue14 %>%  
    select(gkpol,m1202) %>% 
    pivot_longer(everything(),names_to = "variable") %>% 
    count(variable,value)

tab_kat %>% 
  flextable() %>% 
  autofit() %>% 
  border_remove() %>%
  hline_top() %>% # linie oben
  hline(i =c(7))

# Bonus flextable befehle
tab_kat %>% 
  flextable() %>% 
  autofit() %>% 
  border_remove() %>%
  hline_top() %>% # linie oben
  hline(i =c(7,11)) %>% 
  set_header_labels(variable ="Variable",
                    value = "Kategorie") %>% 
  merge_v(j = 1)

# Regressionstabelle -----------
m1 <- lm(az ~ m1202 , etb_ue14)
m2 <- lm(az ~ m1202 + zpalter, etb_ue14)

modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2),
  output = "flextable",
  gof_omit = "IC|Log|RMS")


ref_rows <- tribble( ~ term,    ~ "Modell 1",  ~ "Modell 2",
                     "keine Ausbildung",    'ref.',   'ref.')
attr(ref_rows, 'position') <- 3 # Zeile angeben

# regtab2 <- 
  modelsummary(
    list("Modell 1" = m1, "Modell 2" = m2),
    output = "flextable",
    gof_omit = "IC|Log|RMS",
    coef_rename = c(
      "(Intercept)" = "Intercept",
      "m1202dual/schul." = "Duale/Schulische Ausbildung",
      "m1202Aufst." = "Aufstiegsfortbildung",
      "m1202FH/Uni" = "FH/Uni-Abschluss",
      "zpalter" = "Alter"
    ),
    add_rows = ref_rows,
    stars = T,
    fmt = 2) %>% 
  autofit() %>% 
  italic(i = ~ `Modell 2` == "ref.",j =2:3)
