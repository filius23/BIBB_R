
# ------------------- #
# Kapitel 14: Tabellenexport
# Lösung
# ------------------- #


# daten einlesen -----
etb_ue13 <- 
  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",col_select = c("gkpol","az","zpalter","m1202"))%>% 
  filter(zpalter < 100, m1202 > 0) %>% 
  mutate(gkpol = factor(gkpol,levels = 1:7, labels = c("<2k", "2k bis <5k", "5k bis <20k", "20k bis <50k", "50k bis <100k", 
                                                       "100k bis <500k", "500k und mehr")),
         m1202 = factor(m1202, levels = 1:4,labels = c("ohne","dual/schul.","Aufst.","FH/Uni"))) 

## metrische Variablen ------
tab_df <- 
  etb_ue13 %>% 
    select(az,zpalter) %>% 
    pivot_longer(cols = everything(), names_to = "variable") %>% 
    group_by(variable) %>% 
    summarise(min = min(value,na.rm = T),
              mean = mean(value,na.rm = T),
              max = max(value,na.rm = T))


# als flextable formatieren
tab_df %>% 
  flextable() %>% 
  colformat_double(digits = 3,j = 3) %>% 
  autofit()

tab_ftab <- 
  tab_df %>% 
    flextable() %>% 
    colformat_double(digits = 3,j = 3) %>% 
    autofit()
save_as_docx(tab_ftab,path = "./results/Tabelle_Übung13.docx")


## Bonus: viele formatierungen
tab_ftab2 <- 
etb_ue13 %>% 
  select(Arbeitszeit=az,Alter=zpalter) %>% # in select() umbenennen
  pivot_longer(cols = everything(), names_to = "Variable") %>% 
  group_by(Variable) %>% 
  summarise(Min = min(value,na.rm = T),
            Mean = mean(value,na.rm = T),
            Median = median(value,na.rm = T),
            Max = max(value,na.rm = T)) %>% 
  flextable() %>% 
  border_remove() %>% 
  hline_top(border = fp_border(color = "navy",style = "dotted",width = 4)) %>% 
  hline(i=2,j=3:4,border = fp_border(color = "orange",style = "dashed",width = 4)) %>% 
  colformat_double(j=3,big.mark = ".", decimal.mark = ",",digits = 2) %>% 
  colformat_double(j=c(2,4:5),big.mark = ".", decimal.mark = ",",digits = 0) %>% 
  bg(i= 2,j=4,bg = "green") %>% 
  autofit()
save_as_docx(tab_ftab2,path = "./results/Tabelle_Übung13_ver02.docx")


## kategoriale Variablen --------------
tab_kat <- 
  etb_ue13 %>%  
    select(gkpol,m1202) %>% 
    pivot_longer(everything(),names_to = "variable") %>% 
    count(variable,value)

tab_kat %>% 
  flextable() %>% 
  autofit() %>% 
  border_remove() %>%
  hline_top() %>% # linie oben
  hline(i =c(7)) %>% 
  merge_v(j=1) # gleiche Zeilen in Spalte 1 zusammenfassen

# Bonus flextable befehle
tab_kat %>% 
  flextable() %>% 
  autofit() %>% 
  border_remove() %>%
  hline_top() %>% # linie oben
  hline(i =c(7,11)) %>% 
  set_header_labels(variable ="Variable",
                    value = "Kategorie",
                    n = "Anzahl") %>% 
  merge_v(j = 1)

KAT_FTAB <- 
tab_kat %>% 
  flextable() %>% 
  autofit() %>% 
  border_remove() %>%
  hline_top() %>% # linie oben
  hline(i =c(7,11)) %>% 
  set_header_labels(variable ="Variable",
                    value = "Kategorie",
                    n = "Anzahl") %>% 
  merge_v(j = 1)


save_as_docx(KAT_FTAB ,path = "./results/Übung13_2.docx")



ftab2 <- 
  tab_kat %>% flextable() %>% border_remove()

ftab2 %>% hline(i = ~variable == "gkpol")
# linien wenn folgende Variable anders heißt
ftab2 %>% hline(i = ~variable != lead(variable)) %>% merge_v(j = 1)

ftab2 %>% bg(j=1,i=~n<2000,bg = "orange")

# Regressionstabelle -----------
m1 <- lm(az ~ m1202 , etb_ue13)
m2 <- lm(az ~ m1202 + zpalter, etb_ue13)

modelsummary(list(m1,m2))

modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2),
  output = "flextable",
  gof_omit = "IC|Log|RMS")


ref_rows <- tribble( ~ term,    ~ "Modell 1",  ~ "Modell 2",
                     "keine Ausbildung",    'ref.',   'ref.')
attr(ref_rows, 'position') <- 3 # Zeile angeben

regtab2 <- 
  modelsummary(
    list("Modell 1" = m1, "Modell 2" = m2),
    output = "flextable",
    gof_omit = "IC|Log|RMS",
    add_rows = ref_rows,
    coef_rename = c(
      "(Intercept)" = "Intercept",
      "m1202dual/schul." = "Duale/Schulische Ausbildung",
      "m1202Aufst." = "Aufstiegsfortbildung",
      "m1202FH/Uni" = "FH- oder Uni-Abschluss",
      "zpalter" = "Alter"
    ),
    stars = T,
    fmt = 2) %>% 
  autofit() %>% 
  italic(i = ~ `Modell 2` == "ref.",j =2:3) %>% 
  bg(j=1,i=~`Modell 2`>20,bg = "orange")


# read_docx("pfad/zur/Vorlage/DIN_A4_Vorlage.docx") %>%
read_docx("./results/Vorlage_Test.docx") %>%
  body_add_flextable(., value = met_ft ) %>% # flextable met_ft einfügen
  body_add_par(.,"") %>% # leeren Absatz einfügen
  body_add_flextable(., value = kat_ft ) %>% # flextable cat_ft einfügen
  body_add_par(.,"") %>% # leeren Absatz einfügen
  body_add_flextable(., value = regtab2 ) %>% # flextable regtab2 einfügen
  print(target = "./results/Tables.docx")

