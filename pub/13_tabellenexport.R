# ------------------- #
# Kapitel 13: Tabellenexport
# ------------------- #
library(tidyverse) # für die Datenvorbereitung
library(modelsummary) # Tabellen vorbereiten
library(janitor) # kreuztabellen

library(flextable) # Formatierung der Tabelle für Word
library(officer) # eigentlicher Word-Export


# Daten einlesen -------
etb18_kap14 <- 
  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",col_select = c("F518_SUF","az","S1","m1202")) %>% 
  mutate(across(matches("F518|m1202"),~ifelse(.x<0 | .x >= 99998, NA, .x))) %>% 
  na.omit() # alle Zeilen mit (mind.) 1 NA löschen


# Pakete ------------
## install.packages("flextable")
## library(flextable)
## install.packages("officer")
## library(officer)

# flextable ----------
df1 <- data.frame(x1= c(2,2), y1 = c(0,1))
df1

flextable(df1) 

flextable(df1) %>% 
  border_remove()

flextable(df1) %>% 
  border_remove() %>% 
  hline_top(border = fp_border(color = "orange"))

flextable(df1) %>% 
  border_remove() %>% 
  hline_top(border = fp_border(color = "orange")) %>%
  hline(i=1,border = fp_border(color = "blue",style = "dotted")) %>% 
  set_header_labels(x1 = "Anderes Label") %>% 
  add_header_row(values = c("Überschrift"),colwidths = c(2)) %>% 
  bg(bg ="coral1", part = "body") %>% 
  autofit()


# metrische Variablen ----------
summary(etb18_kap14$F518_SUF)
summary(etb18_kap14$az)

etb18_kap14 %>% 
  select(az,F518_SUF) %>% 
  pivot_longer(cols = everything(), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(Min = min(value,na.rm = T),
            Mean = mean(value,na.rm = T),
            Max = max(value,na.rm = T))

etb18_kap14 %>% 
  select(az,F518_SUF) %>% 
  pivot_longer(cols = everything(), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(Min  = min(value,na.rm = T),
            Mean = mean(value,na.rm = T),
            Max  = max(value,na.rm = T)) %>% 
  flextable()

## flextable formatieren
etb18_kap14 %>% 
  select(az,F518_SUF) %>% 
  pivot_longer(cols = everything(), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(Min  = min(value,na.rm = T),
            Mean = mean(value,na.rm = T),
            Max  = max(value,na.rm = T)) %>% 
  flextable() %>% 
  border_remove() %>% 
  hline_top(border = fp_border(color = "orange")) %>% 
  colformat_double(big.mark = " ", decimal.mark = ",",digits = 2) %>% 
  autofit()
# digits für Anzahl der Nachkommastellen
# big.mark für Tausender-trenner 1,000 1 000 oder 1.000 oder...
# decimal.mark für Dezimaltrenner

met_ft <- 
  etb18_kap14 %>% 
  select(az,F518_SUF) %>% 
  pivot_longer(cols = everything(), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(Min  = min(value,na.rm = T),
            Mean = mean(value,na.rm = T),
            SD = sd(value,na.rm = T),
            Max  = max(value,na.rm = T)) %>% 
  flextable() %>% 
  border_remove() %>% 
  hline_top(border = fp_border(color = "orange")) %>% 
  colformat_double(big.mark = " ", decimal.mark = ",",digits = 2) %>% 
  autofit()


# dir.create("./results")
save_as_docx(met_ft,path = "./results/Tabelle1.docx")


## Variablenspalte labeln -> variablen in select() umbenennen
etb18_kap14 %>% 
  select(Arbeitszeit=az,`Einkommen (brutto)`=F518_SUF) %>% 
  pivot_longer(cols = everything(), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(Min  = min(value,na.rm = T),
            Mean = mean(value,na.rm = T),
            Max  = max(value,na.rm = T)) %>% 
  flextable() %>% 
  border_remove() %>% 
  hline_top(border = fp_border(color = "orange")) %>% 
  colformat_double(big.mark = " ", decimal.mark = ",",digits = 2) %>% 
  autofit()

  

# kategoriale Variablen -----

# vorbereiten -> labels in factor()
etb18_kap14 %>%  
  select(S1,m1202) %>% # variablen auswählen
  mutate(S1 = factor(S1,levels = 1:2, labels = c("Männer","Frauen")),
         m1202 = factor(m1202, levels = 1:4,labels = c("ohne Berufsausbildung","dual/schul.","Aufst.","FH/Uni")))

## als long shape 
etb18_kap14 %>%  
  select(S1,m1202) %>% # variablen auswählen
  mutate(S1 = factor(S1,levels = 1:2, labels = c("Männer","Frauen")),
         m1202 = factor(m1202, levels = 1:4,labels = c("ohne Berufsausbildung","dual/schul.","Aufst.","FH/Uni"))) %>%  # labels setzen
  pivot_longer(everything(),names_to = "variable") %>% # ins long shape bringen
  count(variable,value) # zählen von variable und ausprägung

# als flextable
etb18_kap14 %>%  
  select(S1,m1202) %>% 
  mutate(S1 = factor(S1,levels = 1:2, labels = c("Männer","Frauen")),
           m1202 = factor(m1202, levels = 1:4,labels = c("ohne","dual/schul.","Aufst.","FH/Uni"))) %>% 
  pivot_longer(everything(),names_to = "variable") %>% 
  count(variable,value) %>% 
  flextable()

kat_ft <- 
  etb18_kap14 %>%  
    select(S1,m1202) %>% 
    mutate(S1 = factor(S1,levels = 1:2, labels = c("Männer","Frauen")),
           m1202 = factor(m1202, levels = 1:4,labels = c("ohne","dual/schul.","Aufst.","FH/Uni"))) %>% 
    pivot_longer(everything(),names_to = "variable") %>% 
    count(variable,value)  %>% 
    flextable() %>% 
    border_remove() %>% 
    hline(i = 2) %>% 
    italic(j = 2) %>% 
    autofit()

# Speichern mit save_as_docx
save_as_docx(kat_ft,path = "./results/kat_table.docx")

# export nach excel

library(xlsx)
ausz_df <- 
  etb18_kap14 %>%  
    select(S1,m1202) %>% 
    mutate(S1 = factor(S1,levels = 1:2, labels = c("Männer","Frauen")),
           m1202 = factor(m1202, levels = 1:4,labels = c("ohne","dual/schul.","Aufst.","FH/Uni"))) %>% 
    pivot_longer(everything(),names_to = "variable") %>% 
    count(variable,value) 

write.xlsx(data.frame(ausz_df),
           file = "./results/Auszählung.xlsx", 
           sheetName = "Tabellenblatt 1",row.names = FALSE)

# Regressionstabellen ----------------

## beispieldaten
etb18_kap14_reg_df <- 
  etb18_kap14 %>%
  mutate(S1 = factor(S1,levels = 1:2, labels = c("Männer","Frauen")),
         m1202 = factor(m1202, levels = 1:4,labels = c("ohne","dual/schul.","Aufst.","FH/Uni")))

m1 <- lm(F518_SUF ~ az + S1, data = etb18_kap14_reg_df)
m2 <- lm(F518_SUF ~ az + S1 + m1202, data = etb18_kap14_reg_df)

summary(m2)

## Modelsummary als flextable -----
modelsummary(list("Modell 1"=m1,"Modell 2"=m2))

modelsummary(list("Modell 1"=m1,"Modell 2"=m2),
             output = "flextable",
             gof_omit = "IC|Log|RMS",
             coef_rename = c("(Intercept)"="Intercept",
                             "S1Frauen" = "Frauen",
                             "m1202dual/schul." = "Duale/Schulische Ausbildung",
                             "m1202Aufst." = "Aufstiegsfortbildung",
                             "m1202FH/Uni" = "FH/Uni-Abschluss"),
             stars = T,
             fmt =2)

## ref-kategorien erstellen -----

data.frame(x = c(1,2,4,5))


library(tibble)

tribble( ~ term,    ~ "Modell 1",  ~ "Modell 2",
         "Männer",    'ref.',         'ref.')

ref_rows <- tribble( ~ term,    ~ "Modell 1",  ~ "Modell 2",
                     "Männer",    'ref.',         'ref.')


attr(ref_rows, 'position') <- 5 # Zeile angeben 
# einfügen mit add_rows =
modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2),
  output = "flextable",
  gof_omit = "IC|Log|RMS",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "S1Frauen" = "Frauen",
    "m1202dual/schul." = "Duale/Schulische Ausbildung",
    "m1202Aufst." = "Aufstiegsfortbildung",
    "m1202FH/Uni" = "FH/Uni-Abschluss"
  ),
  add_rows = ref_rows,
  stars = T,
  fmt = 2
)

## mehrere ref-kats -----
ref_rows2 <- tribble(~term,    ~"Modell 1",  ~"Modell 2",
                "Männer",    'ref.',   'ref.',
                "keine Ausbildung",    '',   'ref.',
                )
attr(ref_rows2, 'position') <- c(5,8) # Zeile angeben 

modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2),
  output = "flextable",
  gof_omit = "IC|Log|RMS",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "S1Frauen" = "Frauen",
    "m1202dual/schul." = "Duale/Schulische Ausbildung",
    "m1202Aufst." = "Aufstiegsfortbildung",
    "m1202FH/Uni" = "FH/Uni-Abschluss"
  ),
  add_rows = ref_rows2,
  stars = T,
  fmt = 2
)

###  Umbenennen sparen mit `coef_rename =`-------

# Mehr dazu hier https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html#coef_rename
 rename_function <- function(old_names) {
   new_names <- 
     gsub("m1202", "Ausbildung: ", old_names) %>% 
     gsub("S1", "Geschlecht: ",.) %>% 
     gsub("az", "Arbeitszeit (h) ",.)
   
   return(setNames(new_names, old_names))
 }
 
 ## diese Funktion dann in modelsummary verwenden:
modelsummary(list("Modell 1" = m1, "Modell 2" = m2),gof_omit = "IC|Log|RMS", coef_rename = rename_function)
                   

# als objekt speichern
regtab2 <- 
  modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2),
  output = "flextable",
  gof_omit = "IC|Log|RMS",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "S1Frauen" = "Frauen",
    "m1202dual/schul." = "Duale/Schulische Ausbildung",
    "m1202Aufst." = "Aufstiegsfortbildung",
    "m1202FH/Uni" = "FH/Uni-Abschluss"
  ),
  add_rows = ref_rows2,
  stars = T,
  fmt = 2) %>% autofit()

regtab3 <- 
  regtab2 %>% 
  autofit() %>% 
  italic(i = ~ `Modell 2` == "ref.",j =2:3)

# export
save_as_docx(regtab3,path = "./results/regressionstabelle.docx")


# alles in einem Rutsch -------
# install.packages("officer")
library(officer)

# read_docx("pfad/zur/Vorlage/DIN_A4_Vorlage.docx") %>%
read_docx("C:/Users/filse/Documents/Benutzerdefinierte Office-Vorlagen/Vorlage_Arial.docx") %>%
  body_add_flextable(., value = met_ft ) %>% # flextable met_ft einfügen
  body_add_par(.,"") %>% # leeren Absatz einfügen
  body_add_flextable(., value = kat_ft ) %>% # flextable cat_ft einfügen
  body_add_par(.,"") %>% # leeren Absatz einfügen
  body_add_flextable(., value = regtab3 ) %>% # flextable regtab2 einfügen
  print(target = "./results/Tables.docx")

# Kreuztabellen ------
etb18_kap14 %>% 
  tabyl(m1202,S1) 

  etb18_kap14 %>% 
    tabyl(m1202,S1) %>%
    adorn_totals(where = c("row","col"))
  
tabyl1 <-   
  etb18_kap14 %>% 
    tabyl(m1202,S1) %>%
    adorn_totals(where = c("row","col"))
class(tabyl1)
