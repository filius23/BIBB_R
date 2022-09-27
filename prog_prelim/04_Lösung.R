# ------------------- #
# Kapitel 4: data viz
# Lösung
# ------------------- #

library(haven)
library(tidyverse)
library(patchwork)

etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta", col_select = c("h1216d","S2_j","S1","m1202","F518_SUF"))
etb18_small <- etb18 %>% filter(S2_j < 9999,h1216d>0,F518_SUF<99998) %>% slice(1:150)


attributes(etb18$h1216d)
# Übung 1 -------



ggplot(data= etb18_small, aes(x=S2_j,y=h1216d)) + geom_point()
ggplot(data= etb18_small, aes(x=S2_j,y=h1216d, color = factor(S1))) + geom_point()
ggplot(data= etb18_small, aes(x=S2_j,y=h1216d, color = factor(S1))) + geom_point() +
  scale_color_manual(values = c("goldenrod1","dodgerblue4"))

ggplot(data= etb18_small, aes(x=S2_j,y=h1216d, color = factor(S1))) + geom_point() +
  scale_color_manual(values = c("goldenrod1","dodgerblue4"),
                     breaks = c(1,2),
                     labels = c("M","W")) + 
  labs(x= "Geburtsjahr",
       y= "Ausbildungsdauer",
       color = "Geschlecht") +
  theme_classic() ### mehr im Anhang


# Übung 2 --------

# Erstellen Sie einen Boxplot für die Verteilung des Einkommens (F518_SUF).
ggplot(etb18_small, aes(y = F518_SUF)) + geom_boxplot()

# Passen Sie diesen Boxplot so an, dass die Einkommensverteilungen für Männer und Frauen getrennt dargestellt werden
ggplot(etb18_small, aes(y = F518_SUF, x = factor(S1) )) + geom_boxplot()

## übrigens
ggplot(etb18_small, aes(y = F518_SUF, x = factor(S1),color = factor(S1) )) + geom_boxplot()


# Erstellen Sie ein Histogramm, ebenfalls für die Einkommensverteilung und mit getrennten Farben für Männer und Frauen
# Was passiert wenn Sie für das Histogramm statt color = anstelle von fill = verwenden?
# Verändern Sie die Farben der Balken mit Hilfe von scale_fill_manual oder scale_fill_brewer oder scale_fill_viridis (Siehe Abschnitte Farben und ColorBreweR und viridis unter “weitere Optionen”)
# Ändern Sie die Darstellung in einen density-Plot



