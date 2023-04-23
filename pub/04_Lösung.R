# ------------------- #
# Kapitel 4: data viz
# Lösung
# ------------------- #

library(tidyverse)

etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta", col_select = c("h1216d","S2_j","S1","m1202","F518_SUF","nt","gkpol"))
etb18_small <- etb18 %>% filter(S2_j < 9999,h1216d>0,F518_SUF<99998) %>% slice(1:150)


# Übung 1 -------

ggplot(data= etb18_small, aes(x=S2_j,y=h1216d)) + geom_point() 

ggplot(data= etb18_small, aes(x=S2_j,y=h1216d, color = factor(S1))) + geom_point()



ggplot(data= etb18_small, aes(x=S2_j,y=h1216d, color = factor(S1))) + geom_point() + 
  scale_color_manual(values = c("goldenrod1","dodgerblue4"))

ggplot(data= etb18_small, aes(x=S2_j,y=h1216d, color = factor(S1))) + 
  geom_point() +
  scale_color_manual(values = c("goldenrod1","dodgerblue4"),
                     breaks = c(1,2),
                     labels = c("M","W")) + 
  labs(x= "Geburtsjahr",
       y= "Ausbildungsdauer",
       color = "Geschlecht") +
  theme_classic(base_size = 12) ### mehr im Anhang

## warning umgehen
ggplot(data= etb18_small, aes(x=as.numeric(S2_j),y=as.numeric(h1216d), color = factor(S1))) + 
  geom_point() +
  scale_color_manual(values = c("goldenrod1","dodgerblue4"),
                     breaks = c(1,2),
                     labels = c("M","W")) + 
  labs(x= "Geburtsjahr",
       y= "Ausbildungsdauer",
       color = "Geschlecht") 

  
  ## daten mit pipe
  etb18_small %>% 
  ggplot(data = ., 
         aes(x=S2_j,y=h1216d)) + 
    geom_point()
  

# Übung 2 --------

# Erstellen Sie einen Boxplot/Histogramm für die Verteilung des Einkommens (F518_SUF).
ggplot(etb18_small, aes(y = F518_SUF)) + geom_boxplot()
ggplot(etb18_small, aes(x = F518_SUF)) + geom_histogram()

# Passen Sie diesen Boxplot so an, dass die Einkommensverteilungen für Männer und Frauen getrennt dargestellt werden
ggplot(etb18_small, aes(y = F518_SUF, x = factor(S1) )) + geom_boxplot()
# direkt im factor() labels vergeben
ggplot(etb18_small, aes(y = F518_SUF, 
                        x = factor(S1,levels = 1:2,labels = c("M","W")) )) + 
  geom_boxplot()


## Knobelaufgabe:
ggplot(etb18_small, aes(y = F518_SUF, 
                        x = factor(S1),
                        color = factor(S1) ### color funktioniert auch bei boxplot
                        )) + 
  geom_boxplot()


ggplot(etb18_small, aes(y = F518_SUF, 
                        x = factor(S1),
                        fill = factor(S1),  #auch fill 
                        color = factor(S1) ### color funktioniert auch bei boxplot
)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("orange","green"))

# Erstellen Sie ein Histogramm, ebenfalls für die Einkommensverteilung und mit getrennten Farben für Männer und Frauen
ggplot(etb18_small, aes(x = F518_SUF ))  + 
  geom_histogram()

# Farben nach Geschlecht vergeben:
ggplot(etb18_small, aes(x = F518_SUF,fill = factor(S1) ))  + 
  geom_histogram(position = position_dodge())

ggplot(etb18_small, aes(x = F518_SUF,fill = factor(S1) ))  + 
  geom_histogram(position = position_dodge()) +
  scale_fill_manual(values = c("goldenrod1","dodgerblue4"),
                     breaks = c(1,2),
                     labels = c("M","W")) +
  labs(fill = "Gender")


## y-Achse & x-Achse labeln -----
ggplot(etb18_small, aes(x = F518_SUF ))  + 
  geom_histogram() +
  labs(y = "mein label", x = "test")


# Verändern Sie die Farben der Balken mit Hilfe von scale_fill_manual oder scale_fill_brewer oder scale_fill_viridis (Siehe Abschnitte Farben und ColorBreweR und viridis unter “weitere Optionen”)
ggplot(etb18_small, aes(x = F518_SUF,fill = factor(S1) ))  + 
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("goldenrod1","dodgerblue4"),
                    breaks = c(1,2),
                    labels = c("M","W")) 


# Übung3 ------
etb18_small %>% count(gkpol)


etb18_small %>% 
  count(gkpol) %>%
  ggplot(data = ., aes(x = gkpol, y = n)) +
  geom_col(position = position_dodge())


etb18_small %>% 
  count(gkpol,nt) %>%
  ggplot(data = ., aes(x = gkpol, y = n, fill = factor(nt))) +
  geom_col(position = position_dodge())

# labels und farbe
etb18_small %>% 
  count(gkpol,nt) %>%
  ggplot(data = ., aes(x = gkpol, y = n, fill = factor(nt))) +
  geom_col(position = position_dodge()) +
  scale_x_continuous(breaks = 1:7,
                     labels = c("<2k", "2k bis <5k", "5k bis <20k", "20k bis <50k", 
                                "50k bis <100k", "100k bis <500k", "500k und mehr")) +
  scale_fill_manual(values = c("slateblue4","orangered3"),
                    breaks = c(0,1),
                    labels = c("ja","nein"))  +
  labs(fill = "Nebentätigkeit",
       x = "Wohnortgröße",
       y = "Häufigkeit")


## auf balkendiagramm drehen --> ?
etb18_small %>% 
  count(gkpol,nt) %>%
  ggplot(data = ., aes(y = gkpol, x = n)) +
  geom_col(orientation  = "y")  ## orientation
