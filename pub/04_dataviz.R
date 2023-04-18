# ------------------- #
# Kapitel 4: data viz
# ------------------- #

library(haven)
library(tidyverse)
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta", col_select = c("az","S1","zpalter","m1202"))

# ggplot2 --------------
etb18 %>%
  slice(1:100) %>% 
  mutate(S1_fct = factor(S1, levels = 1:2, labels = c("Männer","Frauen")),
         zpalter = ifelse(zpalter > 100,NA,zpalter)) %>%
ggplot(aes(x = zpalter, y = az)) +
  geom_point(aes(color = S1_fct)) +
  theme_minimal() +
  labs(color = "Geschlecht", y = "Arbeitszeit/Woche",
       x = "Alter") +
  scale_color_manual(values = c("lightskyblue4","navy"))


etb18 %>% select(az,S1,zpalter) %>% head()

attributes(etb18$az)
etb18$zpalter[etb18$zpalter>100] <- NA # missing in zpalter mit NA überschreiben
etb18_small <- etb18 %>% slice(1:100)

# ggplot Aufbau ------
# library(ggplot2) # teil des tidyverse

ggplot(data = etb18_small)

ggplot(data = etb18_small, aes(x = zpalter, y = az))

ggplot(data = etb18_small, aes(x = zpalter, y = az)) + geom_point()

ggplot(data = etb18_small, aes(x = zpalter, y = az)) + geom_point(color = "orange")

# Hier: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf findet sich eine Übersicht mit allen Farbnamen, die verstanden werden

## Farbe nach Geschlecht ------
# problem bei gelabelten Variablen
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = S1 )) + geom_point()

## numerische Variablen ergeben einen Farbverlauf als Legende ------
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = as.numeric(S1))) +  geom_point()
## factor Variablen ergeben eine diskrete Farblegende: ------
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = as.factor(S1)))    + geom_point()
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = as.character(S1))) + geom_point()

## scale_color_manual ------
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"))

# labeln 
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") )

ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point() + 
  scale_color_manual(values = c("#F59400","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") )


## labs() ------
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(color = "Geschlecht", 
       title = "Arbeitszeit und Alter",
       x = "Alter",
       y = "Arbeitszeit/Woche",
       caption = "Quelle: ETB 2018",
       subtitle = "Nach Geschlecht"
       ) 

# einmal alles ---------------
etb18_small$m1202[etb18_small$m1202<0] <- NA
ggplot(data = etb18_small, aes(x = zpalter, y = az, 
                               color = factor(S1),
                               shape = factor(m1202))) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("lightskyblue3","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  scale_shape_manual(values = c(15:18),breaks = c(1:4), 
                     labels = c("ohne Aus", "duale Ausb.","Aufstiegsfortb.","FH/Uni")) +
  labs(color = "Geschlecht", 
       shape = "Ausbildung",
       fill = "Geschlecht",
       y = "Arbeitszeit/Woche",
       x = "Alter",
       title = "Arbeitszeit und Alter",
       subtitle = "Nach Geschlecht",
       caption = "Quelle: ETB 2018"
       ) 

# fertiges objekt speichern -----------
plot_objekt1 <- 
  ggplot(data = etb18_small, aes(x = zpalter, y = az,color = factor(S1))) +
  geom_point(size = 2) 

ggsave(plot = plot_objekt1,
       filename = "./pub/plot1.png",
      dpi = 800, # auflösung
      width = 9, height = 7, # falls angepasst werden soll
      )

## Übung -----

# metrische Variablen ----------
## boxplot -----
ggplot(data = etb18_small, aes(y = az)) + geom_boxplot()
ggplot(data = etb18_small, aes(x = az)) + geom_boxplot()


ggplot(data = etb18_small, aes(y = az, x = factor(S1))) + geom_boxplot()

## Histogramm ----
ggplot(data = etb18_small, aes(x = az)) + geom_histogram()  
ggplot(data = etb18_small, aes(y = az)) + geom_histogram()  

ggplot(data = etb18_small, aes(x = az)) + geom_histogram(fill = "sienna1")  
ggplot(data = etb18_small, aes(x = az)) + geom_histogram(color = "sienna1", fill = "navy")  

ggplot(data = etb18_small, aes(x = az, fill = factor(S1))) + geom_histogram() 

ggplot(data = etb18_small, aes(x = az, fill = factor(S1))) + 
   geom_histogram(position = position_dodge()) 

### mit relativen Anteilen/Prozent ----
ggplot(etb18_small, aes(x = az, fill = factor(S1),
                        y = after_stat(count / sum(count)) ) ) + 
  geom_histogram(position = position_dodge())

## color statt fill
ggplot(data = etb18_small, aes(x = az, color = factor(S1))) + 
  geom_histogram(position = position_dodge()) 

ggplot(data = etb18_small, aes(x = az, fill = factor(S1))) +  
  geom_histogram(position = position_dodge()) +
  scale_fill_manual(values = c("sienna1","dodgerblue4"),
                    breaks = 1:2, labels = c("Männer","Frauen")) +
  labs(fill = "Geschlecht")

ggplot(data = etb18, aes(x = az, fill = factor(S1))) + 
  geom_histogram( color = "grey50",position = position_dodge()) + 
  scale_fill_viridis_d(option = "E",labels = c("Männer","Frauen"))

## density-plot ------
ggplot(data = etb18, aes(x = az,fill = factor(S1))) + 
  geom_density(alpha = .85) + 
  scale_fill_manual(values = c("sienna1","dodgerblue4"),
                    breaks = 1:2, labels = c("Männer","Frauen")) +
  labs(fill = "Geschlecht") 

## Übung -----

# Kategoriale Merkmale -------

etb18$m1202[etb18$m1202<0] <- NA # missings ausschließen

## Ausgangsdaten: Auszählung ----------
etb18 %>% 
  count(S1,m1202) %>% 
  filter(!is.na(m1202))

# Auszählung in ggplot schicken mit der Pipe:
etb18 %>% 
  count(S1,m1202) %>% 
  filter(!is.na(m1202)) %>% 
  ggplot(data = ., aes(x = m1202, y = n, fill = factor(S1))) +
  geom_col(position=position_dodge()) 

## Anteile innerhalb Männer/Frauen ----
etb18 %>% 
  filter(!is.na(m1202)) %>% 
  count(S1,m1202) %>% 
  group_by(S1) %>% 
  mutate(pct_gender = prop.table(n)) 

## Anteile innerhalb Männer/Frauen in den ggplot füttern ----
etb18 %>% 
  filter(!is.na(m1202)) %>% 
  count(S1,m1202) %>% 
  group_by(S1) %>% 
  mutate(pct_gender = prop.table(n)) %>% 
  ggplot(data = ., aes(x = m1202, y = pct_gender, fill = factor(S1))) +
  geom_col(position=position_dodge()) 

## x-Achse labeln -----
etb18 %>% 
  filter(!is.na(m1202)) %>% 
  count(S1,m1202) %>% 
  group_by(S1) %>% 
  mutate(pct_gender = prop.table(n)) %>% 
  ggplot(data = ., aes(x = m1202, y = pct_gender, fill = factor(S1))) +
  geom_col(position=position_dodge())  +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_x_continuous(breaks = 1:4 , 
                     labels = c("ohne Ausb.", "duale Ausb.","Aufstiegsfortb.","FH/Uni")) 

 
## weitere Optionen auf Säulendiagramm anwenden -----
etb18 %>% 
  filter(!is.na(m1202)) %>% 
  count(S1,m1202) %>% 
  group_by(S1) %>% 
  mutate(pct_gender = prop.table(n)) %>% 
  ggplot(data = ., aes(x = m1202, y = pct_gender, fill = factor(S1))) +
  geom_col(position=position_dodge())  +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))  + 
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen")) +
  scale_x_continuous(breaks = 1:4 , 
                     labels = c("ohne Ausb.", "duale Ausb.","Aufstiegsfortb.","FH/Uni")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Ausbildungsabschlüsse nach Geschlecht",
       subtitle = "Relative Häufigkeiten",
       caption = "Quelle: ETB 2018",
       x = "Ausbildung",
       y = "Relative Häufigkeit",
       fill = "Geschlecht" ) 

## ohne vorherige Auszählung: geom_bar() ---
etb18 %>% 
  filter(m1202 > 0) %>% 
  ggplot(data = ., aes(x = m1202, fill = factor(S1))) +
  geom_bar(position=position_dodge()) 

## auch hier wieder alle Optionen von oben verwendbar:
etb18 %>% 
  filter(m1202 > 0) %>% 
  ggplot(data = ., aes(x = m1202, fill = factor(S1))) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values = c("navajowhite","navy"),
                  breaks = c(1,2), labels = c("Männer", "Frauen")) +
  scale_x_continuous(breaks = 1:4 , 
                     labels = c("ohne Ausb.", "duale Ausb.","Aufstiegsfortb.","FH/Uni")) +
  labs(title = "Ausbildungsabschlüsse nach Geschlecht",
       subtitle = "Relative Häufigkeiten",
       caption = "Quelle: ETB 2018",
       x = "Ausbildung",
       y = "Relative Häufigkeit",
       fill = "Geschlecht" ) +
  theme_classic()
