# ------------------- #
# Kapitel 8: Regression
# Lösung
# ------------------- #
library(tidyverse)

# Verwenden Sie folgenden Subdatensatz der ETB2018:
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta", 
                         col_select = c("intnr","zpalter","az","F518_SUF","m1202","S1"))  
etb_reg1 <- etb18 %>% filter(F518_SUF < 99990,intnr< 200000)

# Übung 1 -----
# Erstellen Sie ein Objekt mod1 mit einem linearen Regressionsmodell (lm) 
# mit F518_SUF (Monatsbrutto in EUR) als abhängiger und az (Arbeitszeit in Stunden) als unabhängiger Variable! (siehe hier)

lm(F518_SUF ~ az, data = etb_reg1)

m2 <- lm(F518_SUF ~ az, data = etb_reg1)
summary(m2)

# Betrachten Sie Ergebnisse mod1 - was können Sie zum Zusammenhang zwischen F518_SUF und az erkennen?

# Visualisieren Sie die Regressionsgerade mit {ggplot2}.
ggplot(data = etb_reg1,aes(x= az, y = F518_SUF)) +
  geom_point()


ggplot(data = etb_reg1,aes(x= az, y = F518_SUF)) +
  geom_point() +
  geom_smooth(method = "lm")

# Sehen Sie Ausreißer im Scatterplot? Markieren Sie diese mit Hilfe der Variable intnr und geom_text().
ggplot(data = etb_reg1,aes(x= az, y = F518_SUF)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_text(data = . %>% filter(F518_SUF > 20000),
            aes(label = intnr),
            color = "orange")

# label verschieben
ggplot(data = etb_reg1,aes(x= az, y = F518_SUF)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_text(data = . %>% filter(F518_SUF > 20000),
            aes(label = intnr, x = az -8),
            color = "orange")

etb_reg1 %>% filter(F518_SUF > 20000) %>% select(intnr, F518_SUF, az, zpalter, S1)

# labels für alle Beobachtungen 
ggplot(data = etb_reg1,aes(x= az, y = F518_SUF)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_text(aes(label = intnr, x = az - 3),
            color = "orange")



# Übung 2 ----
# ausgangsmodell
m2 <- lm(F518_SUF ~ az, etb_reg1)

# zwei möglichkeiten, nur fälle unter 20.000EUR zu analysieren:
##subdatensatz
etb_reg1_u20k <- etb_reg1 %>% filter(F518_SUF < 20000)
m3a <- lm(F518_SUF ~ az, etb_reg1_u20k)
## filter im data-Argument
m3b <- lm(F518_SUF ~ az, etb_reg1 %>% filter(F518_SUF < 20000))



library(modelsummary)
modelsummary(list("Modell mit allen"=m2,
                  "Modell u20k ver1"=m3a,
                  "Modell u20k ver2"=m3b),
             stars = T,gof_omit = "IC|RM|Log",output = "markdown")

modelsummary(list("Modell mit allen"=m2,
                  "Modell u20k ver1"=m3a,
                  "Modell u20k ver2"=m3b),
             stars = T,gof_omit = "IC|RM|Log")


## Bonus: grafische Darstellung
ggplot(data = etb_reg1,aes(x= az, y = F518_SUF)) +
  geom_point() +
  geom_smooth(method = "lm",formula = "y~x") + 
  geom_smooth(data= . %>% filter(F518_SUF < 20000),
              method = "lm", color = "orange", fill = "coral")

# Übung 3 ------

## kategoriale unabhängige Variable ------
etb_reg1$m1202_fct <-  
  factor(etb_reg1$m1202,levels = 1:4, labels = c("ohne","dual","Aufstieg","FH/Uni"))

m4 <- lm(F518_SUF ~ m1202_fct,data = etb_reg1)
summary(m4)

## Aufstiegsfort. als Referenz -----
etb_reg1$m1202_fct <-  relevel(etb_reg1$m1202_fct,ref = "Aufstieg")

m4.2 <- lm(F518_SUF ~ m1202_fct,data = etb_reg1)
summary(m4.2)


# Modellsummary
modelsummary(list("m4"=m4,"m4.2"=m4.2),output = "markdown")

## mehre UVs -----

etb_reg1$m1202_fct <-  relevel(etb_reg1$m1202_fct,ref = "ohne")
m5 <- lm(F518_SUF ~ m1202_fct + az,data = etb_reg1)
summary(m5)
modelsummary(list("m4"=m4,"m5"=m5),output = "markdown")

modelplot(list(m4,m5))
modelplot(list(m4,m5),coef_map = c("m1202_fctAufstieg"="Aufstiegsfortbildung"))

modelplot(list(m4,m5)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey40") +  # 0-Linie einfügen
  scale_color_manual(values = c("orange","navy")) +
  theme_grey(base_size = 15,base_family = "mono") 



## individuell mit tidy()
tidy(m5, conf.int = TRUE) %>% 
  mutate(term = str_replace(term, "m1202_fct", "Education: "))


tidy(m5, conf.int = TRUE) %>% 
  mutate(term = str_replace(term, "m1202_fct", "Education: ")) %>% 
  ggplot(aes(y = term, x = estimate)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "navy") +
  geom_errorbarh(aes(xmin = conf.low, xmax  = conf.high), height = .1) + 
  geom_point(color = "navy", shape = 18,size = 8) +
  theme_minimal(base_size = 16)


