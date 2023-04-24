# ------------------- #
# Kapitel 8: Regression
# ------------------- #
library(tidyverse)

# Beispieldatensatz -------
dat1 <- data.frame(id   = 1:8,
                   var1 = c(2,1,2,5,7, 8, 9,5),
                   var2 = c(2,2,1,9,7, 4,25,5),
                   educ = c(3,1,2,2,1, 3, 2,-1),
                   gend = c(2,1,1,2,1,2,1,2),
                   x    = c(2,1,2,4,1,NA,NA,NA) )
dat1

# lm() -------------

lm(var2 ~ var1, data = dat1)

lm(dat1$var2 ~ dat1$var1)

# als  objekt speichern
m1 <- lm(var2 ~ var1, data = dat1)  
summary(m1) # übersichtstabelle

View(m1) ## regressionsobjekt ansehen

# ausgangsdaten
m1$model
# Koeffizienten aufrufen
m1$coefficients


summary(m1)
summary(m1)$coefficients

# visualisieren ------
# Ausreißer finden mit ggplot

# geom_smooth

library(ggplot2)
ggplot(data = dat1, aes(x = var1, y = var2)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm")

# farben von smooth anpassen:
ggplot(dat1, aes(x = var1, y = var2)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", color = "darkblue" , fill = "lightskyblue", size = .65)  

ggplot(dat1, aes(x = var1, y = var2)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", color = "darkblue" , fill = "lightskyblue", size = .65)  +
  geom_text(#data = . %>% filter(var2 > 20), 
            aes(y = var2+3, label = id), size = 5, color = "red")



dat1 %>% filter(id==7)
# Übung

# Modelle nur für manche Fälle berechnen ------------ 
dat1_u20 <- dat1 %>% filter(var2<20)
m2a <- lm(var2~ var1, data = dat1_u20)
summary(m2a)

m2b <- lm(var2~ var1, data = dat1 %>% filter(var2<20))
summary(m2b)
summary(m1)

# Regressionstabellen --------
## install.packages("modelsummary")

library(modelsummary)

modelsummary(list(m1,m2a,m2b))



modelsummary(list(m1,m2a,m2b),output = "markdown")

# Namen mit = angeben, AIC usw ausblenden
modelsummary(list("m1"=m1,"m2a"=m2a,"m2b"=m2b),stars = T,gof_omit = "IC|RM|Log")

modelsummary(list("m1"=m1,"m2a"=m2a,"m2b"=m2b),
             stars = T,gof_omit = "IC|RM|Log",output = "markdown")

# kategoriale UVs -------------
dat1
# kategoriale Variablen als factor verwenden
m3 <- lm(var2~educ, dat1) # funktioniert nicht richtig
summary(m3)

m3 <- lm(var2 ~ as.factor(educ), dat1)
summary(m3)

dat1$ed_fct <- factor(dat1$educ, 
                      levels = 1:3,
                      labels = c("basic","medium","high"))
dat1
levels(dat1$ed_fct)

m3 <- lm(var2 ~ ed_fct, dat1)
summary(m3)

# referenzkategorie ändern mit relevel:
dat1$ed_fct <- relevel(dat1$ed_fct,ref = "medium")
levels(dat1$ed_fct)

m3b <- lm(var2 ~ ed_fct, dat1)
summary(m3b)

m4 <- lm(var2 ~ ed_fct  + var1, dat1)
summary(m4)
modelsummary(m4)

# latex export:
# write(modelsummary(m4,output = "latex"),file = "./results/table.tex")
# Übung

# Koeffizientenplots ------------
# library(modelsummary)
modelplot(m4)

modelplot(list("Modell 1"=m1,
               "Modell 4"=m4))

modelplot(list("Modell 1"=m1,
               "Modell 4"=m4),
          coef_map = c("var1" = "Name für var1",
                       "ed_fcthigh"  = "Höhere Bildung",
                       "ed_fctbasic" = "Grundlegende Bildung"
                          ))

modelplot(list("Modell 1"=m1,
               "Modell 4"=m4),
          coef_map = c("var1" = "Name für var1",
                       "ed_fcthigh"  = "Höhere Bildung",
                       "ed_fctbasic" = "Grundlegende\nBildung")) + # \n fügt einen Zeilenumbruch ein
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey40") +  # 0-Linie einfügen
  scale_color_manual(values = c("orange","navy")) 

modelplot(list("Modell 1"=m1,
               "Modell 4"=m4),
          coef_map = c("var1" = "Name für var1",
                       "ed_fcthigh"  = "Höhere Bildung",
                       "ed_fctbasic" = "Grundlegende\nBildung")) + # \n fügt einen Zeilenumbruch ein
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey40") +  # 0-Linie einfügen
  scale_color_manual(values = c("orange","navy"),
                     breaks = c("Modell 1", "Modell 4"),
                     labels = c("Te1","YbC sadsa")) +
  labs(title = "TEST TEST TITLE")


  theme_grey(base_size = 15,base_family = "mono") 

# Übung