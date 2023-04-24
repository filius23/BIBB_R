

# beispieldatensatz 
dat1 <- data.frame(studs = c(19173,5333,15643), 
                   profs = c(322,67,210),
                   gegr  = c(1971,1830,1973),
                   prom_recht = rep(TRUE,3),
                   uni = c("Uni Bremen","Uni Vechta", "Uni Oldenburg"))
dat2 <- data.frame(studs = c(14954,47269 ,23659,9415 ,38079), 
                   profs = c(250,553,438 ,150,636),
                   prom_recht = c(FALSE,TRUE,TRUE,TRUE,FALSE),
                   gegr  = c(1971,1870,1457,1818,1995),
                   uni = c("FH Aachen","RWTH Aachen","Uni Freiburg","Uni Bonn","FH Bonn-Rhein-Sieg"))
dat3 <- bind_rows(dat1,dat2)

dat1
rm(dat1) #löscen

# Paket: tidyverse
library(tidyverse)

# Variablen hinzufügen mit mutate() -------
dat3 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs))
# Variablen zusammenfassen mit summarise() ----------
dat3 %>%
  summarise(m_studs = mean(studs),
            m_profs = mean(profs))

mean(dat3$studs)

# gruppierte Berechnungen mit .by = oder group_by()
dat3 %>%
  summarise(m_studs2 = mean(studs),.by = prom_recht) 

dat3 %>%
  group_by(prom_recht) %>% 
  summarise(m_studs2 = mean(studs) )

# across() um mehrere Variablen auf gleich Weise zu bearbeiten -----
dat3 %>%
  summarise(across(.cols = matches("studs|profs"),.fns = ~mean(.x)))


# function() für eigene Funktionen -------------
stdize <- function(x){
  stdx <- (x - mean(x,na.rm = T))/sd(x,na.rm = T)
  return(stdx)
}

dat3 %>% 
  mutate(studs = stdize(studs))

# ggplot() für Grafiken ---------

ggplot(data = dat3,aes(x = studs, y = profs, color = prom_recht)) + 
  geom_point() + 
  scale_color_manual(values = c("orange","slateblue")) +
  theme_bw()
