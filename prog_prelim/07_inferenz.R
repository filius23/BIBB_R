
library(tidyverse)
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")

## install.packages("broom") # Ergebnisse in data.frame umwandeln
## install.packages("effectsize") # u.a. Cohen's D berechnen
## install.packages("survey") # Gewichtung

# t.test ------------
etb18$zpalter[etb18$zpalter>100] <- NA
t.test(etb18$zpalter~etb18$S1)

t.test(etb18$zpalter~etb18$S1,alternative = "two.sided")
t.test(etb18$zpalter~etb18$S1,alternative = "less")
t.test(etb18$zpalter~etb18$S1,alternative = "greater")

library(effectsize)
cohens_d(etb18$zpalter~etb18$S1)

attributes(etb18$F231)$label
etb18$F231[etb18$F231>990] <- NA

cor.test(etb18$zpalter,etb18$F231,method = "pearson", use = "pairwise.complete.obs")
c1 <- cor.test(etb18$zpalter,etb18$F231,method = "pearson", use = "pairwise.complete.obs")

# Übung -----

# Korrelation ------------
## install.packages("correlation")

library(correlation)
etb18 %>% select(zpalter,F231,az) %>% 
  correlation() %>% 
  summary(.)

etb18 <- 
  etb18 %>% 
  mutate(educ = case_when(S3 %in% 2:4 ~ 1,
                          S3 %in% 5:6 ~ 2,
                          S3 %in% 7:8 ~ 3),
         F600_12 = ifelse(F600_12 > 4,NA,F600_12))
etb18 %>% count(S3,educ)


cor.test(etb18$educ,etb18$F600_12,method = "spearman", use = "pairwise.complete.obs")

cor.test(etb18$educ,etb18$F600_12, method = "kendall", use = "pairwise.complete.obs")

etb18$F204[etb18$F204>4] <- NA
etb18 %>% filter(!is.na(F204)) %>% count(F204,S1)
attributes(etb18$F204)$label

xtabs(~ F204 + S1, data = etb18)
tab1 <- xtabs(~ F204 + S1, data = etb18)
chisq.test(tab1)

effectsize::cramers_v(etb18$F204,etb18$S1)

# Tests als objekte ------
ttest1 <- t.test(etb18$zpalter~etb18$S1,alternative = "two.sided")
ttest1$statistic # t-Wert
ttest1$p.value # p-Wert

library(broom)
tidy(ttest1)

corr1 <- cor.test(etb18$zpalter,etb18$F231,method = "pearson", use = "pairwise.complete.obs")
tidy(corr1)

# Übung ------

## ttest ploten ------
library(ggplot2)
ttest_df <- tidy(corr1)
ggplot(ttest_df,aes(y="t.test",x=estimate)) +
  geom_vline(aes(xintercept = 0),linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low,xmax = conf.high), color = "navy")

ttest_m <- 
  cor.test(etb18$zpalter[etb18$S1==1],
           etb18$F231[etb18$S1==1],method = "pearson", use = "pairwise.complete.obs")

ttest_w <- 
  cor.test(etb18$zpalter[etb18$S1==2],
           etb18$F231[etb18$S1==2],method = "pearson", use = "pairwise.complete.obs")

test_df2 <- bind_rows(tidy(ttest_m),
                      tidy(ttest_w), 
                      .id = "S1") # Variable S1 erstellen als Identifikator

test_df2

ggplot(test_df2,aes(y=S1,x=estimate)) +
  geom_vline(aes(xintercept = 0),linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low,xmax = conf.high), color = "navy")

etb18 %>% 
  count(S1,m1202,wt = gew2018_hr17)

etb18 %>% summarise(mean = weighted.mean(zpalter,w = gew2018))

# Gewichtung ----------
library(survey)
etb18_weighted <- svydesign(id      = ~intnr,
                            weights = ~gew2018,
                            data    = etb18)
svytable(~S1+m1202,etb18_weighted) # syntax wie xtabs()
xtabs(~S1+m1202,etb18)
svymean(~zpalter, etb18_weighted, na.rm = TRUE)
mean(etb18$zpalter, na.rm = TRUE)

library(gt)
vals1 <- 
  etb18 %>% select(starts_with("F411_01")) %>% 
    map_dfr(.,~attributes(.x)$labels,.id = "var") %>% 
    pivot_longer(-var) %>% 
    pivot_wider(names_from = value,values_from = name) 
    

etb18 %>% select(starts_with("F411_01")) %>% 
  map_dfr(.,~attributes(.x)$label) %>% 
      t(.) %>% data.frame() %>% 
      rownames_to_column(.,var = "var") %>% 
  left_join(vals1) %>% 
  gt() %>% 
  tab_options(  table.font.size = 12) %>% 
  tab_style(style = cell_text(font = "Roboto"),locations = cells_body(var))

## etb18_small <-
##   etb18 %>%
##   mutate(educ = case_when(S3 %in% 2:4 ~ 1,
##                           S3 %in% 5:6 ~ 2,
##                           S3 %in% 7:8 ~ 3),
##          F411_01 = ifelse(F411_01 > 4,NA,F411_01)) %>%
##   slice(1:300)

etb18$F518_SUF[etb18$F518_SUF>99990] <- NA

library(gt)
etb18 %>% select(matches("gkpol|Mig")) %>% 
  map_dfr(.,~attributes(.x)$labels,.id = "var") %>% 
  pivot_longer(-var) %>% 
  filter(!is.na(value)) %>% 
  gt() %>% 
  tab_options(  table.font.size = 12) %>% 
  tab_style(style = cell_text(font = "Roboto"),locations = cells_body(var))

data1 <- data.frame(z = seq(-4,4,.01)) # dataframe erstellen mit Zahlenfolge zwischen -4 & 4
data1$t.var <- dt(x=data1$z,df =  9999) 


ggplot(data = data1, aes(x=z, y =t.var)) + 
  theme_minimal(base_size = 15) +
  labs(y = "Häufigkeitsdichte", x = "t") +
  geom_ribbon(data=filter(data1,z <= - 1.960201), aes(ymin=0, ymax = t.var), fill = "#F5CC71") + # fläche links
  geom_segment(data = data.frame(z = - 1.960201,y1 = dt(x=- 1.960201,df =  1757)) , 
            aes(x=z,y=y1,xend = z, yend = 0), color = "#404040", size = .5, linetype = 2) + # grenze links
  geom_ribbon(data=filter(data1,z >=  1.960201), aes(ymin=0, ymax = t.var), fill = "#F5CC71") + # fläche rechts
  geom_segment(data = data.frame(z =  1.960201,y1 = dt(x= 1.960201,df =  1757)) , 
            aes(x=z,y=y1,xend = z, yend = 0), color = "#404040", size = .5, linetype = 2) + # grenze rechts
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
            aes(x=z,y=y1,xend = z, yend = 0), color = "grey50", size = .5, linetype = 3) + # mittellinie
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
            aes(x=z,y=-0.0125,xend = -1.85, yend = -0.0125), color = "grey25", size = .5,
            arrow = arrow(length = unit(0.5, "lines"), type = "closed")) + # pfeil nach links
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
            aes(x=z,y=-0.0125,xend = 1.85, yend = -0.0125), color = "grey25", size = .5,
            arrow = arrow(length = unit(0.5, "lines"), type = "closed")) + #pfeil nach rechts
  geom_label(data=data.frame(z = 0 , y1 = -0.0125, lab1 = "+/- 1.96", t.var = 0),aes(label = lab1), size = 3.5 ) +
  geom_line(color = "navy")  +   
  scale_x_continuous(breaks = seq(-3,3,1),minor_breaks = seq(-3,3,1))+
  theme(aspect.ratio = 1, panel.grid = element_line(size = rel(.25))) +
  geom_text(data=data.frame(z = c(-3,3), t.var = 0.015, label = rep(paste0(round(pt(q = -1.959964,df = 9999)*100,2),"%"),2) ), 
                            aes(x = z, y = t.var, label = label),
                            size = 3.25,vjust= 0, hjust = c(1,0))

qt(p=.025,df = 9999) # bei welchem t-Wert liegen 2.5% links davon?
qt(p=.975,df = 9999) # bei welchem t-Wert liegen 2.5% rechts davon?

qnorm(p = .025) # z-Wert aus Standard-NV
qt(p = .025, df = 3)
qt(p = .025, df = 30)
qt(p = .025, df = 300)
qt(p = .025, df = 3000)

qt(p = .05, df = 9999)
qt(p = .005, df = 9999)

ki90 <- ggplot(data = data1, aes(x=z, y =t.var)) + 
  theme_minimal() +
  labs(y = "Häufigkeitsdichte", x = "t",title ="90% Konfidenzintervall") +
  geom_ribbon(data=filter(data1,z <= qt(p = .05, df = 9999)), aes(ymin=0, ymax = t.var), fill = "#FCE8CE") + # fläche links
  geom_segment(data = data.frame(z = qt(p = .05, df = 9999),y1 = dt(x=- qt(p = .05, df = 9999),df =  1757)) , 
               aes(x=z,y=y1,xend = z, yend = 0), color = "#404040", size = .5, linetype = 2) + # grenze links
  geom_ribbon(data=filter(data1,z >=  qt(p = .95, df = 9999)), aes(ymin=0, ymax = t.var), fill = "#FCE8CE") + # fläche rechts
  geom_segment(data = data.frame(z =  qt(p = .95, df = 9999),y1 = dt(x= qt(p = .05, df = 9999),df =  1757)) , 
               aes(x=z,y=y1,xend = z, yend = 0), color = "#404040", size = .5, linetype = 2) + # grenze rechts
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
               aes(x=z,y=y1,xend = z, yend = 0), color = "grey50", size = .5, linetype = 3) + # mittellinie
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
               aes(x=z,y=-0.0125,xend = -1.64, yend = -0.0125), color = "grey25", size = .5,
               arrow = arrow(length = unit(0.5, "lines"), type = "closed")) + # pfeil nach links
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
               aes(x=z,y=-0.0125,xend = 1.64, yend = -0.0125), color = "grey25", size = .5,
               arrow = arrow(length = unit(0.5, "lines"), type = "closed")) + #pfeil nach rechts
  geom_label(data=data.frame(z = 0 , y1 = -0.0125, lab1 = paste0("+/- ",round(qt(p = .95, df = 9999),3)), t.var = 0),aes(label = lab1), size = 2.75 ) +
  geom_line(color = "#263056")  +   
  scale_x_continuous(breaks = seq(-3,3,1),minor_breaks = seq(-3,3,1))+
  theme(aspect.ratio = 1, panel.grid = element_line(size = rel(.25))) +
  geom_text(data=data.frame(z = c(-3,3), t.var = 0.015, label = rep(paste0(round(pt(q = -1.645,df = 9999)*100,3),"%"),2) ), 
            aes(x = z, y = t.var, label = label),
            size = 3.25,vjust= 0, hjust = c(1,0))+
  theme(plot.margin = margin(0, 0.1, 0, 0.1, "cm"))

ki99 <- ggplot(data = data1, aes(x=z, y =t.var)) + 
  theme_minimal() +
  labs(y = "Häufigkeitsdichte", x = "t",title ="99% Konfidenzintervall") +
  geom_ribbon(data=filter(data1,z <= qt(p = .005, df = 9999)), aes(ymin=0, ymax = t.var), fill = "#FFABC2") + # fläche links
  geom_segment(data = data.frame(z = qt(p = .005, df = 9999),y1 = dt(x=- qt(p = .005, df = 9999),df =  1757)) , 
               aes(x=z,y=y1,xend = z, yend = 0), color = "#404040", size = .5, linetype = 2) + # grenze links
  geom_ribbon(data=filter(data1,z >=  qt(p = .995, df = 9999)), aes(ymin=0, ymax = t.var), fill = "#FFABC2") + # fläche rechts
  geom_segment(data = data.frame(z =  qt(p = .995, df = 9999),y1 = dt(x= qt(p = .005, df = 9999),df =  1757)) , 
               aes(x=z,y=y1,xend = z, yend = 0), color = "#404040", size = .5, linetype = 2) + # grenze rechts
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
               aes(x=z,y=y1,xend = z, yend = 0), color = "grey50", size = .5, linetype = 3) + # mittellinie
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
               aes(x=z,y=-0.0125,xend = -2.55, yend = -0.0125), color = "grey25", size = .5,
               arrow = arrow(length = unit(0.5, "lines"), type = "closed")) + # pfeil nach links
  geom_segment(data = data.frame(z = 0,y1 = dt(x=0,df = 9999)) , 
               aes(x=z,y=-0.0125,xend = 2.55, yend = -0.0125), color = "grey25", size = .5,
               arrow = arrow(length = unit(0.5, "lines"), type = "closed")) + #pfeil nach rechts
  geom_label(data=data.frame(z = 0 , y1 = -0.0125, lab1 = paste0("+/- ",round(qt(p = .995, df = 9999),3)), t.var = 0),aes(label = lab1), size = 2.75 ) +
  geom_line(color = "#263056")  +   
  scale_x_continuous(breaks = seq(-3,3,1),minor_breaks = seq(-3,3,1))+
  theme(aspect.ratio = 1, panel.grid = element_line(size = rel(.25))) +
  geom_text(data=data.frame(z = c(-3,3), t.var = 0.015, label = rep(paste0(round(pt(q = -2.576321,df = 9999)*100,3),"%"),2) ), 
            aes(x = z, y = t.var, label = label),
            size = 3.25,vjust= 0, hjust = c(1,0)) +
  theme(plot.margin = margin(0, 0.1, 0, 0.1, "cm"))
library(patchwork)
ki90 + ki99

## install.packages("DescTools")
library(DescTools)
KendallTauA(etb18$educ,etb18$F600_12)

library(DescTools)
GoodmanKruskalGamma(etb18$educ,etb18$F600_12)

etb18 %>% 
  select(educ,F600_12) %>% 
  correlation(method = "kendall")
