if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4")
if(Sys.getenv("USERNAME") == "filse" ) path <- "D:/oCloud/RFS/"
library(haven)
library(tidyverse)
library(patchwork)
knitr::opts_chunk$set(message = F,warning = F,highlight = "#<<",cache = T,
                      out.height= "65%", out.width = "65%", fig.align="center")
# https://wtoivo.github.io/SGSSS-data-viz-workshop/bar-plots.html
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")
tab_df <- xtabs(~S1+m1202, data = etb18) %>% data.frame()

# etb18$zpalter[etb18$zpalter>100] <- NA
# etb18$m1202[etb18$m1202<0] <- NA

## library(haven) # für datenimport
## library(tidyverse)
## # library(ggplot2) # nicht nötig wenn tidyverse geladen

etb18 %>%
  slice(1:100) %>% 
  mutate(S1_fct = factor(S1, levels = 1:2, labels = c("Männer","Frauen")),
         zpalter = ifelse(zpalter > 100,NA,zpalter)) %>%
ggplot(aes(x = zpalter, y = az)) +
  geom_point(aes(color = S1_fct)) +
  # facet_grid(~S1_fct) +
  theme_minimal() +
  labs(color = "Geschlecht", y = "Arbeitszeit/Woche",
       x = "Alter") +
  scale_color_manual(values = c("lightskyblue4","navy"))

## etb18 <- read_dta("./data/BIBBBAuA_2018_suf1.0.dta")
etb18 %>% select(az,S1,zpalter) %>% head()

etb18$zpalter[etb18$zpalter>100] <- NA # missing in zpalter mit NA überschreiben
etb18_small <- etb18 %>% slice(1:100)

## ggplot(data = datensatz, aes(x = var1, y = var2, color = var3)) +
##   geom_point() +
##   labs(title= "Titel", subtitle = "Untertitel") +
##   theme_minimal()

ggplot(data = etb18_small)

ggplot(data = etb18_small, aes(x = zpalter, y = az))

ggplot(data = etb18_small, aes(x = zpalter, y = az)) + geom_point()

ggplot(data = etb18_small, aes(x = zpalter, y = az)) + geom_point(color = "orange")

ggplot(data = etb18_small, aes(x = zpalter, y = az, color = S1 )) + 
  geom_point()
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = as.numeric(S1))) + 
  geom_point()
ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point()

ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"))

## ggplot(data = etb18_small, aes(x = zpalter, y = az)) +
##   geom_point(aes(color = factor(S1))) +
##   scale_color_manual(values = c("lightskyblue4","navy"))

ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") )

ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(color = "Geschlecht", y = "Arbeitszeit/Woche",
       x = "Alter",
       title = "Arbeitszeit und Alter",
       subtitle = "Nach Geschlecht",
       caption = "Quelle: ETB 2018"
       ) 

p1 <- ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point(size = 2) 

p1 + scale_color_manual(values = c("lightskyblue3","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) 

p1 + scale_color_manual(values = c("coral","orange"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) 

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

## plot_objekt1 <- ggplot(data = etb18_small, aes(x = zpalter, y = az,
##                                color = factor(S1),
##                                shape = factor(m1202))) +
##   geom_point(size = 2) +
##   scale_color_manual(values = c("lightskyblue3","navy"),
##                     breaks = c(1,2), labels = c("Männer", "Frauen") ) +
##   scale_shape_manual(values = c(15:18),breaks = c(1:4),
##                      labels = c("ohne Aus", "duale Ausb.","Aufstiegsfortb.","FH/Uni")) +
##   labs(color = "Geschlecht",
##        shape = "Ausbildung",
##        fill = "Geschlecht",
##        y = "Arbeitszeit/Woche",
##        x = "Alter",
##        title = "Arbeitszeit und Alter",
##        subtitle = "Nach Geschlecht",
##        caption = "Quelle: ETB 2018"
##        )
## ggsave(plot = plot_objekt1,filename = "./grafik/plot1.png",
##        dpi = 800, # auflösung
##        # width = 9, height = 7, # falls angepasst werden soll
##        )

## install.packages("ggeasy")

library(ggeasy)
etb18_small$S1 <- factor(etb18_small$S1)
ggplot(data = etb18_small, aes(x = zpalter, 
                               y = az)) + 
  geom_point(size = 2) + 
  easy_labs()

ak <- readr::read_delim(paste0(path,"allbus_kumuliert.csv"), delim = ";", col_types = cols(.default = col_double()),
                        col_select = c("hs16"))

bp_ann_df <- 
  filter(ak,hs16>0) %>% 
  mutate_at(vars(hs16),~ifelse(.<0,NA,.)) %>% 
  summarise(q25 = quantile(hs16,probs = .25),
            q50 = quantile(hs16,probs = .5),
            q75 = quantile(hs16,probs = .75),
            whis1 = q25 - 1.5*(q75-q25) + .5 ,
            whis2 = q75 + 1.5*(q75-q25) - .5) %>% 
  mutate_all(~if_else(. < 0,0,.)) %>% 
  pivot_longer(cols=everything(), values_to = "hs16") %>% 
  mutate(xend1 = ifelse(grepl("whis",name),.015,.4),
         name = case_when(name == "q25" ~ "1. Quartil (25% Grenze)",
                          name == "q50" ~ "Median (50% Grenze)",
                          name == "q75" ~ "3. Quartil (75% Grenze)",
                          name == "whis1" ~ "unterer Whisker: q1 - 1.5*IQR",
                          name == "whis2" ~ "oberer Whisker: q3 - 1.5*IQR"),
         x = 1)

bp_ann_ausr <- filter(ak,!between(hs16,146,197),hs16>0) %>% select(hs16) %>% mutate(name="Ausreißer",x=1) %>% 
  distinct() %>% group_by(aus=hs16>146) %>% 
  mutate(hs16m=mean(hs16) %>% if_else(.<144,140,.))

  
ggplot(filter(ak,hs16>0), aes(x = 0, y = hs16)) + 
  geom_boxplot() + 
  geom_label(data = bp_ann_df, aes(x = x-.45, y = hs16, label = name), hjust = 0, label.size = .0,fontface="italic", size = 5.25) +
  geom_label(data = bp_ann_ausr, aes(x = x-.45, y = hs16m, label = name), hjust = 0, label.size = .0,fontface="italic", size = 5.25) +
  geom_segment(data = bp_ann_df, aes(x = x-.451, xend = xend1, yend = hs16, y = hs16), color = "#172869",
               lineend = 'butt', linejoin ='bevel',arrow = arrow(length = unit(.025,"npc"), type = "closed")) +
  geom_segment(data = bp_ann_ausr, aes(x = x-.451, xend = .015, yend = hs16, y = hs16m), color = "#172869",
               lineend = 'butt', linejoin ='bevel',arrow = arrow(length = unit(.01,"npc"), type = "closed")) +
  labs(color = "", # legenden-label auf leer
       y = "", # y-Achse labeln
       x="")+ # x-Achse labeln
  theme_void() + 
  theme(axis.text.x = element_blank()) +
  expand_limits(x = c(0,1.2))


ggplot(data = etb18_small, aes(y = az)) + geom_boxplot()

ggplot(data = etb18_small, aes(y = az, x = factor(S1))) + geom_boxplot()

ggplot(data = etb18_small, aes(x = az)) + 
  geom_histogram()  

ggplot(data = etb18_small, aes(x = az)) + 
  geom_histogram(fill = "sienna1")  

ggplot(data = etb18_small, aes(x = az, fill = factor(S1))) + 
  geom_histogram() 

ggplot(data = etb18_small, aes(x = az, fill = factor(S1))) + 
  geom_histogram(position = position_dodge()) 

ggplot(data = etb18_small, aes(x = az, fill = factor(S1))) + 
  geom_histogram(position = position_dodge()) +
  scale_fill_manual(values = c("sienna1","dodgerblue4"),
                    breaks = 1:2, labels = c("Männer","Frauen")) +
  labs(fill = "Geschlecht")

ggplot(data = etb18, aes(x = az)) + 
  geom_histogram(aes(fill = factor(S1)), color = "grey50",position = position_dodge()) + 
  scale_fill_viridis_d(option = "E",labels = c("Männer","Frauen"))

ggplot(data = etb18, aes(x = az,fill = factor(S1))) + 
  geom_density(alpha = .5) + 
  scale_fill_manual(values = c("sienna1","dodgerblue4"),
                    breaks = 1:2, labels = c("Männer","Frauen")) +
  labs(fill = "Geschlecht") 

etb18$m1202[etb18$m1202<0] <- NA # missings ausschließen
etb18 %>% 
  count(S1,m1202) %>% 
  filter(!is.na(m1202))

etb18 %>% 
  count(S1,m1202) %>% 
  filter(!is.na(m1202)) %>% 
  ggplot(data = ., aes(x = m1202, y = n, fill = factor(S1))) +
  geom_col(position=position_dodge()) 

etb18 %>% 
  filter(!is.na(m1202)) %>% 
  count(S1,m1202) %>% 
  group_by(S1) %>% 
  mutate(pct_gender = prop.table(n)) %>% 
  ggplot(data = ., aes(x = m1202, y = pct_gender, fill = factor(S1))) +
  geom_col(position=position_dodge()) 


etb18 %>% 
  filter(!is.na(m1202)) %>% 
  count(S1,m1202) %>% 
  group_by(S1) %>% 
  mutate(pct_gender = prop.table(n)) %>% 
  ggplot(data = ., aes(x = m1202, y = pct_gender, fill = factor(S1))) +
  geom_col(position=position_dodge())  +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) 

ggplot(data = etb18 , 
       aes(x = m1202, fill = factor(S1) )) + 
  geom_bar(  position=position_dodge()) + 
  labs(title = "Absolute Häufigkeit")


ggplot(data = etb18 , 
       aes(x = m1202, fill = factor(S1),
           y = ..count../sum(..count..)
           )) + 
  geom_bar(  position=position_dodge()) + 
  labs(title = "Relative Häufigkeit") 

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
  scale_x_continuous(breaks = 1:4 , labels = c("ohne Ausb.", "duale Ausb.","Aufstiegsfortb.","FH/Uni")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Ausbildungsabschlüsse nach Geschlecht",
       subtitle = "Relative Häufigkeiten",
       caption = "Quelle: ETB 2018",
       x = "Ausbildung",
       y = "Relative Häufigkeit",
       fill = "Geschlecht" ) 


## etb %>% filter(S2_j < 9999,h1216d>0) %>% slice(1:150) %>%
##   ggplot(aes(x=S2_j,y=h1216d)) + geom_point()
## etb %>% filter(S2_j < 9999,h1216d>0) %>% slice(1:150) %>%
##   ggplot(aes(x=S2_j,y=h1216d,color = factor(S1))) + geom_point()

etb18 %>% count(F100_wib2,gkpol) %>% filter(F100_wib2>=0) %>% 
  ggplot(aes(x = gkpol, y = n, fill = factor(F100_wib2))) +
  geom_col(position=position_dodge()) +
  scale_x_continuous(breaks = 1:7,
                   labels = c("<2k", "2k bis <5k", "5k bis <20k", "20k bis <50k", 
                              "50k bis <100k", "100k bis <500k", "500k und mehr"))

etb18 %>% count(F100_wib2,gkpol) %>% pull(gkpol) %>% as_factor() %>% levels() %>% 
  str_remove_all(.," Einwohner") %>% 
  str_replace_all(.,pattern = "\\.000","k") %>% 
  str_replace(.,pattern = "unter ","<") %>% 
  dput()
## scale_x_continuous(breaks = 1:7,
##                    labels = c("<2k", "2k bis <5k", "5k bis <20k", "20k bis <50k",
##                               "50k bis <100k", "100k bis <500k", "500k und mehr"))
## 

eg <- tribble(
  ~x, ~y, ~size, ~x1,
  "A", 1, 5, 1,
  "B", 1, 10, 2,
  "C", 1, 15, 3
)

eg_theme <- 
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        # aspect.ratio = .5,
        plot.title = element_text(size = rel(1.5),hjust = 0.5))

aes_clr <- 
  ggplot(eg, aes(x = x, y = y, color = x)) +
    geom_point(size = 5) +
    guides(color = FALSE) +
    labs(title = "color (discrete)") +
    eg_theme   

aes_clrc <- 
  ggplot(eg, aes(x = x1, y = y, color = x1)) +
    geom_point(size = 5) +
    guides(color = FALSE) +
    coord_cartesian(xlim = c(0.5, 3.5)) +
    labs(title=  "color (continuous)") +
    eg_theme

aes_size <- 
  ggplot(eg, aes(x = x, y = y, size = x)) +
    geom_point() +
    scale_size_discrete(range = c(1.5, 10)) +
    guides(size = FALSE) +
    labs(title = "size") +
    eg_theme 
  
aes_fill <-   
  ggplot(eg, aes(x = x, y = y, fill = x)) +
    geom_point(size = 5, pch = 21, stroke = 1.5) +
    guides(fill = FALSE) +
    eg_theme+ 
  labs(title = "fill")

aes_shape <- 
  ggplot(eg, aes(x = x, y = y, shape = x)) +
    geom_point(size = 5) +
    guides(shape = FALSE) +
    eg_theme + 
    labs(title = "shape")
# Alpha

aes_alpha <- 
  ggplot(eg, aes(x = x, y = y, alpha = x)) +
    geom_point(size = 5) +
    guides(alpha = FALSE) +
    eg_theme +
  labs(title="alpha")


aes_clr + aes_size + aes_shape
aes_clrc + aes_fill + aes_alpha

## ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) +
##   geom_point(size = 2) +
##   theme_minimal()
## 
## ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) +
##   geom_point(size = 2) +
##   theme_dark()

p1 <- ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point(size = 2) 

p1 + theme_gray() + labs(title="theme_grey() - Standard") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
p1 + theme_bw() + labs(title="theme_bw()") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
p1 + theme_minimal() + labs(title="theme_minimal()") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))
p1 + theme_dark() + labs(title="theme_dark()") + theme(plot.title = element_text(size = rel(2.5),hjust = 0.5))

p1 <- ggplot(data = etb18_small, aes(x = zpalter, y = az, color = factor(S1))) + 
  geom_point(size = 3) 

p1 +  scale_color_manual(values = c("dodgerblue4","sienna1"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") )

p1 +  scale_color_manual(values = c("#005b96","#6497b1"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") )

p1 +
  scale_color_brewer(palette = "RdYlBu",
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) 

p1 +
  scale_color_viridis_d(option="magma",
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) 
p1 +
  scale_color_viridis_d(option="magma",begin = .65,end = .85,
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) 

knitr::include_graphics("./pic/104_viridis-scales.png")

## install.packages('scico')
## install.packages("MetBrewer")

scico::scico_palette_show()

knitr::include_graphics("./pic/104_metbrewer.png")

library(scico)
p1 +
  scale_color_scico_d(palette = "oslo",begin = .5,end = .8,
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) 


library(MetBrewer)
p1 +
  scale_color_met_d(name = "Kandinsky",
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) 

shp_df <- data.frame(shp = factor(1:25), x = rep(1:5,each=5), y = rep(1:5,5))
ggplot(shp_df,aes(x,y)) +
  geom_point(shape=shp_df$shp, size = 7, fill = "dodgerblue") +
  geom_text(aes(label=shp,x = x-.2), size = 6) +
  theme_void(base_size=15)+
  scale_y_reverse() +
  theme(plot.margin = unit(c(2,2,2,2),"lines"))

lt_df <- data.frame(x = 0, y = seq(0,.75,.125),
                    lty = 0:6,
                     lt = c("0 'blank'"   ,"1 'solid'"   ,"2 'dashed'"  ,"3 'dotted'"  ,"4 'dotdash'" ,"5 'longdash'",  "6 'twodash'" )  )
ggplot(lt_df, aes(x,y,linetype = factor(lty))) + 
  geom_segment(aes(xend = 1,yend = y), size = 1) +
  geom_text(aes(x=-.35,label = lt),hjust= 0, size = 6) +
  theme_void(base_size=12)+
  guides(linetype = F) +
  scale_y_reverse()


knitr::include_graphics("./pic/104_decision.png")
