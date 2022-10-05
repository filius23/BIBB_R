if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 
library(tidyverse)
library(LaCroixColoR)
library(patchwork)
library(ggrepel)
library(gt)
library(paletteer)
library(kableExtra)
library(extrafont)
library(marginaleffects)

windowsFonts(Nunito=windowsFont("Nunito Sans"))
mark_color <- "grey25"
color1x =  "#00519E" # uol farbe
colorhex <- "#FCFCFC" #"#FCF9F0FF"7
colorhex <- NA #"#FCF9F0FF"7

m_etb18 <- haven::read_dta("D:/Datenspeicher/BIBB_BAuA/BIBBBAuA_2018_suf1.0.dta",col_select = c("S1","F605","F518_SUF","m1202","zpalter")) 

theme_x <- 
  theme_minimal(base_family = "Nunito",base_size = 10) +
  theme(
    text = element_text(family = "Nunito"),
    plot.background = element_rect(fill = colorhex, linetype = 1, colour = NA),
    rect = element_rect(fill = colorhex, linetype = 1, colour = NA),
    axis.text =  element_text(color = mark_color,face = "plain", size = rel(1.05), angle = 0), 
    axis.title = element_text(color = mark_color,face = "plain", size = rel(1), angle = 0), 
    axis.title.y = element_text(color = mark_color,face = "plain", angle = 90,vjust = .5), 
    axis.ticks = element_blank(),
    axis.line = element_line(size = .1), 
    legend.text = element_text(family = "Nunito"),
    legend.title = element_text(family = "Nunito"),
    panel.grid = element_line(colour = "grey81", linetype = 1, size = .15), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    plot.subtitle = element_text(hjust=.5,family = "Nunito"),
    plot.caption = element_text(hjust=1, size = rel(1.2), color = mark_color),
    plot.margin = unit(c(1, 1, 1, 1), "lines"))

theme_set(theme_x)


m_etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                           col_select = c("S1","F605","F518_SUF","m1202","zpalter","Bula")) %>% 
  filter(F605 < 9, F518_SUF < 99998, zpalter < 100, S1==1) %>% 
  mutate(inc100 = F518_SUF/100,
         outside = 2-F605)

m_etb18 %>% count(outside,F605)
summary(m_etb18$inc100)

m2 <- glm(outside ~ inc100, family = "binomial", data = m_etb18)
summary(m2)

## install.packages("marginaleffects") # nur einmal nötig
## library(marginaleffects)

mfx <- marginaleffects(m2)
summary(mfx)

mx <- summary(mfx) %>% data.frame(.) %>% pull(estimate)

library(fixest)
m_etb18 <-
  m_etb18 %>%
  mutate(m1202_fct = factor(m1202,levels = c(-1,1:4),
                            labels = names(attributes(m1202)$labels) %>%
                              substr(.,1,5) %>% str_squish()),
         m1202_fct = fct_relevel(m1202_fct,"Ohne"))

feglm(outside ~ m1202_fct*zpalter |Bula, data = m_etb18, family = binomial)


etb_ue10 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                            col_select = c("F1605e","m1202")) %>% 
  filter(F1605e < 4, !is.na(F1605e), m1202 %in% 1:4) %>% 
  mutate(fam_beruf = 2-F1605e,
         m1202 = factor(m1202))

ggplot(m_etb18, aes(x = inc100, y = outside)) +
  geom_point(color = "#172869") +
  theme(aspect.ratio = .75)

m1 <- lm(outside ~ inc100, data = m_etb18)
summary(m1)

ggplot(m_etb18, aes(x = inc100, y = outside)) +
  geom_point(color = "#172869", size = .75) +
  geom_smooth(method = "lm", color = lacroix_palette("PeachPear",6)[2],se = F ) + 
  labs(y = "P(outside = 1)", x = "Einkommen (in 100 EUR)",
       title = "lineares Wahrscheinlichkeitsmodell")
  
## library(ggfortify)
## autoplot(m1,which = 1:2) # Homosk. & NV

library(ggfortify)
autoplot(m1,which = 1:2,nrow=1,ncol = 2) + theme_x + theme(aspect.ratio = .75)

df <- 
  data.frame(p1 = c(.5,1/3,1/4,1/5,2/3,3/4,1)) %>% 
  mutate(p = c("$$\\frac{1}{2}$$","$$\\frac{1}{3}$$","$$\\frac{1}{4}$$",
               "$$\\frac{1}{5}$$","$$\\frac{2}{3}$$","$$\\frac{3}{4}$$",
               "$$1$$" ),
         odds1 = c("$$1$$","$$0.5$$","$$0.33$$","$$0.25$$","$$2$$","$$3$$","$$\\frac{1}{0}$$"),
         odds2 = c("oder 1:1","oder 1:2","oder 1:3","oder 1:5","oder 2:1","oder 3:1","*sicher*"), 
         logodds = round(log(p1/(1-p1)),4))

gt(df[,2:5]) %>% 
  cols_width(p~px(120),
             odds1 ~ px(200),
             logodds ~ px(200),
             everything()  ~ px(100)) %>% 
  opt_align_table_header(align = c("right")) %>% 
  cols_align(align = "right",columns = everything()) %>% 
  cols_align(align = c("right"),columns = logodds) %>% 
    cols_label(
      p = md('$$P$$'), 
      odds1 = md('$$Odds = \\frac{P}{1-P}$$'),
      odds2 = '',
      logodds= md('$$Logits = log(Odds)$$')) %>% 
  fmt_markdown(columns = everything()) %>% 
  tab_options(heading.title.font.size = "small",
              table.border.top.style = "hidden",
              table.border.bottom.style = "hidden") %>% 
  tab_style(
    style = "vertical-align:top",
    locations = cells_title(groups = "title")) %>% 
  tab_options(data_row.padding = px(1))
  
  

summary(m2)$coefficients
-0.53856546   + -0.03868975 * 10
predict(m2, data.frame(inc100 = 10))

logits <- -0.9254629 
exp(logits) # Odds statt Logits
odds <- exp(logits) 
odds/(1+odds) # Wahrscheinlichkeit statt Odds 
exp(logits)/(1+exp(logits)) # beide Schritte auf einmal

predict(m2, data.frame(inc100 = 10), type="response")

predict(m2, data.frame(inc100=c("54.5"=54.5,"55.5"=55.5,"64.5"=64.5,"65.5"=65.5,
                                "74.5"=74.5,"75.5"=75.5)))

predict(m2, data.frame(inc100=c("54.5"=54.5,"55.5"=55.5,"64.5"=64.5,"65.5"=65.5,
                                "74.5"=74.5,"75.5"=75.5)), type = "response")

df3 <- tibble(x1 = m_etb18$inc100,
              y.logit = m2$fitted.values,
              me = margins::margins(m2)$dydx_inc, # steigung
              ) %>%
  mutate(y.p = exp(y.logit)/(1+exp(y.logit)),
         y = ifelse(y.logit>0,1,0),
         pred.p = predict(m2,data.frame(inc100 = x1), type = "response"), # vorhergesagte Wkt-Werte: y-Werte für Scatterplot
         x2 = dplyr::lead(x1),         # x- Werte um eine Zeile verschieben
         pred.p2 = dplyr::lead(pred.p), # y- Werte um eine Zeile verschieben
         dif_pred = pred.p2 -pred.p, # Differenz zwischen y- und y-Wert für x+1,
         pred.l = predict(m2,data.frame(inc100 = x1)),
         pred.l2 = dplyr::lead(pred.l), # logit pred. Werte um eine Zeile verschieben
         dif_pred.l = pred.l2 - pred.l, # Differenz
         me_min = pred.p - .5*me,
         me_max = pred.p + .5*me
  )
highlight2 <- c(55,75)
set.seed(2224)
df3_small <- df3 %>% slice_sample(n=75) 

la16_zoom <- 
      ggplot(df3_small %>% filter(between(x1,highlight2[1],highlight2[2])) %>% distinct(x1,.keep_all = T), 
             aes(x = x1 , y = pred.l)) + 
        geom_segment(aes(x=x1-.5,xend=x1+.5,y=pred.l-.5*m2$coefficients[2],yend=pred.l+.5*m2$coefficients[2]),
                     size = .5,
                     lineend = "square",
                     color = "orange") + #lacroix_palette("PeachPear", type = "discrete")[2]) +
        geom_segment(aes(x=x1-.5, xend=x1+.5, y=pred.l-.5*m2$coefficients[2],yend=pred.l-.5*m2$coefficients[2]),
                     size = .5,
                     color = lacroix_palette("PeachPear", type = "discrete")[1]) +
        geom_segment(aes(x=x1+.5, xend=x1+.5, y=pred.l-.5*m2$coefficients[2],yend=pred.l+.5*m2$coefficients[2]),
                     linejoin = "bevel",lineend = "butt",
                     arrow = arrow(length = unit(0.015, "npc")),
                     size = .5,
                     color = lacroix_palette("PeachPear", type = "discrete")[1]) +
        geom_point(color = lacroix_palette("PeachPear", type = "discrete")[6],size = 2) +
        geom_text_repel(aes(label = round(pred.l+.5*m2$coefficients[2] - (pred.l-.5*m2$coefficients[2]),5)),
                        nudge_x = 2,
                        direction = "x",
                        segment.colour = NA,
                        size = 2.75) +
        expand_limits(x = c(highlight2+2)) +
  labs(x = "inc100", title = "Steigung, ausgedrückt in Logits",y ="Logit(outside = 1) aus m2") +
  theme_x +
  theme(plot.title = element_text(hjust=.5,size = 15))

la13_inset <- 
    ggplot(df3 %>% distinct(x1,.keep_all = T), aes(x = x1 , y = pred.l)) + 
      geom_point(color = lacroix_palette("PeachPear", type = "discrete")[6],size = .35) +
      geom_rect(data = filter(df3, x1 %in% highlight2),
                aes(xmin= min(x1) - (max(x1)-min(x1))/10, 
                    xmax= max(x1) + (max(x1)-min(x1))/10, 
                    ymax= min(pred.l) - (max(pred.l)-min(pred.l))/10, 
                    ymin= max(pred.l) + (max(pred.l)-min(pred.l))/10),
                fill = NA,size = .15,color = lacroix_palette("PeachPear", type = "discrete")[4]) +
      labs(title ="", x = "", y = "") + 
      theme_x + 
      theme_minimal(base_family = "Nunito",base_size = 5) +
      theme(axis.title = element_blank(),plot.title = element_blank(),plot.subtitle = element_blank(),
            plot.background = element_rect(fill = colorhex, colour = lacroix_palette("PeachPear", type = "discrete")[5]))
            

la16_zoom + inset_element(la13_inset, left = 0.6, bottom = 0.6, right = 1, top = 1)

pa16_zoom <- 
ggplot(df3_small %>% filter(between(x1,highlight2[1],highlight2[2])) %>% distinct(x1,.keep_all = T), 
       aes(x = x1 , y = pred.p)) + 
  geom_segment(aes(x=x1-.5,xend=x1+.5,y=me_min,yend=me_max),color = paletteer_d("dutchmasters::milkmaid")[4], size = .5) +
  geom_segment(aes(x=x1+.5,xend=x1+.5,y=me_min,yend=me_max),color = paletteer_d("dutchmasters::milkmaid")[8], size = .5,
               linejoin = "bevel",lineend = "butt",
               arrow = arrow(length = unit(0.015, "npc"))) +
  geom_segment(aes(x=x1-.5,xend=x1+.5,y=me_min,yend=me_min), color = paletteer_d("dutchmasters::milkmaid")[8], size = .5) +
  geom_point(color = "#FF3200", size =2 ) + #paletteer_d("dutchmasters::milkmaid")[6],size = 2) +
  geom_text_repel(aes(label = me %>% round(.,5)),
                  nudge_x = 2,
                  direction = "x",
                  segment.colour = NA,
                  size = 2.75) +
  labs(x = "inc100", title = "Steigung, ausgedrückt in Wahrscheinlichkeiten",y ="P(outside = 1) aus m2")+
  theme_x + 
  theme(plot.title = element_text(hjust=.5,size = 15),
        plot.subtitle = element_text(hjust=.5)) +
  expand_limits(x = c(highlight2+2)) 

p.a16 <- ggplot(df3 %>% distinct(x1,.keep_all = T), aes(x = x1 , y = pred.p)) + 
            geom_point(color =  "#FF3200",size = .35) + #paletteer_d("dutchmasters::milkmaid")[6],size = .35) +
            geom_rect(data = filter(df3, x1 %in% highlight2),
                      aes(xmin= min(x1) - (max(x1)-min(x1))/10, 
                          xmax= max(x1) + (max(x1)-min(x1))/10, 
                          ymax= min(pred.p) - (max(pred.p)-min(pred.p))/10, 
                          ymin= max(pred.p) + (max(pred.p)-min(pred.p))/10),
                      fill = NA,size = .075,color = paletteer_d("dutchmasters::milkmaid")[10]) +
            labs(title ="", x = "", y = "") + 
            theme_x + 
            theme_minimal(base_family = "Nunito",base_size = 5) +
            theme(axis.title = element_blank(),plot.title = element_blank(),plot.subtitle = element_blank(),
                  plot.background = element_rect(fill = colorhex, colour = paletteer_d("dutchmasters::milkmaid")[13]))

pa16_zoom + inset_element(p.a16, left = 0.6, bottom = 0.6, right = 1, top = 1)
