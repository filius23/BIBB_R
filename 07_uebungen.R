
# Übung 1 ---------
# Unterschied in der Arbeitszeit (`az`) zwischen Männern und Frauen besteht (`S1`)
t.test(etb18$az~etb18$S1)


# Berechnen Sie das Cohen's d für diesen Zusammenhang.
library(effectsize)
cohens_d(etb18$az~etb18$S1)

# Übung 2 ---------

## Korrelation az & F518_SUF ----
etb18$F518_SUF[etb18$F518_SUF>99990] <- NA

cor.test(etb18$F518_SUF,etb18$az,method = "pearson", use = "pairwise.complete.obs")

## educ & F411_01 -----
etb18_small <- 
  etb18 %>% 
  mutate(educ = case_when(S3 %in% 2:4 ~ 1,
                          S3 %in% 5:6 ~ 2,
                          S3 %in% 7:8 ~ 3),
         F411_01 = ifelse(F411_01 > 4,NA,F411_01)) %>% 
  slice(1:300)

## Kreuztabelle
xtabs(~educ+F411_01,etb18_small)
tab2 <- xtabs(~educ+F411_01,etb18_small)
chisq.test(tab2)

cor.test(etb18_small$educ,etb18_small$F411_01,method = "spearman", use = "pairwise.complete.obs")
cor.test(etb18_small$educ,etb18_small$F411_01,method = "kendall", use = "pairwise.complete.obs")

effectsize::cramers_v(etb18_small$educ,etb18_small$F411_01)


# Übung 3 ----------

library(survey)
etb18$F518_SUF[etb18$F518_SUF>99990] <- NA

etb18_weighted <- svydesign(id      = ~intnr,
                            weights = ~gew2018,
                            data    = etb18)
svytable(~Mig+gkpol,etb18_weighted) # syntax wie xtabs()
xtabs(~Mig+gkpol,etb18)

svymean(~F518_SUF, etb18_weighted, na.rm = TRUE)
mean(etb18$F518_SUF, na.rm = TRUE)