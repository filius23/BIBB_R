# ------------------- #
# Installations & Funktionstest
# BIBB Kurs 2023
# ------------------- #

# Pakete --------------
# installiert die komplette Paketsammlung des tidyverse
paks <- c("tidyverse","labelled",'scico',"MetBrewer","effectsize","survey","marginaleffects","modelsummary","flextable","skimr","janitor","officer")
install.packages(paks)

library(tidyverse) 
library(labelled) 
library(effectsize) 
library(survey) 
library(marginaleffects)
library(modelsummary)
library(skimr) # Tabellen vorbereiten
library(janitor) # cleaning & kreuztabellen
library(flextable) # Formatierung der Tabelle für Word
library(officer) # office export

# Paketversionen ----
paks
pkgs <- sapply(c(paks,tidyverse::tidyverse_packages()),function(x) {
  # paste0(x, " Version: ", pacman::p_ver(x))
  numeric_version(packageVersion(paste0(x)))
}) 
pkgs <- lapply(pkgs,function(x) paste(x, collapse = "-"))

## log file zu Paketversionen ------

# # # # # # # # # # # # # # # # # # # # # #
log <- file("./log/01_install_test.txt")  # hier pfad anpassen
# # # # # # # # # # # # # # # # # # # # # #

sink(log)
t(data.frame(pkgs) )
sink()

# data.frame -----------------
studs <- c(19173,5333,15643)    # Studierendenzahlen unter "studs" ablegen 
profs       <- c(322,67,210)    # Prof-Zahlen unter "profs" ablegen

# data.frame()
dat1_orig <- data.frame(studs, profs)
dat1_orig

dat1 <- data.frame(studs = c(19173,5333,15643), 
                   profs = c(322,67,210),
                   gegr  = c(1971,1830,1973)) # ohne zwischen-Objekte
dat1    # zeigt den kompletten Datensatz an

# tidyverse tests -----------
dat1 %>% filter(.,studs < 10000) %>% select(.,gegr)
dat1 %>% filter(studs < 10000) %>% select(gegr)
dat1 %>% select(studs)
dat1 %>% arrange(studs)

# tidyr -------
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)


# labelled::labelled() ----
s3 <- labelled(
  c(1, 1, 2),
  c(Male = 1, Female = 2),
  label = "Assigned sex at birth"
)

s3

# effectsize ----
data("Music_preferences")
Xsq <- chisq.test(Music_preferences)
effectsize(Xsq)
effectsize(Xsq, type = "cohens_w")

# survey -----
data(api)
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
summary(svyglm(api00~ell+meals+mobility, design=dstrat))

# marginaleffects -----
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- slopes(mod)
head(mfx)
# Average Marginal Effect (AME)
avg_slopes(mod, by = TRUE)

# modelsummary --------
data(trees)
models <- list()
models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)

# simple table
modelsummary(models)

# flextable -----
ft <- flextable(head(mtcars))
ft

# janitor ----
x <- data.frame(caseID = 1, DOB = 2, Other = 3)
clean_names(x)
skim(iris, Species)


