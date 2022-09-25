
library(modelsummary)
R.version

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)

models <- list(
  "OLS 1"     = lm(Donations ~ Literacy + Clergy, data = dat),
  "Poisson 1" = glm(Donations ~ Literacy + Commerce, family = poisson, data = dat),
  "OLS 2"     = lm(Crime_pers ~ Literacy + Clergy, data = dat),
  "Poisson 2" = glm(Crime_pers ~ Literacy + Commerce, family = poisson, data = dat),
  "OLS 3"     = lm(Crime_prop ~ Literacy + Clergy, data = dat)
)

modelsummary(models, vcov = ~Region)
