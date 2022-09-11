if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4")

log <- file("./log/01_install_test.txt")
sink(log)
# sink(log, type="message")
Sys.time()

# test skript für paket installation und 


pakete1 <- c("haven","tidyverse","labelled","easystats","modelsummary","ggeffects","flextable","pacman")

start <- Sys.time()

inst_funct <- function(px){
  print(paste0(Sys.time(), ": " ,px, " wird installiert"))
  install.packages(px)
}

lapply(pakete1, inst_funct)
invisible(sapply(pakete1,library, character.only = TRUE))
sapply(pakete1,function(x) {
  paste0(x, " Version: ", pacman::p_ver(x))
  })

print("Tests")
mtcars %>% summarise(across(everything(),~mean(.x)))


mtcars %>%
  rowwise() %>%
  mutate(
    sum = sum(c_across(1:5)),
    sd = sd(c_across(1:5))
  )

models <-list(
   mod1='mpg~hp', 
   mod2='mpg~hp+qsec', 
   mod3='mpg~hp+qsec+am'
   )


print("Modeltest")
models %>% map_dfr(.,~broom::tidy(lm(formula=.x,data=mtcars)),.id = "mod") 
print("")
print("")
print("")
print("")
paste("Start: ", start, "Ende: ", Sys.time())
sink()