if(Sys.getenv("USERNAME") == "Filser" ) .libPaths("D:/R-library4")
library(tidyverse)
knitr::opts_chunk$set(collapse = F)


etb <- haven::read_dta("D:/Datenspeicher/BIBB_BAuA/BIBBBAuA_2018_suf1.0.dta")

etb %>% count(S3)
xtabs(~S3,etb)


file.copy(from = "D:/Datenspeicher/BIBB_BAuA/BIBBBAuA_2018_suf1.0.sav",
          to =   "D:/oCloud/Home-Cloud/Lehre/BIBB/BIBB_R/data/BIBBBAuA_2018_suf1.0.sav")



table(etb$m1202)

etb_small <- etb %>% select(intnr,az,S1,S3,S2_j,zpalter,Stib,Bula,m1202,matches("F209")) %>% as.data.frame()
head(etb_small)

write_delim(x = etb_small,"./data/BIBBBAuA_2018_small.csv",delim = ";")

