
tnl <- 
    list.files(path = "../admin/2022/",pattern = "xlsx",full.names = T) %>% 
     map(~readxl::read_xlsx(.x,skip = 6) %>% select(matches("Mail")))


tnl[[2]]