
tnl <- 
    list.files(path = "../admin/2022/",pattern = "xlsx",full.names = T) %>% 
     map(~readxl::read_xlsx(.x,skip = 6) %>% select(matches("Mail")) %>% 
           rename_with(~tolower(.x)) %>% 
           rename_with(~str_extract(.,"adresse")))


bind_rows(tnl) %>% na.omit() %>% distinct() %>% xlsx::write.xlsx(.,file = "../admin/2022/mailadressen.xlsx")

# Überschrift 1 ----
## Abschnit 1.1 ----
## Abschnit 1.2 ----

# Überschrift 2 ----
