library(readxl)
library(dplyr)
library(magrittr)

db <- read_excel("../Data/BD_ECV_2020.xlsx")

db$id_hogar %>% unique() %>% length() %>%  print()
db %>% filter(., Orden == 1) %>% count() %>% print() 
db %>% filter(., hRel == "Jefe del Hogar") %>% count() %>% print()

db %<>% filter(Orden == 1)
db %<>%  select(P_6, P_10, P_12, P_45, P_73,P_146, P_226, P_235, P_243, P_291, P_295)

write.csv(db, "Data/db.csv")
