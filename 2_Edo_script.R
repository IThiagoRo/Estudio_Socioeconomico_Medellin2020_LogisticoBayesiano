library(tidyverse)
library(magrittr)
library(readxl)


db <- read_excel("../Data/BD_ECV_2020.xlsx")


db$P_6 %<>% factor()
db$P_10 %<>% factor() 
db$P_45 %<>% factor()
db$P_73 %<>% factor()
db$P_146 %<>% factor() 
db$P_226 %<>% factor()
db$P_291 %<>% factor()


db_factor <- db %>% select(P_6, P_10, P_45, P_73, P_146, P_226, P_291)
db_num <- db %>% select(P_235, P_243)


# Grafico de correlacion 
resum <- db_num %>% filter(!P_235 %in% c(-88, -98, -99)) %>% filter(!P_243 %in% c(-88, -98, -99)) %>% filter(P_235 <= 1500000 & P_243 <= 1500000) 

resum <- resum %>% rename(Alimentos = P_235,Gastos = P_243)
cor_db_num <- cor(resum)


library(corrplot)
corrplot(cor_db_num, method="number", type="upper")


# Grafico Estrato###
library(scales)
resum <- db %>% select(., P_10, P_291) %>% group_by(P_10, P_291) %>% count()
resum <- resum %>% rename(Estrato = P_10, Riesgo = P_291)

resum$Riesgo <- factor(resum$Riesgo, levels = c("1", "0"))
resum$porcentaje <- resum$n / sum(resum$n) * 100


ggplot(resum, aes(x = as.factor(Estrato), y = porcentaje, fill=as.factor(Riesgo))) +
  geom_bar(stat = "identity", position = "dodge")+
  ylab("Porcentaje de hogares")+
  xlab("Estrato")+
  geom_text(aes(label=scales::percent(n/sum(n))),position=position_dodge(0.90), vjust=-0.3, size=3)+
  scale_fill_brewer(name = "Riesgo", 
                       labels=c("Sin alimentos", "Con alimentos"))+
  theme_bw()

# Grafico numero de personas en el hogar
resum <- db %>% select(., P_12, P_291) %>% group_by(P_12, P_291) %>% count()
resum <- resum %>% rename(Personas = P_12, Riesgo = P_291)

resum$Riesgo <- factor(resum$Riesgo, levels = c("1", "0"))
resum$porcentaje <- resum$n / sum(resum$n) * 100


ggplot(resum, aes(x = as.factor(Personas), y = porcentaje, fill=as.factor(Riesgo))) +
  geom_bar(stat = "identity", position = "dodge")+
  ylab("Porcentaje de hogares")+
  xlab("Numero de personas en el hogar")+
  geom_text(aes(label=scales::percent(round(n/sum(n), 3))),position=position_dodge(0.90), vjust=-0.3, size=3)+
  scale_fill_brewer(name = "Riesgo", 
                    labels=c("Sin alimentos", "Con alimentos"))+
  theme_bw()


# Grafico tipo de vivienda
resum <- db %>% select(., P_146, P_291) %>% filter(., P_146 != -99) %>% group_by(P_146, P_291) %>% count()
resum <- resum %>% rename(Vivienda = P_146, Riesgo = P_291)

resum$Vivienda <- factor(resum$Vivienda)

resum$Vivienda <- factor(resum$Vivienda, labels = c("Rancho", "Cuarto(s)", "Inquilinato", "Apartamento", "Casa"))
resum$Riesgo <- factor(resum$Riesgo, levels = c("1", "0"))
resum$porcentaje <- resum$n / sum(resum$n) * 100


ggplot(resum, aes(x = as.factor(Vivienda), y = porcentaje, fill=as.factor(Riesgo))) +
  geom_bar(stat = "identity", position = "dodge")+
  ylab("Porcentaje de hogares")+
  xlab("Tipo de vivienda")+
  geom_text(aes(label=scales::percent(round(n/sum(n), 3))),position=position_dodge(0.90), vjust=-0.3, size=3)+
  scale_fill_brewer(name = "Riesgo", 
                    labels=c("Sin alimentos", "Con alimentos"))+
  theme_bw()

