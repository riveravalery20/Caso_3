library(readr)
Medio_ambiente <- read_csv("C:/Users/etmel.DESKTOP-N79IL9B/Downloads/Medio ambiente.csv")
View(Medio_ambiente)

Medio_ambiente=data.frame(Medio_ambiente)

library(readr)
Variables_añadidas_medio_ambiente <- read_csv("C:/Users/etmel.DESKTOP-N79IL9B/Downloads/Variables añadidas, medio ambiente.csv")
View(Variables_añadidas_medio_ambiente)

Variables=data.frame(Variables_añadidas_medio_ambiente)

#Limpieza de datos
library(tidyverse)
library(janitor)
Medio_ambiente=Medio_ambiente %>% clean_names()
Variables=Variables %>% clean_names()

print("Columnas de base1:")
print(names(Medio_ambiente))
print("Columnas de base2:")
print(names(Variables))

Medio_ambiente_final <- full_join(Medio_ambiente, Variables, by = "country_name")

Medio_ambiente_final <- Medio_ambiente_final[-(218:312), ]
Medio_ambiente_final=Medio_ambiente_final %>%select(-c(1,5,7,6,8,18))
Medio_ambiente_final <- na.omit(Medio_ambiente_final)

#Renombrar datos
colnames(Medio_ambiente_final)=c("País","Acceso_combustibles_renovables_para_cocinar",
                                 "Emisión_de_gases_de_efecto_invernadero","Nivel_de_estrés_hidrico",
                                 "combustibles_renovables","Extracciones_anuales_de_agua_dulce",
                                 "Uso_de_energias_alternativas_y_nucleares","Tierra_cultivable",
                                 "Recursos_internos_renovables_de_agua_dulce","Densidad_poblacional",
                                 "Areas_protegidas","Productividad_del_agua","Tierra_agrícola",
                                 "Producción_acuícola","Importaciones_netas_de_energía","Consumo_de_fertilizantes")
