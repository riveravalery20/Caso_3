library(readr)
Educación <- read_csv("C:/Users/etmel.DESKTOP-N79IL9B/Downloads/Educación.csv")
View(Educación)

Educación=data.frame(Educación)

#limpieza de NA
library(tidyverse)

Educación=Educación %>%select(-c(1,3,7,11,12,13,14,17,22,23,24,25))
Educación= Educación[-(218:222), ]
Educación <- na.omit(Educación)

#Renombre de variables
colnames(Educación) <- c("País", "Adolescentes_fuera_del_colegio", 
                         "Niños_fuera_del_colegio","Años_obligatorios_escolares",
                         "Nivel_educativo_al_menos_secundaria_completa",
                         "Nivel_educativo_al_menos_primaria_completa",
                         "Nivel_educativo_posgrado", "Gasto_público_en_educación",
                         "Docentes_capacitados_en_educación_primaria","Contribución_social",
                         "Gasto_en_educación_secundaria","Efectividad_del_gobierno",
                         "Estimación_estabilidad_politica_ausencia_terrorismo","Ingresos_fiscales")
