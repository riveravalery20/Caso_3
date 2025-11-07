library(readr)
Empleos_ANS_2_ <- read_csv("C:/Users/etmel.DESKTOP-N79IL9B/Downloads/Empleos. ANS (2).csv")
View(Empleos_ANS_2_)

Empleos=data.frame(Empleos_ANS_2_)

#Limpieza de datos.
library(tidyverse)

Empleos = Empleos_ANS_2_ %>% select(-c(1,3,8,9,10,21,22,24,25,26))
Empleos=Empleos[-(218:247), ]

Empleos=na.omit(Empleos)

#Renombre de columnas
colnames(Empleos)=c("País","Empleo_total","Fuerza_laboral_total","Desempleo_total(%)",
                    "Gasto_en_educación","Nuevas_empresas_registradas","Población_total",
                    "Ingresos_fiscales","Crecimiento_del_PIB","PIB_per_capita","Valor_añadido_Industrias",
                    "Valor_añadido_manufactura","Volumen_de_exportación","Salidas_netas_de_inversión_extranjera",
                    "Entradas_netas_de_inversión_extranjera","Crecimiento_poblacional")
# Preparación de la base
library(tidyverse)
library(FactoClass)
library(factoextra)

# Base limpia
Empleos_df <- Empleos %>% select(-País)
rownames(Empleos_df) <- Empleos$País

# Aseguramos que todo sea numérico, sin NA y estandarizado
Empleos_df <- Empleos_df %>% mutate(across(everything(), as.numeric))
Empleos_df <- na.omit(Empleos_df)
Empleos_df <- scale(Empleos_df)

#  ACP sin clasificación
res_acp <- dudi.pca(Empleos_df, scannf = FALSE, nf = 5)

# Extraemos coordenadas de los individuos (dimensiones)
coord_ind <- res_acp$li  # Coordenadas sobre componentes principales


set.seed(123)
modelo_kmeans <- kmeans(coord_ind, centers = 4)
Grupo <- modelo_kmeans$cluster

# Unimos a la base original
Empleos_clasificados <- data.frame(Grupo = Grupo, Empleos)

# Visualización de individuos en componentes 1 y 2
s.class(coord_ind, fac = Grupo,
        sub = "Clustering sobre dimensiones ACP",
        possub = "bottomright",
        xax = 1, yax = 2,
        col = c("#EE220D", "#00AFBB", "#E7B800", "#FC4E07"))

# Visualización en componentes 1 y 3
s.class(coord_ind, fac = Grupo,
        sub = "Clustering sobre dimensiones ACP (1 vs 3)",
        possub = "bottomright",
        xax = 1, yax = 3,
        col = c("#EE220D", "#00AFBB", "#E7B800", "#FC4E07"))

# Caracterización de grupos por medias
Empleos_clasificados %>%
  group_by(Grupo) %>%
  summarise(across(where(is.numeric), mean, .names = "media_{.col}"))