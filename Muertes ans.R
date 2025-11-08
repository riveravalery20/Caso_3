# Cargar librerías necesarias
library(readr)
library(tidyverse)
library(FactoMineR)
library(factoextra)

# Leer la base

Muertes_ans <- read_csv("C:/Users/etmel.DESKTOP-N79IL9B/Downloads/Muertes ans.csv")
View(Muertes_ans)
Muertes=data.frame(Muertes_ans)

# Limpieza y selección de variables

Muertes=Muertes[-(218:245), ]

# Renombrar columnas
colnames(Muertes) <- c("País", "Expectativa_de_vida_mujer", "Expectativa_de-vida_hombres", "Accidentes_de_transito",
                       "Enfermedades_cardiacas_cancer_diabetes_mujeres", "Enfermedades_cardiacas_cancer_diabetes_hombres", 
                       "Polución_del_aire_mujeres", "Polución_del_aire_hombres", "Envenenamiento_accidental_mujeres", 
                       "Envenenamiento_accidental_hombres", "Tasa_de_mortalidad_mujeres_adultas","Tasa_de_mortalidad_hombres_adultos"
                       "Tasa_de_mortalidad_infantil_mujeres", "Tasa_de_mortalidad_infantil_varones", "Salidas_netas_de_inversión_extranjera",
                       "Entradas_netas_de_inversión_extranjera", "Crecimiento_poblacional")

# Preparar base para ACP
Empleos_df <- Empleos %>% select(-País)
rownames(Empleos_df) <- Empleos$País
Empleos_df <- Empleos_df %>% mutate(across(everything(), as.numeric)) %>% scale()

# ACP con FactoMineR
res.pca <- PCA(Empleos_df, graph = FALSE)

# Clustering sobre coordenadas de individuos
coord_ind <- res.pca$ind$coord
set.seed(123)
modelo_kmeans <- kmeans(coord_ind, centers = 4)
Grupo <- as.factor(modelo_kmeans$cluster)

# Visualización con factoextra
fviz_pca_ind(res.pca,
             geom.ind = "point",
             col.ind = Grupo,  # Color por grupo
             palette = c("#EE220D", "#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             label = "none",
             repel = TRUE,
             legend.title = "Grupo",
             title = "Gráfico de individuos según clustering sobre ACP"
)
