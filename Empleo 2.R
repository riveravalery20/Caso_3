# Cargar librerías necesarias
library(readr)
library(tidyverse)
library(FactoMineR)
library(factoextra)

# Leer la base
Empleos_ANS_2_ <- read_csv("C:/Users/etmel.DESKTOP-N79IL9B/Downloads/Empleos. ANS (2).csv")

# Limpieza y selección de variables
Empleos <- Empleos_ANS_2_ %>%
  select(-c(1, 3, 8, 9, 10, 21, 22, 24, 25, 26)) %>%
  slice(1:217) %>%  # Eliminar filas 218 a 247
  na.omit()

# Renombrar columnas
colnames(Empleos) <- c("País", "Empleo_total", "Fuerza_laboral_total", "Desempleo_total(%)",
                       "Gasto_en_educación", "Nuevas_empresas_registradas", "Población_total",
                       "Ingresos_fiscales", "Crecimiento_del_PIB", "PIB_per_capita", "Valor_añadido_Industrias",
                       "Valor_añadido_manufactura", "Volumen_de_exportación", "Salidas_netas_de_inversión_extranjera",
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
