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
Muertes= na.omit(Muertes)
# Renombrar columnas
colnames(Muertes) <- c("País", "Expectativa_de_vida_mujer", "Expectativa_de-vida_hombres", "Accidentes_de_transito",
                       "Enfermedades_cardiacas_cancer_diabetes_mujeres", "Enfermedades_cardiacas_cancer_diabetes_hombres", 
                       "Polución_del_aire_mujeres", "Polución_del_aire_hombres", "Envenenamiento_accidental_mujeres", 
                       "Envenenamiento_accidental_hombres", "Tasa_de_mortalidad_mujeres_adultas","Tasa_de_mortalidad_hombres_adultos",
                       "Tasa_de_mortalidad_infantil_mujeres", "Tasa_de_mortalidad_infantil_varones", "Tasa_de_mortalidad_neonatal",
                       "Tasa_de_mortalidad_infantil_temprana_mujeres", "Tasa_de_mortalidad_infantil_temprana_hombres","Numero_de_muertes_infantiles",
                       "Numero_de_muertes_neonatales","Tasa_de_suicidios_mujeres","Tasa_de_suicidios_hombres","Supervivencia_hasta_los_65_años(%)_mujeres",
                       "Supervivencia_hasta_los_65_años(%)_hombres")

# Preparar base para ACP
Muertes_df= Muertes %>% select(-País)
rownames(Muertes_df) = Muertes$País
Muertes_df = Muertes_df %>% mutate(across(everything(), as.numeric)) %>% scale()

# ACP con FactoMineR
res.pca <- PCA(Muertes_df, graph = FALSE)

# Clustering sobre coordenadas de individuos
coord_ind <- res.pca$ind$coord
set.seed(123)
modelo_kmeans=kmeans(coord_ind, centers = 4)
Grupo=as.factor(modelo_kmeans$cluster)

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
library(FactoClass)
resultado_muertes = FactoClass(Muertes_df, dudi.pca)
# Visualizamos los grupos asignados
resultado_muertes$cluster

Muertes_clasificados =data.frame(Grupo = resultado_muertes$cluster, Muertes)
# Gráfico de individuos

plot(resultado_muertes$dudi)

# Círculo de correlaciones
s.corcircle((resultado_muertes$dudi)$co)

# Etiquetas de países
s.label((resultado_muertes$dudi)$li, 
        label = rownames(Muertes_df))

# Variables en componentes 1 y 2
s.label((resultado_muertes$dudi)$co, xax = 1, yax = 2, 
        sub = "Componentes 1 y 2", possub = "bottomright")

# Gráfico conjunto
scatter(resultado_muertes$dudi, xax = 1, yax = 2)

# Coloreamos por grupo
Grupo= Muertes_clasificados$Grupo
s.class((resultado_muertes$dudi)$li, fac = Grupo, sub = "Componentes 1 y 2", 
        possub = "bottomright", xax = 1, yax = 3, col = c(1,2,3,4))
View(Muertes_clasificados)

# Análisis de medias por grupo
resultado_muertes$carac.cont

