library(factoextra)
# Preparar base para ACP
Muertes_df <- Muertes %>% 
  select(-Pais) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()
rownames(Muertes_df) <- Muertes$Pais

head(Muertes_df[, 1:6]) # Ver las primeras filas y columnas

# REALIZAR EL ANÁLISIS DE COMPONENTES PRINCIPALES YA ESCALADOS

res.pca <- prcomp(Muertes_df, scale = FALSE)
res.pca # resumen del ACP

# VISUALIZACIONES PRINCIPALES
# Gráfico de sedimentación (scree plot)

fviz_eig(res.pca)

# Gráfico de individuos (países)
# Color por calidad de representación (cos2)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color por la calidad de representación
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

# Gráfico de variables
# Color por contribución a los componentes principales
fviz_pca_var(res.pca,
             col.var = "contrib", # Color por contribuciones a los CP
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             axes = c(1,2) # Evitar superposición de texto
)

# Biplot: visualización conjunta de individuos y variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Color de las variables
                col.ind = "#696969", # Color de los individuos
                axes = c(1,2) # Componentes 1 y 2
)

# RESULTADOS

library(factoextra)

# Valores propios (Eigenvalues)
eig.val <- get_eigenvalue(res.pca)
eig.val


# Resultados para Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordenadas
res.var$contrib        # Contribuciones a los CPs
res.var$cos2           # Calidad de representación


# Visualizar contribuciones de variables a los primeros componentes
colSums(res.var$contrib[,1:2])

# Resultados para individuos (países)
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordenadas
res.ind$contrib        # Contribuciones a los CPs
res.ind$cos2           # Calidad de representación

# Visualizar contribuciones de países a los primeros componentes
View(res.ind$contrib[,1:3]) # Miro los primeros factores
res.ind$contrib[,1:2]

