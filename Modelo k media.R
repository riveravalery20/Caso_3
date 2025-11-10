# AHORA SÍ PUEDES EJECUTAR TU ANÁLISIS PCA

# Preparar datos para PCA (CORREGIDO - usar "Pais" en lugar de "País")
Muertes_df <- Muertes %>%
  select(-Pais) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()
rownames(Muertes_df) <- Muertes$Pais

# Verificar que los datos estén listos
View(Muertes_df)
print(paste("Dimensión de Muertes_df:", dim(Muertes_df)))

# Análisis PCA
res.pca <- prcomp(Muertes_df, scale = TRUE)
print(res.pca)

# Visualización de eigenvalues
fviz_eig(res.pca)

# Visualización de individuos y variables
fviz_pca_ind(res.pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_var(res.pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")

# Resultados numéricos del PCA
eig.val <- get_eigenvalue(res.pca)
print(eig.val)

res.var <- get_pca_var(res.pca)
View(res.var$contrib)
print("Contribuciones de variables a las dimensiones:")
print(colSums(res.var$contrib[,1:2]))

res.ind <- get_pca_ind(res.pca)
View(res.ind$contrib)

# Preparar datos para clustering (con las 3 primeras dimensiones)
pca_scores <- as.data.frame(res.pca$x[, 1:3])
colnames(pca_scores) <- c("Dim1", "Dim2", "Dim3")

# Encontrar número óptimo de clusters
fviz_nbclust(pca_scores, FUN = kmeans, method = "silhouette")
fviz_nbclust(pca_scores, FUN = kmeans, method = "wss")

# Ajustar el modelo y agrupar
set.seed(123) # Para reproducibilidad
modelo <- kmeans(pca_scores, centers = 2)
pca_scores <- pca_scores %>%
  mutate(cluster = modelo$cluster)

# Ver resultados del clustering
print(table(pca_scores$cluster))

# Opcional: visualizar clusters en el plano PCA
fviz_cluster(modelo, data = pca_scores, 
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())
