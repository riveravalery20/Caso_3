# Preparar datos para PCA (CORREGIDO - usar "Pais" en lugar de "País")
Muertes_df <- Muertes %>%
  select(-Pais) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()
rownames(Muertes_df) <- Muertes$Pais

# Verificar que los datos estén listos
View(Muertes_df)
print(paste("Dimension de Muertes_df:", dim(Muertes_df)))

# Análisis PCA
res.pca <- prcomp(Muertes_df, scale = TRUE)
print(res.pca)

# Visualización de eigenvalues
library(factoextra)
fviz_eig(res.pca)


# Resultados numéricos del PCA
eig.val <- get_eigenvalue(res.pca)
print(eig.val)

res.var <- get_pca_var(res.pca)
print("Contribuciones de variables a las dimensiones:")
print(colSums(res.var$contrib[,1:2]))

res.ind <- get_pca_ind(res.pca)

# Preparar datos para clustering (con las 3 primeras dimensiones)
pca_scores <- as.data.frame(res.pca$x[, 1:3])
colnames(pca_scores) <- c("Dim1", "Dim2", "Dim3")


#MODELO WARD

#Metodo de Ward o Jerarquico
library(tidyverse)

dist_entre_paises <- dist(pca_scores)
modelo_jerarquico <- hclust(dist_entre_paises, method = "ward.D2")

library(dendextend)

dend_modelo <- as.dendrogram(modelo_jerarquico)
plot(dend_modelo)
modelo_jerarquico

# Análisis de número óptimo de clusters
fviz_nbclust(pca_scores, FUN = hcut, method = "silhouette")

# Crear clusters
paises_agrupados <- pca_scores %>% 
  mutate(cluster = cutree(modelo_jerarquico, k = 2))

# Ver los nombres de las columnas para identificar las correctas
colnames(paises_agrupados)

# Gráfico corregido - OPCIÓN 1: Si las componentes se llaman PC1 y PC2
paises_agrupados %>% 
  ggplot() +
  aes(Dim1, Dim2, Dim3, color = factor(cluster)) +
  geom_point(size = 5) +
  theme(legend.position = "none") +
  labs(x = "Componente Principal 1", y = "Componente Principal 2")

