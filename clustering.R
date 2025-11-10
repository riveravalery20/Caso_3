library(FactoClass)
library(FactoMineR)

# Opción 1: Modo interactivo (si quieres confirmar visualmente)
resultado_ACP <- FactoClass(Muertes_df, dudi.pca)

# Ver los clusters asignados a cada país
resultado_ACP$cluster

# Crear nueva base de datos con la variable cluster
NuevaBase <- data.frame(Cluster = resultado_ACP$cluster, Muertes_df)

# Visualizar la nueva base con clusters
View(NuevaBase)

# GRÁFICOS DEL ANÁLISIS CON CLUSTERING
# ============================================================================

# Gráfico del análisis (scree plot)
plot(resultado_ACP$dudi)

# Círculo de correlaciones (cómo se relacionan las variables)
s.corcircle((resultado_ACP$dudi)$co)

# Gráfico de individuos - países en el espacio de componentes
s.label((resultado_ACP$dudi)$li, label = row.names(Muertes_df))

# Gráfico de variables (componentes 1 y 2)
s.label((resultado_ACP$dudi)$co, xax = 1, yax = 2, 
        sub = "Componente 1 y 2", possub = "bottomright")

# Gráfico conjunto (scatter biplot)
scatter(resultado_ACP$dudi, xax = 1, yax = 2)

# Gráfico de clases/grupos coloreado por cluster (componentes 1 y 2)
Grupo <- NuevaBase$Cluster
s.class((resultado_ACP$dudi)$li, Grupo, 
        sub = "Componentes 1 y 2", possub = "bottomright",
        xax = 1, yax = 2, col = c(1, 2))  # Solo 2 colores porque tienes 2 clusters

# Gráfico de clases/grupos (componentes 1 y 3) - para ver otra perspectiva
s.class((resultado_ACP$dudi)$li, Grupo, 
        sub = "Componentes 1 y 3", possub = "bottomright",
        xax = 1, yax = 3, col = c(1, 2))

# ============================================================================
# DESCRIPCIÓN DE LOS GRUPOS (Análisis de medias)
# ============================================================================
# Esta tabla muestra qué variables caracterizan significativamente cada grupo
# Valores p < 0.05 indican diferencias significativas entre grupos
resultado_ACP$carac.cont

# Ver resumen de estadísticas por cluster
# Media de cada variable en cada cluster
aggregate(. ~ Cluster, data = NuevaBase, FUN = mean)

# Contar cuántos países hay en cada cluster
table(NuevaBase$Cluster)

