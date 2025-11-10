# ANÁLISIS CON CLUSTERING (FactoClass)
# ============================================================================

library(FactoClass)
library(FactoMineR)

# Realizar ACP con clustering automático
# El sistema te preguntará interactivamente:
# 1. Número de ejes a retener (ejemplo: 3)
# 2. Número de clústers K-means (ejemplo: 3)
# 3. Número de clústers jerárquicos (ejemplo: 4)

resultado_ACP <- FactoClass(Muertes_df, dudi.pca)

#Cerrar dispositivos gráficos
dev.off()
# O cerrar TODOS si tienes varios abiertos
while (!is.null(dev.list())) dev.off()
# Intentar de nuevo
NuevaBase <- data.frame(Cluster = resultado_ACP$cluster, Muertes)
View(NuevaBase)

# Descripción de grupos
resultado_ACP$carac.cont

resultado_ACP <- FactoClass(Muertes_df, dudi.pca)
# Responde: 3, 3, 4 (o los valores que consideres apropiados)

# Ver los clusters asignados
resultado_ACP$cluster

# Crear nueva base de datos con la variable cluster
NuevaBase <- data.frame(Cluster = resultado_ACP$cluster, Muertes)

# Visualizar la nueva base con clusters
View(NuevaBase)

# ============================================================================
# GRÁFICOS DEL ANÁLISIS CON CLUSTERING
# ============================================================================

# Gráfico del análisis
plot(resultado_ACP$dudi)

# Círculo de correlaciones
s.corcircle((resultado_ACP$dudi)$co)

# Gráfico de individuos (países)
s.label((resultado_ACP$dudi)$li, label = row.names(Muertes_df))

# Gráfico de variables (componentes 1 y 2)
s.label((resultado_ACP$dudi)$co, xax = 1, yax = 2, 
        sub = "Componente 1 y 2", possub = "bottomright")

# Gráfico conjunto (scatter)
scatter(resultado_ACP$dudi, xax = 1, yax = 2)

# Gráfico de clases/grupos coloreado por cluster
Grupo <- NuevaBase$Cluster
s.class((resultado_ACP$dudi)$li, Grupo, 
        sub = "Componentes 1 y 2", possub = "bottomright",
        xax = 1, yax = 3, col = c(1, 2, 3, 4))

# ============================================================================
# DESCRIPCIÓN DE LOS GRUPOS (Análisis de medias)
# ============================================================================

# Esta tabla muestra qué variables caracterizan significativamente cada grupo
# Valores p < 0.05 indican diferencias significativas entre grupos
resultado_ACP$carac.cont