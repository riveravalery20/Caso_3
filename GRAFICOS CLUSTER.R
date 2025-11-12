#otra vez
library(FactoClass)
library(FactoMineR)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(ggrepel)
library(viridis)
library(ggforce)
library(factoextra)
library(cluster)

# Limpia y configura
graphics.off()
par(mar = c(4, 4, 3, 2))

resultado_ACP <- FactoClass(Muertes_df, dudi.pca)
NuevaBase <- data.frame(Cluster = resultado_ACP$cluster, Muertes_df)

# GRÁFICOS ORIGINALES DE FACTOCLASS (para comparación)
# ============================================================================

# Gráficos básicos de FactoClass
plot(resultado_ACP$dudi)
s.corcircle((resultado_ACP$dudi)$co)
s.label((resultado_ACP$dudi)$li, label = row.names(Muertes_df))

# Gráfico de variables
s.label((resultado_ACP$dudi)$co, xax = 1, yax = 2, 
        sub = "Componente 1 y 2", possub = "bottomright")

# Gráfico conjunto
scatter(resultado_ACP$dudi, xax = 1, yax = 2)

# Gráficos de clases/grupos
Grupo <- NuevaBase$Cluster
s.class((resultado_ACP$dudi)$li, Grupo, 
        sub = "Componentes 1 y 2", possub = "bottomright",
        xax = 1, yax = 2, col = c(1, 2))

s.class((resultado_ACP$dudi)$li, Grupo, 
        sub = "Componentes 1 y 3", possub = "bottomright",
        xax = 1, yax = 3, col = c(1, 2))

# ============================================================================
# VERSIÓN FINAL CON GGPLOT2 Y PLOTLY
# ============================================================================

# Extraer información del ACP para ggplot
acp_ind <- as.data.frame(resultado_ACP$dudi$li)
acp_var <- as.data.frame(resultado_ACP$dudi$co)

# Asignar nombres consistentes
colnames(acp_ind) <- paste0("Axis", 1:ncol(acp_ind))
colnames(acp_var) <- paste0("Axis", 1:ncol(acp_var))

acp_ind$Cluster <- as.factor(NuevaBase$Cluster)
acp_ind$Pais <- rownames(Muertes_df)
acp_var$Variable <- rownames(acp_var)

# Porcentaje de varianza explicada
eigenvals <- resultado_ACP$dudi$eig
var_exp <- round(eigenvals / sum(eigenvals) * 100, 2)

# ANÁLISIS JERÁRQUICO - DENDROGRAMA
# ============================================================================

# Crear matriz de distancia y dendrograma
dist_matrix <- dist(resultado_ACP$dudi$li[, 1:3])  # Usar las primeras 3 componentes
hc <- hclust(dist_matrix, method = "ward.D2")

# Dendrograma con ggplot2
dendro_plot <- fviz_dend(hc, k = 2, 
                         cex = 0.6,
                         k_colors = viridis(2),
                         color_labels_by_k = TRUE,
                         ggtheme = theme_minimal(),
                         main = "Dendrograma - Clasificación Jerárquica",
                         xlab = "Países", ylab = "Distancia")

print(dendro_plot)


# 1. GRÁFICO DE INDIVIDUOS (PAÍSES) - Componentes 1 y 2 Gráfico interactivo de países #BORRAR NO INTERACT Y PONER LEYENDA
plotly_p1 <- plot_ly(acp_ind, 
                     x = ~Axis1, y = ~Axis2, 
                     color = ~Cluster,
                     colors = viridis_pal()(2),
                     text = ~paste("País:", Pais, 
                                   "<br>Cluster:", Cluster,
                                   "<br>Comp1:", round(Axis1, 2),
                                   "<br>Comp2:", round(Axis2, 2)),
                     hoverinfo = "text",
                     type = "scatter",
                     mode = "markers",
                     marker = list(size = 10, opacity = 0.7, line = list(width = 1))) %>%
  layout(title = "ACP Interactivo - Países por Cluster",
         xaxis = list(title = paste0("Componente 1 (", var_exp[1], "%)")),
         yaxis = list(title = paste0("Componente 2 (", var_exp[2], "%)")),
         hoverlabel = list(bgcolor = "white"))

plotly_p1


# 2. CÍRCULO DE CORRELACIONES - Versión robusta
circle_data <- data.frame(
  Axis1 = acp_var[,1],
  Axis2 = acp_var[,2],
  Variable = rownames(acp_var)
)

p2 <- ggplot(circle_data, aes(x = Axis1, y = Axis2, label = Variable)) +
  geom_segment(aes(x = 0, y = 0, xend = Axis1, yend = Axis2),
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#E41A1C", alpha = 0.8, size = 1) +
  geom_text_repel(size = 4, fontface = "bold", color = "darkred",
                  max.overlaps = 20, box.padding = 0.7) +
  geom_point(size = 2, color = "#E41A1C") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), 
              inherit.aes = FALSE, color = "blue", linetype = "dashed") +
  coord_fixed(ratio = 1) +
  labs(
    title = "Círculo de Correlaciones",
    subtitle = "Componentes 1 y 2",
    x = paste0("Componente 1 (", var_exp[1], "%)"),
    y = paste0("Componente 2 (", var_exp[2], "%)")
  ) +
  theme_minimal()

print(p2)


# 3. Biplot interactivo
plotly_biplot <- plot_ly() %>%
  # Agregar países
  add_trace(data = acp_ind,
            x = ~Axis1, y = ~Axis2,
            color = ~Cluster,
            colors = viridis_pal()(2),
            text = ~paste("País:", Pais, "<br>Cluster:", Cluster),
            type = "scatter",
            mode = "markers",
            marker = list(size = 8, opacity = 0.6),
            name = "Países") %>%
  # Agregar variables
  add_trace(data = circle_data,
            x = ~Axis1*3, y = ~Axis2*3,
            text = ~Variable,
            type = "scatter",
            mode = "text",
            textfont = list(color = "red", size = 12),
            name = "Variables") %>%
  # Agregar flechas para variables
  add_annotations(data = circle_data,
                  x = ~Axis1*3, y = ~Axis2*3,
                  ax = 0, ay = 0,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  text = "",
                  showarrow = TRUE,
                  arrowhead = 3,
                  arrowsize = 1.5,
                  arrowwidth = 1.2,
                  arrowcolor = "rgba(255,0,0,0.8)") %>%
  layout(title = "Biplot Interactivo ACP",
         xaxis = list(title = paste0("Componente 1 (", var_exp[1], "%)")),
         yaxis = list(title = paste0("Componente 2 (", var_exp[2], "%)")))

plotly_biplot


# 4. GRÁFICO DE CLASES/GRUPOS CON ELIPSES (Componentes 1 y 2) interactivo de elipses

plotly_elipses <- plot_ly(acp_ind) %>%
  add_trace(x = ~Axis1, y = ~Axis2, color = ~Cluster,
            colors = viridis_pal()(2),
            text = ~paste("País:", Pais, "<br>Cluster:", Cluster),
            type = "scatter",
            mode = "markers",
            marker = list(size = 8, opacity = 0.7),
            name = "Países") %>%
  layout(title = "Clusters con Países - Componentes 1 y 2",
         xaxis = list(title = paste0("Componente 1 (", var_exp[1], "%)")),
         yaxis = list(title = paste0("Componente 2 (", var_exp[2], "%)")))

plotly_elipses

# 5. GRÁFICO DE CLASES/GRUPOS (Componentes 1 y 3)
if(ncol(acp_ind) >= 3) {
  p5 <- ggplot(acp_ind, aes(x = Axis1, y = Axis3, color = Cluster, fill = Cluster)) +
    geom_point(alpha = 0.7) +
    stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.95) +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    labs(
      title = "Clusters con Elipses de Confianza",
      subtitle = "Componentes 1 y 3 (95% de confianza)",
      x = paste0("Componente 1 (", var_exp[1], "%)"),
      y = paste0("Componente 3 (", var_exp[3], "%)"),
      color = "Cluster", fill = "Cluster"
    ) +
    theme_minimal()
  
  print(p5)
}


# ============================================================================
# GRÁFICOS ADICIONALES PROFESIONALES
# ============================================================================

# 6. GRÁFICO DE BARRAS - CONTRIBUCIÓN DE VARIABLES
contrib_data <- acp_var %>%
  mutate(Contrib1 = abs(Axis1) / sum(abs(Axis1)) * 100,
         Contrib2 = abs(Axis2) / sum(abs(Axis2)) * 100) %>%
  pivot_longer(cols = c(Contrib1, Contrib2), 
               names_to = "Componente", 
               values_to = "Contribucion")

p6 <- ggplot(contrib_data, aes(x = reorder(Variable, Contribucion), 
                               y = Contribucion, fill = Componente)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8) +
  labs(
    title = "Contribución de Variables a los Componentes Principales",
    x = "Variables",
    y = "Contribución Absoluta (%)",
    fill = "Componente"
  ) +
  coord_flip() +
  theme_minimal()

print(p6)

# 7. SCREE PLOT - VARIANZA EXPLICADA
scree_data <- data.frame(
  Componente = 1:length(var_exp),
  Varianza = var_exp,
  Acumulado = cumsum(var_exp)
)

p7 <- ggplot(scree_data, aes(x = factor(Componente), y = Varianza, group = 1)) +
  geom_line(color = "#377EB8", size = 1.2) +
  geom_point(color = "#377EB8", size = 3) +
  geom_text(aes(label = paste0(var_exp, "%")), vjust = -1, size = 3.5) +
  labs(
    title = "Scree Plot - Varianza Explicada por Componente",
    x = "Componentes Principales",
    y = "Porcentaje de Varianza Explicada (%)"
  ) +
  theme_minimal()

print(p7)

# ============================================================================
# RESUMEN ESTADÍSTICO MEJORADO
# ============================================================================

cat("=== RESUMEN DEL ANÁLISIS ACP Y CLUSTERING ===\n")
cat("Número total de países:", nrow(NuevaBase), "\n")
cat("Número de clusters:", length(unique(NuevaBase$Cluster)), "\n\n")

# Distribución por cluster
distribucion <- table(NuevaBase$Cluster)
cat("Distribución de países por cluster:\n")
print(distribucion)
cat("\n")

# Varianza explicada
cat("Varianza explicada por componentes:\n")
for(i in 1:min(20, length(var_exp))) {
  cat("Componente", i, ":", var_exp[i], "%\n")
}
cat("Varianza total explicada (3 componentes):", sum(var_exp[1:3]), "%\n")

# Medias por cluster
cat("\nMedias de variables por cluster:\n")
medias_cluster <- NuevaBase %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(medias_cluster)

