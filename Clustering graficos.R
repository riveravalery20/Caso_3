# GRÁFICOS MEJORADOS PARA ANÁLISIS DE COMPONENTES PRINCIPALES

# Librerias
library(ggplot2)
library(factoextra)
library(plotly)
library(ggrepel)


# ============================================================================
# DENDROGRAMA CLÁSICO MEJORADO (Vertical - Arriba hacia abajo)

library(dendextend)
library(ggplot2)
library(viridis)

# Realizar clustering jerárquico
distancia <- dist(Muertes_df, method = "euclidean")
modelo_jerarquico <- hclust(distancia, method = "ward.D2")

# Convertir a dendrograma
dend_modelo <- as.dendrogram(modelo_jerarquico)

# ============================================================================
# ANÁLISIS DE SILUETA PARA K ÓPTIMO
# ============================================================================

# Gráfico de silueta promedio
df_silueta <- data.frame(
  K = 2:max_k,
  Silueta = siluetas
)

p_silueta <- ggplot(df_silueta, aes(x = K, y = Silueta)) +
  geom_line(color = "#3498DB", size = 1.5) +
  geom_point(color = "#2980B9", size = 3) +
  geom_point(data = df_silueta[df_silueta$K == k_optimo, ], 
             aes(x = K, y = Silueta),
             color = "#E74C3C", size = 5, shape = 18) +
  geom_vline(xintercept = k_optimo, linetype = "dashed", color = "#E74C3C") +
  annotate("text", x = k_optimo, y = max(siluetas),
           label = paste("Óptimo: K =", k_optimo, "\nSilueta =", round(max(siluetas), 3)),
           color = "#E74C3C", size = 3.5, fontface = "bold", hjust = -0.1) +
  labs(title = "Método de Silueta Promedio - Número Óptimo de Clusters",
       x = "Número de Clusters (K)",
       y = "Ancho de Silueta Promedio") +
  scale_x_continuous(breaks = 2:max_k) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

print(p_silueta)


# Dendrograma interactivo con plotly
# ============================================================================

library(plotly)

# Crear dendrograma interactivo
dend_interactivo <- plot_ly() %>%
  add_segments(
    data = segment(dend_data),
    x = ~x, y = ~y,
    xend = ~xend, yend = ~yend,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#3498DB', width = 1),
    hoverinfo = 'none'
  ) %>%
  add_text(
    data = label(dend_data),
    x = ~x, y = ~y,
    text = ~label,
    type = 'scatter',
    mode = 'text',
    textfont = list(size = 10, color = '#2C3E50'),
    hoverinfo = 'text',
    hovertext = ~paste("País:", label, "<br>Altura:", round(y, 2))
  ) %>%
  layout(
    title = list(
      text = "Dendrograma Interactivo",
      font = list(size = 16, color = '#2C3E50')
    ),
    xaxis = list(
      title = "",
      showticklabels = FALSE,
      showgrid = FALSE
    ),
    yaxis = list(
      title = "Distancia",
      titlefont = list(size = 12)
    ),
    showlegend = FALSE,
    hovermode = 'closest'
  )

dend_interactivo

# GRÁFICO POR CLUSTERS (Componentes 1 y 2)
# --------------------------------------------
# Muestra cómo se agrupan los países según el análisis de clusters
# Permite validar visualmente la calidad del clustering

p5 <- fviz_pca_ind(resultado_ACP$dudi,
                   geom.ind = "point",
                   col.ind = as.factor(NuevaBase$Cluster),
                   palette = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12"),
                   addEllipses = TRUE,
                   ellipse.type = "convex",
                   legend.title = "Cluster",
                   title = "Agrupación de Países por Clusters (PC1 vs PC2)",
                   repel = TRUE) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

print(p5)

# Versión interactiva con colores por cluster
cluster_colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12")
p5_interactive <- plot_ly(data = data.frame(resultado_ACP$dudi$li),
                          x = ~Axis1,
                          y = ~Axis2,
                          text = row.names(Muertes_df),
                          color = as.factor(NuevaBase$Cluster),
                          colors = cluster_colors[1:length(unique(NuevaBase$Cluster))],
                          type = "scatter",
                          mode = "markers+text",
                          marker = list(size = 12, 
                                        line = list(width = 2)),
                          textposition = "top center",
                          hovertemplate = paste('<b>%{text}</b><br>',
                                                'PC1: %{x:.2f}<br>',
                                                'PC2: %{y:.2f}<br>',
                                                'Cluster: ', NuevaBase$Cluster,
                                                '<extra></extra>')) %>%
  layout(title = list(text = "Agrupación Interactiva por Clusters",
                      font = list(size = 16)),
         xaxis = list(title = "Componente Principal 1"),
         yaxis = list(title = "Componente Principal 2"),
         hovermode = "closest")

p5_interactive

# GRÁFICO POR CLUSTERS (Componentes 1 y 3)
# --------------------------------------------
# Muestra otra perspectiva usando el tercer componente
# Útil para detectar patrones no visibles en PC1-PC2

p6 <- fviz_pca_ind(resultado_ACP$dudi,
                   axes = c(1, 3),
                   geom.ind = "point",
                   col.ind = as.factor(NuevaBase$Cluster),
                   palette = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12"),
                   addEllipses = TRUE,
                   ellipse.type = "convex",
                   legend.title = "Cluster",
                   title = "Agrupación de Países por Clusters (PC1 vs PC3)",
                   repel = TRUE) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

print(p6)

# CONTRIBUCIÓN DE VARIABLES A LOS COMPONENTES
# --------------------------------------------
# Identifica qué variables contribuyen más a cada componente

p7 <- fviz_contrib(resultado_ACP$dudi, 
                   choice = "var", 
                   axes = 1:2,
                   top = 10,
                   fill = "#9C27B0",
                   color = "#6A1B9A",
                   title = "Top 10 Variables - Contribución a PC1 y PC2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p7)

# CALIDAD DE REPRESENTACIÓN (COS2)
# --------------------------------------------
# Muestra qué tan bien están representadas las variables

p8 <- fviz_cos2(resultado_ACP$dudi, 
                choice = "var", 
                axes = 1:2,
                top = 10,
                fill = "#00BCD4",
                color = "#00838F",
                title = "Calidad de Representación de Variables (Cos²)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p8)

# DASHBOARD INTERACTIVO 3D
# --------------------------------------------
# Visualización 3D interactiva de los tres primeros componentes

p9_3d <- plot_ly(data = data.frame(resultado_ACP$dudi$li),
                 x = ~Axis1,
                 y = ~Axis2,
                 z = ~Axis3,
                 text = row.names(Muertes_df),
                 color = as.factor(NuevaBase$Cluster),
                 colors = cluster_colors[1:length(unique(NuevaBase$Cluster))],
                 type = "scatter3d",
                 mode = "markers+text",
                 marker = list(size = 8),
                 hovertemplate = paste('<b>%{text}</b><br>',
                                       'PC1: %{x:.2f}<br>',
                                       'PC2: %{y:.2f}<br>',
                                       'PC3: %{z:.2f}<br>',
                                       '<extra></extra>')) %>%
  layout(title = "Visualización 3D de Clusters - ACP",
         scene = list(
           xaxis = list(title = "PC1"),
           yaxis = list(title = "PC2"),
           zaxis = list(title = "PC3")
         ))

p9_3d

# RESUMEN ESTADÍSTICO
# --------------------------------------------
# Tabla con varianza explicada por cada componente

resumen_acp <- data.frame(
  Componente = paste0("PC", 1:min(5, ncol(resultado_ACP$dudi$li))),
  Varianza = resultado_ACP$dudi$eig[1:min(5, length(resultado_ACP$dudi$eig))],
  Porcentaje = (resultado_ACP$dudi$eig[1:min(5, length(resultado_ACP$dudi$eig))] / 
                  sum(resultado_ACP$dudi$eig)) * 100,
  Acumulado = cumsum((resultado_ACP$dudi$eig[1:min(5, length(resultado_ACP$dudi$eig))] / 
                        sum(resultado_ACP$dudi$eig)) * 100)
)

print("Resumen del Análisis de Componentes Principales:")
print(resumen_acp)

# Guardar gráficos en alta resolución
# ggsave("scree_plot.png", p1, width = 10, height = 6, dpi = 300)
# ggsave("circulo_correlaciones.png", p2, width = 10, height = 8, dpi = 300)
# ggsave("mapa_paises.png", p3, width = 12, height = 8, dpi = 300)
# ggsave("biplot.png", p4, width = 12, height = 8, dpi = 300)
# ggsave("clusters_pc1_pc2.png", p5, width = 12, height = 8, dpi = 300)
# ggsave("clusters_pc1_pc3.png", p6, width = 12, height = 8, dpi = 300)


# ============================================================================
# RESULTADOS FINALES
# ============================================================================

cat("=== RESULTADOS DEL ANÁLISIS DE CLUSTERING ===\n")
cat("Número óptimo de clusters (silueta):", k_optimo, "\n")
cat("Silueta promedio óptima:", round(max(siluetas), 3), "\n")
cat("Altura de corte sugerida:", round(heights_per_k.dendrogram(dend_modelo)[as.character(k_optimo)], 2), "\n")

# Asignar clusters a los países
clusters_finales <- cutree(modelo_jerarquico, k = k_optimo)
cat("\nDistribución de países por cluster:\n")
print(table(clusters_finales))

