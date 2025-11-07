library(readr)
Empleos_ANS_2_ <- read_csv("C:/Users/etmel.DESKTOP-N79IL9B/Downloads/Empleos. ANS (2).csv")
View(Empleos_ANS_2_)

Empleos=data.frame(Empleos_ANS_2_)

#Limpieza de datos.
library(tidyverse)

Empleos = Empleos_ANS_2_ %>% select(-c(1,3,8,9,10,21,22,24,25,26))
Empleos=Empleos[-(218:247), ]

Empleos=na.omit(Empleos)

#Renombre de columnas
colnames(Empleos)=c("País","Empleo_total","Fuerza_laboral_total","Desempleo_total(%)",
                     "Gasto_en_educación","Nuevas_empresas_registradas","Población_total",
                     "Ingresos_fiscales","Crecimiento_del_PIB","PIB_per_capita","Valor_añadido_Industrias",
                     "Valor_añadido_manufactura","Volumen_de_exportación","Salidas_netas_de_inversión_extranjera",
                     "Entradas_netas_de_inversión_extranjera","Crecimiento_poblacional")

#PREPARACIÓN DEL MODELO

library(tidyverse)
library(FactoClass)

Empleos_df = as.data.frame(Empleos)
rownames(Empleos_df) <- Empleos_df$País
Empleos_df = Empleos_df[,-1] 
Empleos_df = Empleos_df %>% mutate(across(everything(), as.numeric))


resultado_empleos = FactoClass(Empleos_df, dudi.pca)

# Visualizamos los grupos asignados
resultado_empleos$cluster

Empleos_clasificados =data.frame(Grupo = resultado_empleos$cluster, Empleos)
# Gráfico de individuos
plot(resultado_empleos$dudi)

# Círculo de correlaciones
s.corcircle((resultado_empleos$dudi)$co)

# Etiquetas de países
s.label((resultado_empleos$dudi)$li, 
        label = rownames(Empleos_df))

# Variables en componentes 1 y 2
s.label((resultado_empleos$dudi)$co, xax = 1, yax = 2, 
        sub = "Componentes 1 y 2", possub = "bottomright")

# Gráfico conjunto
scatter(resultado_empleos$dudi, xax = 1, yax = 2)

# Coloreamos por grupo
Grupo <- Empleos_clasificados$Grupo
s.class((resultado_empleos$dudi)$li, fac = Grupo, sub = "Componentes 1 y 2", 
        possub = "bottomright", xax = 1, yax = 3, col = c(1,2,3,4))
View(Empleos_clasificados)

# Análisis de medias por grupo
resultado_empleos$carac.cont
