library(readr)
library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)

Muertes_ans = read_csv("Muertes ans.csv")
View(Muertes_ans)
Muertes=data.frame(Muertes_ans)

# Limpieza y selección de variables

Muertes=Muertes[-(218:245), ]
Muertes= na.omit(Muertes)
names(Muertes)


# Renombrar columnas

colnames(Muertes) = c("País", "Expectativa_de_vida_mujer", "Expectativa_de-vida_hombres", "Accidentes_de_transito",
                       "Enfermedades_cardiacas_cancer_diabetes_mujeres", "Enfermedades_cardiacas_cancer_diabetes_hombres", 
                       "Polución_del_aire_mujeres", "Polución_del_aire_hombres", "Envenenamiento_accidental_mujeres", 
                       "Envenenamiento_accidental_hombres", "Tasa_de_mortalidad_mujeres_adultas","Tasa_de_mortalidad_hombres_adultos",
                       "Tasa_de_mortalidad_infantil_mujeres", "Tasa_de_mortalidad_infantil_varones", "Tasa_de_mortalidad_neonatal",
                       "Tasa_de_mortalidad_infantil_temprana_mujeres", "Tasa_de_mortalidad_infantil_temprana_hombres","Numero_de_muertes_infantiles",
                       "Numero_de_muertes_neonatales","Tasa_de_suicidios_mujeres","Tasa_de_suicidios_hombres","Supervivencia_hasta_los_65_años(%)_mujeres",
                       "Supervivencia_hasta_los_65_años(%)_hombres")

#MAPA MUNDIAL INTERACTIVO (PRIMER GRÁFICO)

world <- st_read("custom.geo.json")
names(world)

mapa_mundo <- left_join(world, Muertes, by = c("sovereignt" = "País"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(viridis)
library(gganimate)

# --- 1️⃣ Reorganizar la base a formato largo ---

Muertes <- Muertes %>%
  mutate(across(
    -País,                     
    ~ suppressWarnings(as.numeric(.))  
  ))

base_larga <- Muertes %>%
  pivot_longer(
    cols = c(
      "Expectativa_de_vida_mujer", "Expectativa_de-vida_hombres",
      "Enfermedades_cardiacas_cancer_diabetes_mujeres", "Enfermedades_cardiacas_cancer_diabetes_hombres",
      "Polución_del_aire_mujeres", "Polución_del_aire_hombres",
      "Envenenamiento_accidental_mujeres", "Envenenamiento_accidental_hombres",
      "Tasa_de_mortalidad_mujeres_adultas", "Tasa_de_mortalidad_hombres_adultos",
      "Tasa_de_mortalidad_infantil_mujeres", "Tasa_de_mortalidad_infantil_varones",
      "Tasa_de_mortalidad_infantil_temprana_mujeres", "Tasa_de_mortalidad_infantil_temprana_hombres",
      "Tasa_de_suicidios_mujeres", "Tasa_de_suicidios_hombres",
      "Supervivencia_hasta_los_65_años(%)_mujeres", "Supervivencia_hasta_los_65_años(%)_hombres"
    ),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(
    sexo = case_when(
      grepl("_mujer|_mujeres", variable) ~ "Mujeres",
      grepl("_hombre|_hombres|_varones", variable) ~ "Hombres",
      TRUE ~ "Otro"
    ),
   
    variable = gsub("_mujer(es)?|_hombre(s)?|_varones", "", variable),
    variable = gsub("_", " ", variable)
  )

# --
mapa_completo <- mapa_mundo %>%
  left_join(base_larga, by = c("sovereignt" = "País"))


animacion <- ggplot(mapa_completo) +
  geom_sf(aes(fill = valor), color = NA) +
  facet_wrap(~sexo) +
  scale_fill_viridis(option = "magma", direction = -1, name = "Valor") +
  theme_minimal() +
  labs(
    title = "Causa de muerte: {closest_state} (2019)",
    subtitle = "Comparación por sexo en todos los países",
    caption = "Fuente: Indicadores de Desarrollo Mundial (Banco Mundial)"
  ) +
  transition_states(
    variable,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes("linear")


animate(animacion, nframes = 200, fps = 10, width = 900, height = 500)


anim_save("mapa_muertes_2019.gif", animation = last_animation())
animacion_render <- animate(animacion, nframes = 200, fps = 10, width = 900, height = 500)
animacion_render  # Mostrar en el visor

#MUERTES TOTALES POR SEXO.

muertes_sexo <- base_larga %>%
  group_by(sexo) %>%
  summarise(total = sum(valor, na.rm = TRUE))
ggplot(muertes_sexo, aes(x = sexo, y = total, fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Hombres" = "#0B5CAD", "Mujeres" = "#E47792")) +
  labs(
    title = "Muertes totales por sexo",
    x = "Sexo",
    y = "Total de muertes"
  ) +
  theme_minimal()

#CAUSAS PRINCIPALES POR SEXO


top_causas <- base_larga %>%
  group_by(variable) %>%
  summarise(total = sum(valor, na.rm = TRUE)) %>%
  top_n(22, total)
top_causas <- base_larga %>%
  filter(variable %in% c("Enfermedades cardiacas cancer diabetes", "Tasa de suicidios",
                          "Polución del aire","Envenenamiento accidental"))

ggplot(
  base_larga %>% filter(variable %in% top_causas$variable),
  aes(x = reorder(variable, valor), y = valor, fill = sexo)
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +scale_fill_manual(values = c("Hombres" = "#0B5CAD", "Mujeres" = "#E47792")) +

  labs(
    title = "Principales causas de muerte por sexo",
    x = "Causa de muerte",
    y = "Número de muertes"
  ) +
  theme_minimal()
#Mapas 



