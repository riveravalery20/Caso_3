#MAPA 1--------------------------------

library(leaflet)
library(dplyr)
library(sf)
library(rworldmap)
library(readr)

# Leer la base de datos (ajusta la ruta si es necesario)
datos= Muertes
datos <- datos %>%
  mutate(across(-País, ~ suppressWarnings(as.numeric(gsub(",", ".", .)))))

# Preparar datos: seleccionar y renombrar columnas
datos <- datos %>%
  select(
    País = País,
    expectativa_vida_mujer = Expectativa_de_vida_mujer,
    expectativa_vida_hombres = Expectativa_de_vida_hombres,
    accidentes_transito = Accidentes_de_transito,
    enfermedades_cvd_mujeres = Enfermedades_cardiacas_cancer_diabetes_mujeres,
    enfermedades_cvd_hombres = Enfermedades_cardiacas_cancer_diabetes_hombres,
    polucion_aire_mujeres = Polución_del_aire_mujeres,
    polucion_aire_hombres = Polución_del_aire_hombres,
    envenenamiento_mujeres = Envenenamiento_accidental_mujeres,
    envenenamiento_hombres = Envenenamiento_accidental_hombres,
    mortalidad_adultos_mujeres = Tasa_de_mortalidad_mujeres_adultas,
    mortalidad_adultos_hombres = Tasa_de_mortalidad_hombres_adultos,
    mortalidad_infantil_mujeres = Tasa_de_mortalidad_infantil_mujeres,
    mortalidad_infantil_varones = Tasa_de_mortalidad_infantil_varones,
    mortalidad_neonatal = Tasa_de_mortalidad_neonatal,
    mortalidad_infantil_temprana_mujeres = Tasa_de_mortalidad_infantil_temprana_mujeres,
    mortalidad_infantil_temprana_hombres = Tasa_de_mortalidad_infantil_temprana_hombres,
    numero_muertes_infantiles = Numero_de_muertes_infantiles,
    numero_muertes_neonatales = Numero_de_muertes_neonatales,
    suicidios_mujeres = Tasa_de_suicidios_mujeres,
    suicidios_hombres = Tasa_de_suicidios_hombres,
    supervivencia_65_mujeres = Supervivencia_hasta_los_65_años_mujeres,
    supervivencia_65_hombres = Supervivencia_hasta_los_65_años_hombres
  ) %>%
  mutate(
    # Calcular frecuencias (suma para género, directo para sin género)
    frecuencia_expectativa_vida = expectativa_vida_mujer + expectativa_vida_hombres,
    frecuencia_accidentes = accidentes_transito,
    frecuencia_enfermedades_cvd = enfermedades_cvd_mujeres + enfermedades_cvd_hombres,
    frecuencia_polucion = polucion_aire_mujeres + polucion_aire_hombres,
    frecuencia_envenenamiento = envenenamiento_mujeres + envenenamiento_hombres,
    frecuencia_mortalidad_adultos = mortalidad_adultos_mujeres + mortalidad_adultos_hombres,
    frecuencia_mortalidad_infantil = mortalidad_infantil_mujeres + mortalidad_infantil_varones,
    frecuencia_mortalidad_neonatal = mortalidad_neonatal,
    frecuencia_mortalidad_temprana = mortalidad_infantil_temprana_mujeres + mortalidad_infantil_temprana_hombres,
    frecuencia_numero_infantiles = numero_muertes_infantiles,
    frecuencia_numero_neonatales = numero_muertes_neonatales,
    frecuencia_suicidios = suicidios_mujeres + suicidios_hombres,
    frecuencia_supervivencia_65 = supervivencia_65_mujeres + supervivencia_65_hombres
  )

# Lista de variables para el selector (nombres amigables)
variables <- c(
  "Expectativa de Vida" = "frecuencia_expectativa_vida",
  "Accidentes de Tránsito" = "frecuencia_accidentes",
  "Enfermedades Cardíacas, Cáncer, Diabetes" = "frecuencia_enfermedades_cvd",
  "Polución del Aire" = "frecuencia_polucion",
  "Envenenamiento Accidental" = "frecuencia_envenenamiento",
  "Tasa de Mortalidad Adultos" = "frecuencia_mortalidad_adultos",
  "Tasa de Mortalidad Infantil" = "frecuencia_mortalidad_infantil",
  "Tasa de Mortalidad Neonatal" = "frecuencia_mortalidad_neonatal",
  "Tasa de Mortalidad Infantil Temprana" = "frecuencia_mortalidad_temprana",
  "Número de Muertes Infantiles" = "frecuencia_numero_infantiles",
  "Número de Muertes Neonatales" = "frecuencia_numero_neonatales",
  "Tasa de Suicidios" = "frecuencia_suicidios",
  "Supervivencia hasta los 65 Años" = "frecuencia_supervivencia_65"
)

# Obtener datos geoespaciales
world <- getMap(resolution = "low")
world_sf <- st_as_sf(world)

# Función para crear el mapa
crear_mapa <- function(variable_seleccionada) {
  col_frecuencia <- variables[variable_seleccionada]
  
  # Definir columnas de hombres y mujeres según la variable
  if (variable_seleccionada == "Expectativa de Vida") {
    col_hombres <- "expectativa_vida_hombres"
    col_mujeres <- "expectativa_vida_mujer"
  } else if (variable_seleccionada == "Enfermedades Cardíacas, Cáncer, Diabetes") {
    col_hombres <- "enfermedades_cvd_hombres"
    col_mujeres <- "enfermedades_cvd_mujeres"
  } else if (variable_seleccionada == "Polución del Aire") {
    col_hombres <- "polucion_aire_hombres"
    col_mujeres <- "polucion_aire_mujeres"
  } else if (variable_seleccionada == "Envenenamiento Accidental") {
    col_hombres <- "envenenamiento_hombres"
    col_mujeres <- "envenenamiento_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Adultos") {
    col_hombres <- "mortalidad_adultos_hombres"
    col_mujeres <- "mortalidad_adultos_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil") {
    col_hombres <- "mortalidad_infantil_varones"
    col_mujeres <- "mortalidad_infantil_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil Temprana") {
    col_hombres <- "mortalidad_infantil_temprana_hombres"
    col_mujeres <- "mortalidad_infantil_temprana_mujeres"
  } else if (variable_seleccionada == "Tasa de Suicidios") {
    col_hombres <- "suicidios_hombres"
    col_mujeres <- "suicidios_mujeres"
  } else if (variable_seleccionada == "Supervivencia hasta los 65 Años") {
    col_hombres <- "supervivencia_65_hombres"
    col_mujeres <- "supervivencia_65_mujeres"
  } else {
    # Para variables sin género, no hay hombres/mujeres
    col_hombres <- NA
    col_mujeres <- NA
  }
  
  world_sf <- world_sf %>%
    left_join(datos, by = c("NAME" = "País"))
  
  pal <- colorNumeric(palette = "Greens", domain = world_sf[[col_frecuencia]], na.color = "#cccccc")
  
  mapa <- leaflet(world_sf) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(get(col_frecuencia)),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste0(
        "<strong>País:</strong> ", NAME, "<br>",
        if (!is.na(col_hombres)) paste0("<strong>Hombres:</strong> ", ifelse(is.na(get(col_hombres)), "Sin datos", get(col_hombres)), "<br>") else "",
        if (!is.na(col_mujeres)) paste0("<strong>Mujeres:</strong> ", ifelse(is.na(get(col_mujeres)), "Sin datos", get(col_mujeres))) else paste0("<strong>Total:</strong> ", ifelse(is.na(get(col_frecuencia)), "Sin datos", get(col_frecuencia)))
      ) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) %>%
    addLegend(pal = pal, values = ~get(col_frecuencia), opacity = 0.7, title = paste("Frecuencia -", variable_seleccionada),
              position = "bottomright")
  
  return(mapa)
}

# Mapa inicial
mapa_inicial <- crear_mapa("Expectativa de Vida")
mapa_inicial

# Shiny para selector interactivo
shinyApp(ui, server)

#MAPA 2-----------------------------------------------

library(leaflet)
library(dplyr)
library(sf)
library(rworldmap)
library(readr)
library(countrycode) 

# Leer la base de datos
datos <- Muertes
datos <- datos %>%
  mutate(across(-País, ~ suppressWarnings(as.numeric(gsub(",", ".", .)))))

# Preparar datos: seleccionar y renombrar columnas
datos <- datos %>%
  select(
    País = País,
    expectativa_vida_mujer = Expectativa_de_vida_mujer,
    expectativa_vida_hombres = Expectativa_de_vida_hombres,
    accidentes_transito = Accidentes_de_transito,
    enfermedades_cvd_mujeres = Enfermedades_cardiacas_cancer_diabetes_mujeres,
    enfermedades_cvd_hombres = Enfermedades_cardiacas_cancer_diabetes_hombres,
    polucion_aire_mujeres = Polución_del_aire_mujeres,
    polucion_aire_hombres = Polución_del_aire_hombres,
    envenenamiento_mujeres = Envenenamiento_accidental_mujeres,
    envenenamiento_hombres = Envenenamiento_accidental_hombres,
    mortalidad_adultos_mujeres = Tasa_de_mortalidad_mujeres_adultas,
    mortalidad_adultos_hombres = Tasa_de_mortalidad_hombres_adultos,
    mortalidad_infantil_mujeres = Tasa_de_mortalidad_infantil_mujeres,
    mortalidad_infantil_varones = Tasa_de_mortalidad_infantil_varones,
    mortalidad_neonatal = Tasa_de_mortalidad_neonatal,
    mortalidad_infantil_temprana_mujeres = Tasa_de_mortalidad_infantil_temprana_mujeres,
    mortalidad_infantil_temprana_hombres = Tasa_de_mortalidad_infantil_temprana_hombres,
    numero_muertes_infantiles = Numero_de_muertes_infantiles,
    numero_muertes_neonatales = Numero_de_muertes_neonatales,
    suicidios_mujeres = Tasa_de_suicidios_mujeres,
    suicidios_hombres = Tasa_de_suicidios_hombres,
    supervivencia_65_mujeres = Supervivencia_hasta_los_65_años_mujeres,
    supervivencia_65_hombres = Supervivencia_hasta_los_65_años_hombres
  ) %>%
  mutate(
    # Calcular frecuencias
    frecuencia_expectativa_vida = expectativa_vida_mujer + expectativa_vida_hombres,
    frecuencia_accidentes = accidentes_transito,
    frecuencia_enfermedades_cvd = enfermedades_cvd_mujeres + enfermedades_cvd_hombres,
    frecuencia_polucion = polucion_aire_mujeres + polucion_aire_hombres,
    frecuencia_envenenamiento = envenenamiento_mujeres + envenenamiento_hombres,
    frecuencia_mortalidad_adultos = mortalidad_adultos_mujeres + mortalidad_adultos_hombres,
    frecuencia_mortalidad_infantil = mortalidad_infantil_mujeres + mortalidad_infantil_varones,
    frecuencia_mortalidad_neonatal = mortalidad_neonatal,
    frecuencia_mortalidad_temprana = mortalidad_infantil_temprana_mujeres + mortalidad_infantil_temprana_hombres,
    frecuencia_numero_infantiles = numero_muertes_infantiles,
    frecuencia_numero_neonatales = numero_muertes_neonatales,
    frecuencia_suicidios = suicidios_mujeres + suicidios_hombres,
    frecuencia_supervivencia_65 = supervivencia_65_mujeres + supervivencia_65_hombres,
    # Nuevo: Convertir nombres de países a códigos ISO3 para un join preciso
    iso3 = countrycode(País, "country.name", "iso3c", warn = FALSE)  # warn=FALSE para evitar mensajes de error
  ) %>%
  filter(!is.na(iso3))  # Opcional: filtrar países que no se pudieron convertir (si quieres)

# Lista de variables
variables <- c(
  "Expectativa de Vida" = "frecuencia_expectativa_vida",
  "Accidentes de Tránsito" = "frecuencia_accidentes",
  "Enfermedades Cardíacas, Cáncer, Diabetes" = "frecuencia_enfermedades_cvd",
  "Polución del Aire" = "frecuencia_polucion",
  "Envenenamiento Accidental" = "frecuencia_envenenamiento",
  "Tasa de Mortalidad Adultos" = "frecuencia_mortalidad_adultos",
  "Tasa de Mortalidad Infantil" = "frecuencia_mortalidad_infantil",
  "Tasa de Mortalidad Neonatal" = "frecuencia_mortalidad_neonatal",
  "Tasa de Mortalidad Infantil Temprana" = "frecuencia_mortalidad_temprana",
  "Número de Muertes Infantiles" = "frecuencia_numero_infantiles",
  "Número de Muertes Neonatales" = "frecuencia_numero_neonatales",
  "Tasa de Suicidios" = "frecuencia_suicidios",
  "Supervivencia hasta los 65 Años" = "frecuencia_supervivencia_65"
)

# Obtener datos geoespaciales
world <- getMap(resolution = "low")
world_sf <- st_as_sf(world)

# Función para crear el mapa
crear_mapa <- function(variable_seleccionada) {
  col_frecuencia <- variables[variable_seleccionada]
  
  # Definir columnas de hombres y mujeres
  if (variable_seleccionada == "Expectativa de Vida") {
    col_hombres <- "expectativa_vida_hombres"
    col_mujeres <- "expectativa_vida_mujer"
  } else if (variable_seleccionada == "Enfermedades Cardíacas, Cáncer, Diabetes") {
    col_hombres <- "enfermedades_cvd_hombres"
    col_mujeres <- "enfermedades_cvd_mujeres"
  } else if (variable_seleccionada == "Polución del Aire") {
    col_hombres <- "polucion_aire_hombres"
    col_mujeres <- "polucion_aire_mujeres"
  } else if (variable_seleccionada == "Envenenamiento Accidental") {
    col_hombres <- "envenenamiento_hombres"
    col_mujeres <- "envenenamiento_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Adultos") {
    col_hombres <- "mortalidad_adultos_hombres"
    col_mujeres <- "mortalidad_adultos_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil") {
    col_hombres <- "mortalidad_infantil_varones"
    col_mujeres <- "mortalidad_infantil_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil Temprana") {
    col_hombres <- "mortalidad_infantil_temprana_hombres"
    col_mujeres <- "mortalidad_infantil_temprana_mujeres"
  } else if (variable_seleccionada == "Tasa de Suicidios") {
    col_hombres <- "suicidios_hombres"
    col_mujeres <- "suicidios_mujeres"
  } else if (variable_seleccionada == "Supervivencia hasta los 65 Años") {
    col_hombres <- "supervivencia_65_hombres"
    col_mujeres <- "supervivencia_65_mujeres"
  } else {
    col_hombres <- NA
    col_mujeres <- NA
  }
  
  # Nuevo: Join por códigos ISO3 en lugar de nombres
  world_sf <- world_sf %>%
    left_join(datos, by = c("ISO3" = "iso3"))
  
  pal <- colorNumeric(palette = "Greens", domain = world_sf[[col_frecuencia]], na.color = "#cccccc")
  
  mapa <- leaflet(world_sf) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(get(col_frecuencia)),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste0(
        "<strong>País:</strong> ", NAME, "<br>",
        if (!is.na(col_hombres)) paste0("<strong>Hombres:</strong> ", ifelse(is.na(get(col_hombres)), "Sin datos", get(col_hombres)), "<br>") else "",
        if (!is.na(col_mujeres)) paste0("<strong>Mujeres:</strong> ", ifelse(is.na(get(col_mujeres)), "Sin datos", get(col_mujeres))) else paste0("<strong>Total:</strong> ", ifelse(is.na(get(col_frecuencia)), "Sin datos", get(col_frecuencia)))
      ) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) %>%
    addLegend(pal = pal, values = ~get(col_frecuencia), opacity = 0.7, title = paste("Frecuencia -", variable_seleccionada),
              position = "bottomright")
  
  return(mapa)
}

# Mapa inicial
mapa_inicial <- crear_mapa("Expectativa de Vida")
mapa_inicial

# Shiny para selector interactivo
library(shiny)
ui <- fluidPage(
  selectInput("variable", "Selecciona Variable:", choices = names(variables)),
  leafletOutput("mapa")
)
server <- function(input, output) {
  output$mapa <- renderLeaflet({
    crear_mapa(input$variable)
  })
}
shinyApp(ui, server)


#MAPA 3------------------------

library(leaflet)
library(dplyr)
library(sf)
library(rworldmap)
library(readr)
library(countrycode)

# Leer la base de datos
datos <-Muertes
datos <- datos %>%
  mutate(across(.cols = -País, .fns = ~as.numeric(.)))

# Preparar datos
datos <- datos %>%
  select(
    País = País,
    expectativa_vida_mujer = Expectativa_de_vida_mujer,
    expectativa_vida_hombres = Expectativa_de_vida_hombres,
    accidentes_transito = Accidentes_de_transito,
    enfermedades_cvd_mujeres = Enfermedades_cardiacas_cancer_diabetes_mujeres,
    enfermedades_cvd_hombres = Enfermedades_cardiacas_cancer_diabetes_hombres,
    polucion_aire_mujeres = Polución_del_aire_mujeres,
    polucion_aire_hombres = Polución_del_aire_hombres,
    envenenamiento_mujeres = Envenenamiento_accidental_mujeres,
    envenenamiento_hombres = Envenenamiento_accidental_hombres,
    mortalidad_adultos_mujeres = Tasa_de_mortalidad_mujeres_adultas,
    mortalidad_adultos_hombres = Tasa_de_mortalidad_hombres_adultos,
    mortalidad_infantil_mujeres = Tasa_de_mortalidad_infantil_mujeres,
    mortalidad_infantil_varones = Tasa_de_mortalidad_infantil_varones,
    mortalidad_neonatal = Tasa_de_mortalidad_neonatal,
    mortalidad_infantil_temprana_mujeres = Tasa_de_mortalidad_infantil_temprana_mujeres,
    mortalidad_infantil_temprana_hombres = Tasa_de_mortalidad_infantil_temprana_hombres,
    numero_muertes_infantiles = Numero_de_muertes_infantiles,
    numero_muertes_neonatales = Numero_de_muertes_neonatales,
    suicidios_mujeres = Tasa_de_suicidios_mujeres,
    suicidios_hombres = Tasa_de_suicidios_hombres,
    supervivencia_65_mujeres = Supervivencia_hasta_los_65_años_mujeres,
    supervivencia_65_hombres = Supervivencia_hasta_los_65_años_hombres
  ) %>%
  mutate(
    frecuencia_expectativa_vida = expectativa_vida_mujer + expectativa_vida_hombres,
    frecuencia_accidentes = accidentes_transito,
    frecuencia_enfermedades_cvd = enfermedades_cvd_mujeres + enfermedades_cvd_hombres,
    frecuencia_polucion = polucion_aire_mujeres + polucion_aire_hombres,
    frecuencia_envenenamiento = envenenamiento_mujeres + envenenamiento_hombres,
    frecuencia_mortalidad_adultos = mortalidad_adultos_mujeres + mortalidad_adultos_hombres,
    frecuencia_mortalidad_infantil = mortalidad_infantil_mujeres + mortalidad_infantil_varones,
    frecuencia_mortalidad_neonatal = mortalidad_neonatal,
    frecuencia_mortalidad_temprana = mortalidad_infantil_temprana_mujeres + mortalidad_infantil_temprana_hombres,
    frecuencia_numero_infantiles = numero_muertes_infantiles,
    frecuencia_numero_neonatales = numero_muertes_neonatales,
    frecuencia_suicidios = suicidios_mujeres + suicidios_hombres,
    frecuencia_supervivencia_65 = supervivencia_65_mujeres + supervivencia_65_hombres,
    iso3 = countrycode(País, "country.name", "iso3c", warn = FALSE)
  ) %>%
  mutate(
    iso3 = recode(País,
                  "Japón" = "JPN",
                  "Jamaica" = "JAM",
                  "México" = "MEX",
                  "Estados Unidos" = "USA",
                  "Reino Unido" = "GBR",
                  "Francia" = "FRA",
                  "Alemania" = "DEU",
                  "Italia" = "ITA",
                  "España" = "ESP",
                  "Brasil" = "BRA",
                  "Argentina" = "ARG",
                  "Chile" = "CHL",
                  "Colombia" = "COL",
                  "Perú" = "PER",
                  "Venezuela" = "VEN",
                  "Ecuador" = "ECU",
                  "Bolivia" = "BOL",
                  "Paraguay" = "PRY",
                  "Uruguay" = "URY",
                  "Panamá" = "PAN",
                  "Costa Rica" = "CRI",
                  "Nicaragua" = "NIC",
                  "Honduras" = "HND",
                  "El Salvador" = "SLV",
                  "Guatemala" = "GTM",
                  "Belice" = "BLZ",
                  "Cuba" = "CUB",
                  "República Dominicana" = "DOM",
                  "Haití" = "HTI",
                  "Puerto Rico" = "PRI",
                  "Canadá" = "CAN",
                  "Australia" = "AUS",
                  "Nueva Zelanda" = "NZL",
                  "China" = "CHN",
                  "India" = "IND",
                  "Rusia" = "RUS",
                  "Sudáfrica" = "ZAF",
                  .default = iso3
    )
  ) %>%
  filter(!is.na(iso3))

# Lista de variables
variables <- c(
  "Expectativa de Vida" = "frecuencia_expectativa_vida",
  "Accidentes de Tránsito" = "frecuencia_accidentes",
  "Enfermedades Cardíacas, Cáncer, Diabetes" = "frecuencia_enfermedades_cvd",
  "Polución del Aire" = "frecuencia_polucion",
  "Envenenamiento Accidental" = "frecuencia_envenenamiento",
  "Tasa de Mortalidad Adultos" = "frecuencia_mortalidad_adultos",
  "Tasa de Mortalidad Infantil" = "frecuencia_mortalidad_infantil",
  "Tasa de Mortalidad Neonatal" = "frecuencia_mortalidad_neonatal",
  "Tasa de Mortalidad Infantil Temprana" = "frecuencia_mortalidad_temprana",
  "Número de Muertes Infantiles" = "frecuencia_numero_infantiles",
  "Número de Muertes Neonatales" = "frecuencia_numero_neonatales",
  "Tasa de Suicidios" = "frecuencia_suicidios",
  "Supervivencia hasta los 65 Años" = "frecuencia_supervivencia_65"
)

# Obtener datos geoespaciales
world <- getMap(resolution = "low")
world_sf <- st_as_sf(world)

# Función para crear el mapa
crear_mapa <- function(variable_seleccionada) {
  # Definir variable_seleccionada en el entorno de la función
  assign("variable_seleccionada", variable_seleccionada, envir = environment())
  
  col_frecuencia <- variables[variable_seleccionada]
  
  # Definir col_hombres y col_mujeres al inicio
  col_hombres <- NA
  col_mujeres <- NA
  if (variable_seleccionada == "Expectativa de Vida") {
    col_hombres <- "expectativa_vida_hombres"
    col_mujeres <- "expectativa_vida_mujer"
  } else if (variable_seleccionada == "Enfermedades Cardíacas, Cáncer, Diabetes") {
    col_hombres <- "enfermedades_cvd_hombres"
    col_mujeres <- "enfermedades_cvd_mujeres"
  } else if (variable_seleccionada == "Polución del Aire") {
    col_hombres <- "polucion_aire_hombres"
    col_mujeres <- "polucion_aire_mujeres"
  } else if (variable_seleccionada == "Envenenamiento Accidental") {
    col_hombres <- "envenenamiento_hombres"
    col_mujeres <- "envenenamiento_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Adultos") {
    col_hombres <- "mortalidad_adultos_hombres"
    col_mujeres <- "mortalidad_adultos_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil") {
    col_hombres <- "mortalidad_infantil_varones"
    col_mujeres <- "mortalidad_infantil_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil Temprana") {
    col_hombres <- "mortalidad_infantil_temprana_hombres"
    col_mujeres <- "mortalidad_infantil_temprana_mujeres"
  } else if (variable_seleccionada == "Tasa de Suicidios") {
    col_hombres <- "suicidios_hombres"
    col_mujeres <- "suicidios_mujeres"
  } else if (variable_seleccionada == "Supervivencia hasta los 65 Años") {
    col_hombres <- "supervivencia_65_hombres"
    col_mujeres <- "supervivencia_65_mujeres"
  }
  
  # Preparar expresiones en el entorno de la función
  if (!is.na(col_hombres)) {
    assign("hombres_col", sym(col_hombres), envir = environment())
  } else {
    assign("hombres_col", NA, envir = environment())
  }
  if (!is.na(col_mujeres)) {
    assign("mujeres_col", sym(col_mujeres), envir = environment())
  } else {
    assign("mujeres_col", NA, envir = environment())
  }
  assign("frecuencia_col", sym(col_frecuencia), envir = environment())
  
  world_sf <- world_sf %>%
    left_join(datos, by = c("ISO3" = "iso3"))
  
  # Preparar valores seguros para tooltip (aplicado a todo world_sf)
  world_sf <- world_sf %>%
    mutate(
      hombres_val = if (!is.na(hombres_col)) !!hombres_col else NA_real_,
      mujeres_val = if (!is.na(mujeres_col)) !!mujeres_col else NA_real_,
      frecuencia_val = !!frecuencia_col
    )
  
  pal <- colorNumeric(palette = "Greens", domain = world_sf$frecuencia_val, na.color = "#cccccc")
  
  mapa <- leaflet(world_sf) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(frecuencia_val),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste0(
        "<strong>País:</strong> ", NAME, "<br>",
        ifelse(!is.na(hombres_val), paste0("<strong>Hombres:</strong> ", hombres_val, "<br>"), ""),
        ifelse(!is.na(mujeres_val), paste0("<strong>Mujeres:</strong> ", mujeres_val), ""),
        ifelse(is.na(hombres_val) & is.na(mujeres_val), paste0("<strong>Total:</strong> ", ifelse(is.na(frecuencia_val), "Sin datos", frecuencia_val)), "")
      ) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) %>%
    addLegend(pal = pal, values = ~frecuencia_val, opacity = 0.7, title = paste("Frecuencia -", variable_seleccionada),
              position = "bottomright")
  
  return(mapa)
}
  
  # Definir col_hombres y col_mujeres al inicio
  col_hombres <- NA
  col_mujeres <- NA
  if (variable_seleccionada == "Expectativa de Vida") {
    col_hombres <- "expectativa_vida_hombres"
    col_mujeres <- "expectativa_vida_mujer"
  } else if (variable_seleccionada == "Enfermedades Cardíacas, Cáncer, Diabetes") {
    col_hombres <- "enfermedades_cvd_hombres"
    col_mujeres <- "enfermedades_cvd_mujeres"
  } else if (variable_seleccionada == "Polución del Aire") {
    col_hombres <- "polucion_aire_hombres"
    col_mujeres <- "polucion_aire_mujeres"
  } else if (variable_seleccionada == "Envenenamiento Accidental") {
    col_hombres <- "envenenamiento_hombres"
    col_mujeres <- "envenenamiento_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Adultos") {
    col_hombres <- "mortalidad_adultos_hombres"
    col_mujeres <- "mortalidad_adultos_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil") {
    col_hombres <- "mortalidad_infantil_varones"
    col_mujeres <- "mortalidad_infantil_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil Temprana") {
    col_hombres <- "mortalidad_infantil_temprana_hombres"
    col_mujeres <- "mortalidad_infantil_temprana_mujeres"
  } else if (variable_seleccionada == "Tasa de Suicidios") {
    col_hombres <- "suicidios_hombres"
    col_mujeres <- "suicidios_mujeres"
  } else if (variable_seleccionada == "Supervivencia hasta los 65 Años") {
    col_hombres <- "supervivencia_65_hombres"
    col_mujeres <- "supervivencia_65_mujeres"
  }
  
  world_sf <- world_sf %>%
    left_join(datos, by = c("ISO3" = "iso3"))
  
  # Preparar expresiones en el entorno de la función
  if (!is.na(col_hombres)) {
    assign("hombres_col", sym(col_hombres), envir = environment())
  } else {
    assign("hombres_col", NA, envir = environment())
  }
  if (!is.na(col_mujeres)) {
    assign("mujeres_col", sym(col_mujeres), envir = environment())
  } else {
    assign("mujeres_col", NA, envir = environment())
  }
  assign("frecuencia_col", sym(col_frecuencia), envir = environment())
  
  # Preparar valores seguros para tooltip (aplicado a todo world_sf)
  world_sf <- world_sf %>%
    mutate(
      hombres_val = if (!is.na(hombres_col)) !!hombres_col else NA_real_,
      mujeres_val = if (!is.na(mujeres_col)) !!mujeres_col else NA_real_,
      frecuencia_val = !!frecuencia_col
    )
# Mapa inicial
mapa_inicial <- crear_mapa("Expectativa de Vida")
mapa_inicial

# Shiny para selector
library(shiny)
ui <- fluidPage(
  selectInput("variable", "Selecciona Variable:", choices = names(variables)),
  leafletOutput("mapa")
)
server <- function(input, output) {
  output$mapa <- renderLeaflet({
    crear_mapa(input$variable)
  })
}
shinyApp(ui, server)

#MAPA 4-------------------
# Cargar librerías necesarias

library(leaflet)
library(dplyr)
library(sf)
library(rworldmap)
library(readr)
library(countrycode)
library(shiny)

# Leer la base de datos (asumiendo que 'Muertes' es un dataframe disponible en el entorno)
datos <- Muertes

# Convertir columnas numéricas (excepto 'País') a numérico
datos <- datos %>%
  mutate(across(.cols = -País, .fns = ~as.numeric(.)))

# Preparar y renombrar columnas
datos <- datos %>%
  select(
    País = País,
    expectativa_vida_mujer = Expectativa_de_vida_mujer,
    expectativa_vida_hombres = Expectativa_de_vida_hombres,
    accidentes_transito = Accidentes_de_transito,
    enfermedades_cvd_mujeres = Enfermedades_cardiacas_cancer_diabetes_mujeres,
    enfermedades_cvd_hombres = Enfermedades_cardiacas_cancer_diabetes_hombres,
    polucion_aire_mujeres = Polución_del_aire_mujeres,
    polucion_aire_hombres = Polución_del_aire_hombres,
    envenenamiento_mujeres = Envenenamiento_accidental_mujeres,
    envenenamiento_hombres = Envenenamiento_accidental_hombres,
    mortalidad_adultos_mujeres = Tasa_de_mortalidad_mujeres_adultas,
    mortalidad_adultos_hombres = Tasa_de_mortalidad_hombres_adultos,
    mortalidad_infantil_mujeres = Tasa_de_mortalidad_infantil_mujeres,
    mortalidad_infantil_varones = Tasa_de_mortalidad_infantil_varones,
    mortalidad_neonatal = Tasa_de_mortalidad_neonatal,
    mortalidad_infantil_temprana_mujeres = Tasa_de_mortalidad_infantil_temprana_mujeres,
    mortalidad_infantil_temprana_hombres = Tasa_de_mortalidad_infantil_temprana_hombres,
    numero_muertes_infantiles = Numero_de_muertes_infantiles,
    numero_muertes_neonatales = Numero_de_muertes_neonatales,
    suicidios_mujeres = Tasa_de_suicidios_mujeres,
    suicidios_hombres = Tasa_de_suicidios_hombres,
    supervivencia_65_mujeres = Supervivencia_hasta_los_65_años_mujeres,
    supervivencia_65_hombres = Supervivencia_hasta_los_65_años_hombres
  ) %>%
  mutate(
    frecuencia_expectativa_vida = expectativa_vida_mujer + expectativa_vida_hombres,
    frecuencia_accidentes = accidentes_transito,
    frecuencia_enfermedades_cvd = enfermedades_cvd_mujeres + enfermedades_cvd_hombres,
    frecuencia_polucion = polucion_aire_mujeres + polucion_aire_hombres,
    frecuencia_envenenamiento = envenenamiento_mujeres + envenenamiento_hombres,
    frecuencia_mortalidad_adultos = mortalidad_adultos_mujeres + mortalidad_adultos_hombres,
    frecuencia_mortalidad_infantil = mortalidad_infantil_mujeres + mortalidad_infantil_varones,
    frecuencia_mortalidad_neonatal = mortalidad_neonatal,
    frecuencia_mortalidad_temprana = mortalidad_infantil_temprana_mujeres + mortalidad_infantil_temprana_hombres,
    frecuencia_numero_infantiles = numero_muertes_infantiles,
    frecuencia_numero_neonatales = numero_muertes_neonatales,
    frecuencia_suicidios = suicidios_mujeres + suicidios_hombres,
    frecuencia_supervivencia_65 = supervivencia_65_mujeres + supervivencia_65_hombres,
    iso3 = countrycode(País, "country.name", "iso3c", warn = FALSE)
  ) %>%
  mutate(
    iso3 = recode(País,
                  "Japón" = "JPN",
                  "Jamaica" = "JAM",
                  "México" = "MEX",
                  "Estados Unidos" = "USA",
                  "Reino Unido" = "GBR",
                  "Francia" = "FRA",
                  "Alemania" = "DEU",
                  "Italia" = "ITA",
                  "España" = "ESP",
                  "Brasil" = "BRA",
                  "Argentina" = "ARG",
                  "Chile" = "CHL",
                  "Colombia" = "COL",
                  "Perú" = "PER",
                  "Venezuela" = "VEN",
                  "Ecuador" = "ECU",
                  "Bolivia" = "BOL",
                  "Paraguay" = "PRY",
                  "Uruguay" = "URY",
                  "Panamá" = "PAN",
                  "Costa Rica" = "CRI",
                  "Nicaragua" = "NIC",
                  "Honduras" = "HND",
                  "El Salvador" = "SLV",
                  "Guatemala" = "GTM",
                  "Belice" = "BLZ",
                  "Cuba" = "CUB",
                  "República Dominicana" = "DOM",
                  "Haití" = "HTI",
                  "Puerto Rico" = "PRI",
                  "Canadá" = "CAN",
                  "Australia" = "AUS",
                  "Nueva Zelanda" = "NZL",
                  "China" = "CHN",
                  "India" = "IND",
                  "Rusia" = "RUS",
                  "Sudáfrica" = "ZAF",
                  .default = iso3
    )
  ) %>%
  filter(!is.na(iso3))

# Lista de variables para el selector
variables <- c(
  "Expectativa de Vida" = "frecuencia_expectativa_vida",
  "Accidentes de Tránsito" = "frecuencia_accidentes",
  "Enfermedades Cardíacas, Cáncer, Diabetes" = "frecuencia_enfermedades_cvd",
  "Polución del Aire" = "frecuencia_polucion",
  "Envenenamiento Accidental" = "frecuencia_envenenamiento",
  "Tasa de Mortalidad Adultos" = "frecuencia_mortalidad_adultos",
  "Tasa de Mortalidad Infantil" = "frecuencia_mortalidad_infantil",
  "Tasa de Mortalidad Neonatal" = "frecuencia_mortalidad_neonatal",
  "Tasa de Mortalidad Infantil Temprana" = "frecuencia_mortalidad_temprana",
  "Número de Muertes Infantiles" = "frecuencia_numero_infantiles",
  "Número de Muertes Neonatales" = "frecuencia_numero_neonatales",
  "Tasa de Suicidios" = "frecuencia_suicidios",
  "Supervivencia hasta los 65 Años" = "frecuencia_supervivencia_65"
)

# Obtener datos geoespaciales del mundo
world <- getMap(resolution = "low")
world_sf <- st_as_sf(world)

# Función para crear el mapa interactivo
crear_mapa <- function(variable_seleccionada) {
  # Definir variable_seleccionada en el entorno de la función
  assign("variable_seleccionada", variable_seleccionada, envir = environment())
  
  col_frecuencia <- variables[variable_seleccionada]
  
  # Definir col_hombres y col_mujeres al inicio
  col_hombres <- NA
  col_mujeres <- NA
  if (variable_seleccionada == "Expectativa de Vida") {
    col_hombres <- "expectativa_vida_hombres"
    col_mujeres <- "expectativa_vida_mujer"
  } else if (variable_seleccionada == "Enfermedades Cardíacas, Cáncer, Diabetes") {
    col_hombres <- "enfermedades_cvd_hombres"
    col_mujeres <- "enfermedades_cvd_mujeres"
  } else if (variable_seleccionada == "Polución del Aire") {
    col_hombres <- "polucion_aire_hombres"
    col_mujeres <- "polucion_aire_mujeres"
  } else if (variable_seleccionada == "Envenenamiento Accidental") {
    col_hombres <- "envenenamiento_hombres"
    col_mujeres <- "envenenamiento_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Adultos") {
    col_hombres <- "mortalidad_adultos_hombres"
    col_mujeres <- "mortalidad_adultos_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil") {
    col_hombres <- "mortalidad_infantil_varones"
    col_mujeres <- "mortalidad_infantil_mujeres"
  } else if (variable_seleccionada == "Tasa de Mortalidad Infantil Temprana") {
    col_hombres <- "mortalidad_infantil_temprana_hombres"
    col_mujeres <- "mortalidad_infantil_temprana_mujeres"
  } else if (variable_seleccionada == "Tasa de Suicidios") {
    col_hombres <- "suicidios_hombres"
    col_mujeres <- "suicidios_mujeres"
  } else if (variable_seleccionada == "Supervivencia hasta los 65 Años") {
    col_hombres <- "supervivencia_65_hombres"
    col_mujeres <- "supervivencia_65_mujeres"
  }
  
  # Preparar expresiones en el entorno de la función
  if (!is.na(col_hombres)) {
    assign("hombres_col", sym(col_hombres), envir = environment())
  } else {
    assign("hombres_col", NA, envir = environment())
  }
  if (!is.na(col_mujeres)) {
    assign("mujeres_col", sym(col_mujeres), envir = environment())
  } else {
    assign("mujeres_col", NA, envir = environment())
  }
  assign("frecuencia_col", sym(col_frecuencia), envir = environment())
  
  world_sf <- world_sf %>%
    left_join(datos, by = c("ISO3" = "iso3"))
  
  # Preparar valores seguros para tooltip (aplicado a todo world_sf)
  world_sf <- world_sf %>%
    mutate(
      hombres_val = if (!is.na(hombres_col)) !!hombres_col else NA_real_,  # Agregado: verificación para evitar !!NA
      mujeres_val = if (!is.na(mujeres_col)) !!mujeres_col else NA_real_,  # Agregado: verificación para evitar !!NA
      frecuencia_val = !!frecuencia_col
    )
  
  pal <- colorNumeric(palette = "Greens", domain = world_sf$frecuencia_val, na.color = "#cccccc")
  
  mapa <- leaflet(world_sf) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(frecuencia_val),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste0(
        "<strong>País:</strong> ", NAME, "<br>",
        ifelse(!is.na(hombres_val), paste0("<strong>Hombres:</strong> ", hombres_val, "<br>"), ""),
        ifelse(!is.na(mujeres_val), paste0("<strong>Mujeres:</strong> ", mujeres_val), ""),
        ifelse(is.na(hombres_val) & is.na(mujeres_val), paste0("<strong>Total:</strong> ", ifelse(is.na(frecuencia_val), "Sin datos", frecuencia_val)), "")
      ) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) %>%
    addLegend(pal = pal, values = ~frecuencia_val, opacity = 0.7, title = paste("Frecuencia -", variable_seleccionada),
              position = "bottomright")
  
  return(mapa)
}
# Crear mapa inicial con la primera variable
mapa_inicial <- crear_mapa("Expectativa de Vida")
print(mapa_inicial)  # Mostrar el mapa inicial (opcional, para verificar)

# Definir la interfaz de usuario (UI) para Shiny
ui <- fluidPage(
  titlePanel("Mapa Interactivo de Datos de Mortalidad por País"),
  selectInput("variable", "Selecciona Variable:", choices = names(variables)),
  leafletOutput("mapa")
)

# Definir el servidor para Shiny
server <- function(input, output) {
  output$mapa <- renderLeaflet({
    crear_mapa(input$variable)
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
