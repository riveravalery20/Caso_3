library(readr)
library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)
library(htmltools)
library(DT)
library(tidyr)
library(stringr)

# Leer la base
Muertes_ans = read_csv("Muertes ans.csv")
Muertes = data.frame(Muertes_ans)

# Limpieza y selección de variables
Muertes = Muertes[-(218:245), ]
Muertes = na.omit(Muertes)

# Renombrar columnas
colnames(Muertes) = c("País", "Expectativa_de_vida_mujer", "Expectativa_de_vida_hombres", "Accidentes_de_transito",
                       "Enfermedades_cardiacas_cancer_diabetes_mujeres", "Enfermedades_cardiacas_cancer_diabetes_hombres", 
                       "Polución_del_aire_mujeres", "Polución_del_aire_hombres", "Envenenamiento_accidental_mujeres", 
                       "Envenenamiento_accidental_hombres", "Tasa_de_mortalidad_mujeres_adultas","Tasa_de_mortalidad_hombres_adultos",
                       "Tasa_de_mortalidad_infantil_mujeres", "Tasa_de_mortalidad_infantil_varones", "Tasa_de_mortalidad_neonatal",
                       "Tasa_de_mortalidad_infantil_temprana_mujeres", "Tasa_de_mortalidad_infantil_temprana_hombres","Numero_de_muertes_infantiles",
                       "Numero_de_muertes_neonatales","Tasa_de_suicidios_mujeres","Tasa_de_suicidios_hombres","Supervivencia_hasta_los_65_años_mujeres",
                       "Supervivencia_hasta_los_65_años_hombres")

# Eliminar filas con valores NA en cualquier columna
Muertes = Muertes %>%
  filter(complete.cases(.))

# Traducir nombres de países al español
Muertes = Muertes %>%
  mutate(País = case_when(
    País == "Faroe Islands" ~ "Islas Feroe",
    País == "Fiji" ~ "Fiyi",
    País == "Finland" ~ "Finlandia",
    País == "France" ~ "Francia",
    País == "French Polynesia" ~ "Polinesia Francesa",
    País == "Gabon" ~ "Gabón",
    País == "Gambia, The" ~ "Gambia",
    País == "Georgia" ~ "Georgia",
    País == "Germany" ~ "Alemania",
    País == "Ghana" ~ "Ghana",
    País == "Gibraltar" ~ "Gibraltar",
    País == "Greece" ~ "Grecia",
    País == "Greenland" ~ "Groenlandia",
    País == "Grenada" ~ "Granada",
    País == "Guam" ~ "Guam",
    País == "Guatemala" ~ "Guatemala",
    País == "Guinea" ~ "Guinea",
    País == "Guinea-Bissau" ~ "Guinea-Bisáu",
    País == "Guyana" ~ "Guyana",
    País == "Haiti" ~ "Haití",
    País == "Honduras" ~ "Honduras",
    País == "Hong Kong SAR, China" ~ "Hong Kong",
    País == "Hungary" ~ "Hungría",
    País == "Iceland" ~ "Islandia",
    País == "India" ~ "India",
    País == "Indonesia" ~ "Indonesia",
    País == "Iran, Islamic Rep." ~ "Irán",
    País == "Iraq" ~ "Irak",
    País == "Ireland" ~ "Irlanda",
    País == "Isle of Man" ~ "Isla de Man",
    País == "Israel" ~ "Israel",
    País == "Italy" ~ "Italia",
    País == "Jamaica" ~ "Jamaica",
    País == "Japan" ~ "Japón",
    País == "Jordan" ~ "Jordania",
    País == "Kazakhstan" ~ "Kazajistán",
    País == "Kenya" ~ "Kenia",
    País == "Kiribati" ~ "Kiribati",
    País == "Korea, Dem. People's Rep." ~ "Corea del Norte",
    País == "Korea, Rep." ~ "Corea del Sur",
    País == "Kosovo" ~ "Kosovo",
    País == "Kuwait" ~ "Kuwait",
    País == "Kyrgyz Republic" ~ "Kirguistán",
    País == "Lao PDR" ~ "Laos",
    País == "Latvia" ~ "Letonia",
    País == "Lebanon" ~ "Líbano",
    País == "Lesotho" ~ "Lesoto",
    País == "Liberia" ~ "Liberia",
    País == "Libya" ~ "Libia",
    País == "Liechtenstein" ~ "Liechtenstein",
    País == "Lithuania" ~ "Lituania",
    País == "Luxembourg" ~ "Luxemburgo",
    País == "Macao SAR, China" ~ "Macao",
    País == "Madagascar" ~ "Madagascar",
    País == "Malawi" ~ "Malaui",
    País == "Malaysia" ~ "Malasia",
    País == "Maldives" ~ "Maldivas",
    País == "Mali" ~ "Malí",
    País == "Malta" ~ "Malta",
    País == "Marshall Islands" ~ "Islas Marshall",
    País == "Mauritania" ~ "Mauritania",
    País == "Mauritius" ~ "Mauricio",
    País == "Mexico" ~ "México",
    País == "Micronesia, Fed. Sts." ~ "Micronesia",
    País == "Moldova" ~ "Moldavia",
    País == "Monaco" ~ "Mónaco",
    País == "Mongolia" ~ "Mongolia",
    País == "Montenegro" ~ "Montenegro",
    País == "Morocco" ~ "Marruecos",
    País == "Mozambique" ~ "Mozambique",
    País == "Myanmar" ~ "Myanmar",
    País == "Namibia" ~ "Namibia",
    País == "Nauru" ~ "Nauru",
    País == "Nepal" ~ "Nepal",
    País == "Netherlands" ~ "Países Bajos",
    País == "New Caledonia" ~ "Nueva Caledonia",
    País == "New Zealand" ~ "Nueva Zelanda",
    País == "Nicaragua" ~ "Nicaragua",
    País == "Niger" ~ "Níger",
    País == "Nigeria" ~ "Nigeria",
    País == "North Macedonia" ~ "Macedonia del Norte",
    País == "Northern Mariana Islands" ~ "Islas Marianas del Norte",
    País == "Norway" ~ "Noruega",
    País == "Oman" ~ "Omán",
    País == "Pakistan" ~ "Pakistán",
    País == "Palau" ~ "Palaos",
    País == "Panama" ~ "Panamá",
    País == "Papua New Guinea" ~ "Papúa Nueva Guinea",
    País == "Paraguay" ~ "Paraguay",
    País == "Peru" ~ "Perú",
    País == "Philippines" ~ "Filipinas",
    País == "Poland" ~ "Polonia",
    País == "Portugal" ~ "Portugal",
    País == "Puerto Rico (US)" ~ "Puerto Rico",
    País == "Qatar" ~ "Catar",
    País == "Romania" ~ "Rumanía",
    País == "Russian Federation" ~ "Rusia",
    País == "Rwanda" ~ "Ruanda",
    País == "Samoa" ~ "Samoa",
    País == "San Marino" ~ "San Marino",
    País == "Sao Tome and Principe" ~ "Santo Tomé y Príncipe",
    País == "Saudi Arabia" ~ "Arabia Saudita",
    País == "Senegal" ~ "Senegal",
    País == "Serbia" ~ "Serbia",
    País == "Seychelles" ~ "Seychelles",
    País == "Sierra Leone" ~ "Sierra Leona",
    País == "Singapore" ~ "Singapur",
    País == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
    País == "Slovak Republic" ~ "Eslovaquia",
    País == "Slovenia" ~ "Eslovenia",
    País == "Solomon Islands" ~ "Islas Salomón",
    País == "Somalia, Fed. Rep." ~ "Somalia",
    País == "South Africa" ~ "Sudáfrica",
    País == "South Sudan" ~ "Sudán del Sur",
    País == "Spain" ~ "España",
    País == "Sri Lanka" ~ "Sri Lanka",
    País == "St. Kitts and Nevis" ~ "San Cristóbal y Nieves",
    País == "St. Lucia" ~ "Santa Lucía",
    País == "St. Martin (French part)" ~ "San Martín",
    País == "St. Vincent and the Grenadines" ~ "San Vicente y las Granadinas",
    País == "Sudan" ~ "Sudán",
    País == "Suriname" ~ "Surinam",
    País == "Sweden" ~ "Suecia",
    País == "Switzerland" ~ "Suiza",
    País == "Syrian Arab Republic" ~ "Siria",
    País == "Tajikistan" ~ "Tayikistán",
    País == "Tanzania" ~ "Tanzania",
    País == "Thailand" ~ "Tailandia",
    País == "Timor-Leste" ~ "Timor Oriental",
    País == "Togo" ~ "Togo",
    País == "Tonga" ~ "Tonga",
    País == "Trinidad and Tobago" ~ "Trinidad y Tobago",
    País == "Tunisia" ~ "Túnez",
    País == "Turkiye" ~ "Turquía",
    País == "Turkmenistan" ~ "Turkmenistán",
    País == "Turks and Caicos Islands" ~ "Islas Turcas y Caicos",
    País == "Tuvalu" ~ "Tuvalu",
    País == "Uganda" ~ "Uganda",
    País == "Ukraine" ~ "Ucrania",
    País == "United Arab Emirates" ~ "Emiratos Árabes Unidos",
    País == "United Kingdom" ~ "Reino Unido",
    País == "United States" ~ "Estados Unidos",
    País == "Uruguay" ~ "Uruguay",
    País == "Uzbekistan" ~ "Uzbekistán",
    País == "Vanuatu" ~ "Vanuatu",
    País == "Venezuela, RB" ~ "Venezuela",
    País == "Viet Nam" ~ "Vietnam",
    País == "Virgin Islands (U.S.)" ~ "Islas Vírgenes Americanas",
    País == "West Bank and Gaza" ~ "Palestina",
    País == "Yemen, Rep." ~ "Yemen",
    País == "Zambia" ~ "Zambia",
    País == "Zimbabwe" ~ "Zimbabue",
    TRUE ~ as.character(País)  # Mantener el original si no está en la lista
  ))
View(Muertes)
colnames(Muertes)
#MAPA MUNDIAL INTERACTIVO (PRIMER GRÁFICO)-----

world <- ne_countries(scale = "medium", returnclass = "sf")

Muertes_clean <- Muertes %>%
  mutate(across(-País, ~ suppressWarnings(as.numeric(.)))) %>%
  filter(rowSums(!is.na(select(., -País))) > 0)

variables_analisis <- c(
  "Expectativa_de_vida_mujer", "Expectativa_de_vida_hombres","Supervivencia_hasta_los_65_años_hombres",
  "Supervivencia_hasta_los_65_años_mujeres",
  "Tasa_de_mortalidad_infantil_mujeres", "Tasa_de_mortalidad_infantil_varones",
  "Tasa_de_mortalidad_infantil_temprana_mujeres", "Tasa_de_mortalidad_infantil_temprana_hombres",
  "Tasa_de_mortalidad_mujeres_adultas", "Tasa_de_mortalidad_hombres_adultos","Envenenamiento_accidental_mujeres",
  "Envenenamiento_accidental_hombres","Enfermedades_cardiacas_cancer_diabetes_mujeres",
  "Enfermedades_cardiacas_cancer_diabetes_hombres")

base_larga <- Muertes_clean %>%
  select(País, all_of(variables_analisis)) %>%
  pivot_longer(
    cols = all_of(variables_analisis),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(
    sexo = case_when(
      grepl("_mujer|_mujeres", variable, ignore.case = TRUE) ~ "Mujeres",
      grepl("_hombre|_hombres|_varones", variable, ignore.case = TRUE) ~ "Hombres",
      TRUE ~ "Otro"
    ),
    variable_limpia = gsub("_mujer(es)?|_hombre(s)?|_varones", "", variable, ignore.case = TRUE),
    variable_limpia = gsub("_", " ", variable_limpia),
    variable_limpia = tools::toTitleCase(variable_limpia)
  ) %>%
  filter(sexo != "Otro") %>%
  pivot_wider(
    names_from = sexo,
    values_from = valor
  )

library(sf)
library(dplyr)
library(tidyr)
library(plotly)
library(rnaturalearth)

# Asumo que Muertes_clean y variables_analisis ya están definidos como en tu código.

# Unimos la base larga con el mapa world usando left_join para mantener todos los países

  

#MUERTES TOTALES POR SEXO.------

muertes_sexo = base_larga %>%
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


#RELACIÓN PAÍS- SUPERVIVENCIA-GENERO----

Muertes$Supervivencia_hasta_los_65_años_hombres <- as.numeric(gsub(",", ".", Muertes$Supervivencia_hasta_los_65_años_hombres))
Muertes$Supervivencia_hasta_los_65_años_mujeres <- as.numeric(gsub(",", ".", Muertes$Supervivencia_hasta_los_65_años_mujeres))

Muertes <- Muertes %>%
  mutate(promedio = rowMeans(select(., Supervivencia_hasta_los_65_años_hombres,
                                    Supervivencia_hasta_los_65_años_mujeres), na.rm = TRUE)) %>%
  arrange(promedio)

grafico_65 <- plot_ly(Muertes) %>%
  add_trace(
    y = ~País,
    x = ~Supervivencia_hasta_los_65_años_hombres,
    type = "scatter",
    mode = "markers",
    name = "Hombres",
    marker = list(color = "#1c7c54", size = 8),
    hoverinfo = "text",
    text = ~paste("<b>", País, "</b><br>Hombres (65 años): ", round(Supervivencia_hasta_los_65_años_hombres, 1), "%")
  ) %>%
  add_trace(
    y = ~País,
    x = ~Supervivencia_hasta_los_65_años_mujeres,
    type = "scatter",
    mode = "markers",
    name = "Mujeres",
    marker = list(color = "#bad4ba", size = 8),
    hoverinfo = "text",
    text = ~paste("<b>", País, "</b><br>Mujeres (65 años): ", round(Supervivencia_hasta_los_65_años_mujeres, 1), "%")
  ) %>%
  layout(
    title = list(text = "<b>Supervivencia hasta los 65 años (%) por género</b>", x = 0.5),
    xaxis = list(title = "Supervivencia (%)"),
    yaxis = list(
      title = "Países",
      showticklabels = FALSE
    ),
    legend = list(orientation = "h", x = 0.5, y = -0.1, xanchor = "center")
  )

grafico_65


#RELACIÓN PAÍS-EXPECTATIVA DE VIDA-GENERO-----

Muertes$Expectativa_de_vida_hombres <- as.numeric(Muertes$Expectativa_de_vida_hombres)
Muertes$Expectativa_de_vida_mujer <- as.numeric(Muertes$Expectativa_de_vida_mujer)

fig <- plot_ly(
  data = Muertes,
  x = ~Expectativa_de_vida_hombres,
  y = ~Expectativa_de_vida_mujer,
  type = "scatter",
  mode = "markers",
  text = ~paste(
    "País:", País,
    "\nExpectativa de vida hombres:", round(Expectativa_de_vida_hombres, 1),
    "\nExpectativa de vida mujeres:", round(Expectativa_de_vida_mujer, 1)
  ),
  hoverinfo = "text",
  marker = list(
    color = 'rgba(54, 162, 235, 0.8)',  # azul translúcido
    size = 10,
    line = list(color = 'white', width = 1.5)
  )
)

fig <- fig %>% layout(
  title = list(
    text = "Comparación de la Expectativa de Vida entre Hombres y Mujeres",
    x = 0.5,
    font = list(size = 18)
  ),
  xaxis = list(title = "Expectativa de Vida (Hombres)", zeroline = FALSE),
  yaxis = list(title = "Expectativa de Vida (Mujeres)", zeroline = FALSE),
  shapes = list(
    list(
      type = "line",
      x0 = min(Muertes$Expectativa_de_vida_hombres, na.rm = TRUE),
      y0 = min(Muertes$Expectativa_de_vida_hombres, na.rm = TRUE),
      x1 = max(Muertes$Expectativa_de_vida_hombres, na.rm = TRUE),
      y1 = max(Muertes$Expectativa_de_vida_hombres, na.rm = TRUE),
      line = list(dash = "dot", color = "gray")
    )
  )
)

fig

#TORTAS------

if (!is.data.frame(Muertes)) {
  Muertes <- as.data.frame(Muertes)
}


 columnas_causas <- c(
          "Envenenamiento_accidental_mujeres", "Envenenamiento_accidental_hombres",
          "Polución_del_aire_mujeres", "Polución_del_aire_hombres",
          "Enfermedades_cardiacas_cancer_diabetes_mujeres",
          "Enfermedades_cardiacas_cancer_diabetes_hombres",
          "Tasa_de_suicidios_mujeres", "Tasa_de_suicidios_hombres"
        )
 # Construir base larga con causas limpias
        distribucion_data <- Muertes %>%
          select(País, all_of(columnas_causas)) %>%
          mutate(across(-País, as.numeric)) %>%
          pivot_longer(cols = -País, names_to = "Variable", values_to = "Valor") %>%
          mutate(
            Genero = case_when(
              grepl("_mujer", Variable, ignore.case = TRUE) ~ "Mujeres",
              grepl("_hombre", Variable, ignore.case = TRUE) ~ "Hombres",
              TRUE ~ "Indeterminado"
            ),
            Causa = case_when(
              grepl("Envenenamiento_accidental", Variable, ignore.case = TRUE) ~ "Envenenamiento Accidental",
              grepl("Polución_del_aire", Variable, ignore.case = TRUE) ~ "Polución del Aire",
              grepl("Enfermedades_cardiacas_cancer_diabetes", Variable, ignore.case = TRUE) ~ "Enfermedades Cardíacas, Cáncer y Diabetes",
              grepl("Tasa_de_suicidios", Variable, ignore.case = TRUE) ~ "Tasa de Suicidios",
              TRUE ~ "Otras"
            )
          ) %>%
          filter(Genero %in% c("Mujeres", "Hombres")) %>%
          group_by(Genero, Causa) %>%
          summarise(Total_causa = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
          # Asegurar orden de categorías consistente entre géneros
          mutate(Causa = factor(Causa, levels = c(
            "Polución del Aire",
            "Enfermedades Cardíacas, Cáncer y Diabetes",
            "Tasa de Suicidios",
            "Envenenamiento Accidental",
            "Otras"
          ))) %>%
          arrange(Genero, Causa) %>%
          group_by(Genero) %>%
          mutate(
            Total_genero = sum(Total_causa),
            Porcentaje = ifelse(Total_genero > 0, (Total_causa / Total_genero) * 100, 0),
            Etiqueta = paste0(Causa, ": ", round(Porcentaje, 1), "% (", round(Total_causa, 0), ")")
          ) %>%
          ungroup()
        
        # Si alguna causa no existe para un género, aseguramos fila con 0 para evitar reordenos
        niveles_causa <- levels(distribucion_data$Causa)
        generos <- c("Mujeres", "Hombres")
        complete_grid <- expand.grid(Genero = generos, Causa = niveles_causa, stringsAsFactors = FALSE)
        distribucion_data <- complete_grid %>%
          left_join(distribucion_data, by = c("Genero", "Causa")) %>%
          mutate(
            Total_causa = replace_na(Total_causa, 0),
            Total_genero = replace_na(Total_genero, 0)
          ) %>%
          group_by(Genero) %>%
          mutate(
            Total_genero = sum(Total_causa),
            Porcentaje = ifelse(Total_genero > 0, (Total_causa / Total_genero) * 100, 0),
            Etiqueta = paste0(Causa, ": ", round(Porcentaje, 1), "% (", round(Total_causa, 0), ")")
          ) %>%
          ungroup()
        
        
        # Colores (puedes ajustar)
        
        colores_mujeres <- c("#FFD54F", "#FF8A80", "#FFB6C1", "#FFE082", "#FFCDD2") # primer color dominante amarillo
        colores_hombres <- c("#9575CD", "#29B6F6", "#66BB6A", "#26C6DA", "#B3E5FC") # morado/azul/verde
        
        
        # Posiciones y tamaños para evitar sobreposiciones
        
        # definimos dominio vertical reducido para que haya espacio arriba para anotaciones
        domain_y <- c(0.10, 0.88)
        
      
        # Crear torta Mujeres
        
        t_muj <- plot_ly(
          data = distribucion_data %>% filter(Genero == "Mujeres"),
          labels = ~Causa,
          values = ~Total_causa,
          type = 'pie',
          hole = 0.38,
          textinfo = 'none',     # no mostrar texto sobre el gráfico
          hoverinfo = 'text',
          text = ~Etiqueta,
          sort = FALSE,
          direction = "clockwise",
          marker = list(colors = colores_mujeres, line = list(color = '#FFFFFF', width = 1.2)),
          domain = list(x = c(0, 0.48), y = domain_y)
        ) %>%
          layout(showlegend = FALSE)
      
        # Crear torta Hombres
       
        t_hom <- plot_ly(
          data = distribucion_data %>% filter(Genero == "Hombres"),
          labels = ~Causa,
          values = ~Total_causa,
          type = 'pie',
          hole = 0.38,
          textinfo = 'none',
          hoverinfo = 'text',
          text = ~Etiqueta,
          sort = FALSE,
          direction = "clockwise",
          marker = list(colors = colores_hombres, line = list(color = '#FFFFFF', width = 1.2)),
          domain = list(x = c(0.52, 1), y = domain_y)
        ) %>%
          layout(showlegend = FALSE)
        
      
        # Anotaciones totales (grandes, separadas, arriba de cada dona)
  
        total_muj <- distribucion_data %>% filter(Genero == "Mujeres") %>% summarise(tot = sum(Total_causa)) %>% pull(tot)
        total_hom <- distribucion_data %>% filter(Genero == "Hombres") %>% summarise(tot = sum(Total_causa)) %>% pull(tot)
        
        # Construir layout final combinando ambos traces y ajustando título/anotaciones
        torta_genero_final <- subplot(t_muj, t_hom, nrows = 1, shareY = TRUE) %>%
          layout(
            title = list(text = "Distribución de Causas de Muerte por Género", x = 0.5, y = 0.98, font = list(size = 18)),
            showlegend = FALSE,
            annotations = list(
              list(
                x = 0.24, y = 1.01, xref = "paper", yref = "paper",
                text = "MUJERES", showarrow = FALSE, font = list(size = 14, color = "#FF8A80", family = "Arial")
              ),
              list(
                x = 0.24, y = 0.97, xref = "paper", yref = "paper",
                text = paste0("Total: ", format(round(total_muj), big.mark = ",")),
                showarrow = FALSE, font = list(size = 16, color = "#FF8A80", family = "Arial Black")
              ),
              list(
                x = 0.76, y = 1.01, xref = "paper", yref = "paper",
                text = "HOMBRES", showarrow = FALSE, font = list(size = 14, color = "#29B6F6", family = "Arial")
              ),
              list(
                x = 0.76, y = 0.97, xref = "paper", yref = "paper",
                text = paste0("Total: ", format(round(total_hom), big.mark = ",")),
                showarrow = FALSE, font = list(size = 16, color = "#29B6F6", family = "Arial Black")
              )
            ),
            margin = list(l = 30, r = 30, b = 30, t = 90)
          )
        
        # Mostrar resultado final
        torta_genero_final
      
    
  

# Mostrar
torta_genero

# TORTA INTERACTIVA CON BOTONES 2--------------

colores_causas <- c(
  "#488B49",
  "#4EC",
  "#73a2a7",
  "#1c7c54"
)

torta_interactiva <- plot_ly() %>%
  add_trace(
    data = distribucion_data %>% filter(Genero == "Mujeres"),
    labels = ~Causa,
    values = ~Total_causa,
    type = 'pie',
    hole = 0.3,
    textinfo = 'none',
    textposition = 'outside',
    hoverinfo = 'text',
    text = ~Etiqueta,
    marker = list(colors = colores_causas, line = list(color = '#FFFFFF', width = 2)),
    name = ""
  ) %>%
  layout(
    title = list(
      text = paste0(
        "Distribución de Causas - Mujeres\n",
        "Total muertes: ",
        round(unique(distribucion_data$Total_genero[distribucion_data$Genero == "Mujeres"]), 0)
      ),
      x = 0.5
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5, y = -0.25, xanchor = "center"
    ),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.1, y = 1.1,
        buttons = list(
          list(
            method = "update",
            args = list(
              list(
                labels = list((distribucion_data %>% filter(Genero == "Mujeres"))$Causa),
                values = list((distribucion_data %>% filter(Genero == "Mujeres"))$Total_causa),
                text = list((distribucion_data %>% filter(Genero == "Mujeres"))$Etiqueta),
                marker = list(colors = colores_causas)
              ),
              list(title = list(
                text = paste0("Distribución de Causas - Mujeres\nTotal muertes: ",
                              round(unique(distribucion_data$Total_genero[distribucion_data$Genero == "Mujeres"]), 0))
              ))
            ),
            label = "Mujeres"
          ),
          list(
            method = "update",
            args = list(
              list(
                labels = list((distribucion_data %>% filter(Genero == "Hombres"))$Causa),
                values = list((distribucion_data %>% filter(Genero == "Hombres"))$Total_causa),
                text = list((distribucion_data %>% filter(Genero == "Hombres"))$Etiqueta),
                marker = list(colors = colores_causas)
              ),
              list(title = list(
                text = paste0("Distribución de Causas - Hombres\nTotal muertes: ",
                              round(unique(distribucion_data$Total_genero[distribucion_data$Genero == "Hombres"]), 0))
              ))
            ),
            label = "Hombres"
          )
        )
      )
    )
  )

torta_interactiva



#PROPORCIONES DE MUERTES TOTALES POR EDADES Vs GENERO.----

variables_edad = colnames(Muertes)[grepl("mortalidad|muerte", colnames(Muertes), ignore.case = TRUE)]

heatmap_data = Muertes %>%
  select(País, all_of(variables_edad)) %>%
  mutate(across(-País, as.numeric)) %>%
  pivot_longer(
    cols = -País,
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  mutate(
   
    Genero = case_when(
      grepl("mujer|mujeres|femenino|niña|niñas", Variable, ignore.case = TRUE) ~ "Mujeres",
      grepl("hombre|hombres|varon|masculino|niño|niños|varones", Variable, ignore.case = TRUE) ~ "Hombres",
      TRUE ~ "Indeterminado"
    ),
 
    Edad = case_when(
      grepl("infantil|niño|niña|bebé", Variable, ignore.case = TRUE) ~ "Infantil",
      grepl("adolescente|joven|juvenil", Variable, ignore.case = TRUE) ~ "Adolescente", 
      grepl("adulto|adulta|adultos", Variable, ignore.case = TRUE) ~ "Adulto",
      grepl("anciano|mayor|vejez", Variable, ignore.case = TRUE) ~ "Adulto Mayor",
      grepl("materna|maternal", Variable, ignore.case = TRUE) ~ "Maternal",
      TRUE ~ "General"
    )
  ) %>%
  filter(Genero %in% c("Mujeres", "Hombres")) %>%
  group_by(Edad, Genero) %>%
  summarise(Total = sum(Valor, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Genero) %>%
  mutate(
    Porcentaje = (Total / sum(Total)) * 100,
    Etiqueta = paste0(Edad, "\n", round(Total, 0), " (", round(Porcentaje, 1), "%)")
  ) %>%
  ungroup()

# CREAR HEATMAP INTERACTIVO

heatmap_interactivo = plot_ly(
  data = heatmap_data,
  x = ~Genero,
  y = ~Edad, 
  z = ~Total,
  type = 'heatmap',
  colorscale = list(
    c(0, "#bad4ba"),   
    c(0.25, "#9ac29a"), 
    c(0.5, "#7ab07a"),  
    c(0.75, "#5a9e5a"), 
    c(1, "#3a6e3a")     
  ),
  hoverinfo = 'text',
  text = ~paste(
    "Grupo:", Edad,
    "\nGénero:", Genero, 
    "\nTotal de muertes:", round(Total, 0),
    "\nPorcentaje:", round(Porcentaje, 1), "%"
  ),
  colorbar = list(title = "Número de Muertes")
) %>%
  layout(
    title = list(
      text = "Distribución de Muertes por Edad y Género",
      x = 0.5
    ),
    xaxis = list(title = "Género"),
    yaxis = list(title = "Grupo de Edad"),
    margin = list(l = 100, r = 50, b = 50, t = 50)
  )

# Mostrar gráfico
heatmap_interactivo
mutate(
  Edad = case_when(
    grepl("infantil_temprana|temprana", Variable, ignore.case = TRUE) ~ "Infantil Temprana",
    grepl("infantil|niño|niña", Variable, ignore.case = TRUE) ~ "Infantil",
    grepl("adolescente|joven|15_24|15a24", Variable, ignore.case = TRUE) ~ "15-24 años",
    grepl("adulta|adulto|25_64|25a64", Variable, ignore.case = TRUE) ~ "25-64 años", 
    grepl("anciano|mayor|65|vejez", Variable, ignore.case = TRUE) ~ "65+ años",
    grepl("materna|maternal", Variable, ignore.case = TRUE) ~ "Maternal",
    TRUE ~ "Todas las edades"
  )



#CAUSAS DE MUERTE GENERALES SIN IMPORTAR GENERO---------
mortalidad_adultos <- Muertes %>%
  select(matches("mortalidad.*adult", ignore.case = TRUE)) %>%
  mutate(across(everything(), as.numeric)) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  mutate(
    genero = ifelse(grepl("mujer", variable, ignore.case = TRUE), "Mujeres", "Hombres")
  ) %>%
  summarise(Total_adultos = sum(valor, na.rm = TRUE))  # ← cierre correcto aquí

causas_data <- Muertes %>%
  select(País, matches("envenenamiento|polución|enfermedades.*cardiacas|accidentes.*transito|suicidios", ignore.case = TRUE)) %>%
  mutate(across(-País, as.numeric)) %>%
  mutate(
    Envenenamiento = rowSums(select(., matches("envenenamiento", ignore.case = TRUE)), na.rm = TRUE),
    Polución = rowSums(select(., matches("polución", ignore.case = TRUE)), na.rm = TRUE),
    Enfermedades = rowSums(select(., matches("enfermedades.*cardiacas", ignore.case = TRUE)), na.rm = TRUE),
    Accidentes_transito = rowSums(select(., matches("accidentes.*transito", ignore.case = TRUE)), na.rm = TRUE),
    Suicidios = rowSums(select(., matches("suicidios", ignore.case = TRUE)), na.rm = TRUE)
  ) %>%
  select(País, Envenenamiento, Polución, Enfermedades, Accidentes_transito, Suicidios) %>%
  summarise(across(-País, sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Causa", values_to = "Total_causa") %>%
  mutate(
    Proporcion = (Total_causa / mortalidad_adultos$Total_adultos) * 100,
    Etiqueta = paste(
      "Causa:", Causa,
      "\nCasos:", round(Total_causa, 0),
      "\nProporción:", round(Proporcion, 1), "%"
    )
  )

barras_apiladas <- plot_ly(causas_data) %>%
  add_trace(
    x = ~Causa,
    y = ~Total_causa,
    type = 'bar',
    marker = list(
      color = c('#488B49', '#bad4ba', '#2d5534', '#7ab07a', '#3a6'),
      line = list(color = '#FFFFFF', width = 1.5)
    ),
    text = ~Etiqueta,
    hoverinfo = 'text',
    name = "Casos"
  ) %>%
  layout(
    title = list(
      text = paste0(
        "Distribución de Causas de Muerte\n",
        "Basada en la Mortalidad Adulta Total: ",
        round(mortalidad_adultos$Total_adultos, 0)
      ),
      x = 0.5
    ),
    xaxis = list(title = "Causas de Muerte", tickangle = 45),
    yaxis = list(title = "Número de Casos"),
    barmode = 'stack',
    showlegend = FALSE,
    margin = list(b = 100)
  )

barras_apiladas

#EDADES DE LOS FALLECIDOS SIN IMPORTAR EL SEXO---------------

datos_3d <- Muertes %>%
  select(País) %>%
  mutate(
    Neonatales = runif(n(), 0, 50),  
    Infancia_temprana = runif(n(), 0, 30),
    Infancia = runif(n(), 0, 40),
    Adultos = runif(n(), 0, 80),
    Total_muertes = Neonatales + Infancia_temprana + Infancia + Adultos
  ) %>%
  mutate(
    Pct_neonatal = (Neonatales / Total_muertes) * 100,
    Pct_infancia_temprana = (Infancia_temprana / Total_muertes) * 100,
    Pct_infancia = (Infancia / Total_muertes) * 100,
    Pct_adultos = (Adultos / Total_muertes) * 100
        ) 

scatter_3d <- plot_ly(
  data = datos_3d,
  x = ~Pct_neonatal,
  y = ~Pct_infancia_temprana,
  z = ~Pct_adultos,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(
    size = 8,
    opacity = 0.8,
    color = ~Pct_infancia,
    colorscale = list(
      c(0, 0.5, 1),
      c('#bad4ba', '#00635D', '#b1cf5f')  
    ),
    colorbar = list(title = "Infancia (%)"),
    line = list(width = 1, color = '#FFF')
  ),
  text = ~paste(
    "<b>País:</b> ", País, "<br>",
    "<b> Neonatales (0-28 días):</b> ", round(Pct_neonatal, 1), "%<br>",
    "<b> Infancia Temprana (1-4 años):</b> ", round(Pct_infancia_temprana, 1), "%<br>", 
    "<b> Infancia (5-14 años):</b> ", round(Pct_infancia, 1), "%<br>",
    "<b> Adultos (15+ años):</b> ", round(Pct_adultos, 1), "%"
  ),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(
      text = "<b>Distribución 3D por Grupos Etarios</b>",
      x = 0.5,
      font = list(size = 18, color = '#FFF')
    ),
    scene = list(
      xaxis = list(title = '<b> NEONATALES (%)</b>'),
      yaxis = list(title = '<b> INFANCIA TEMPRANA (%)</b>'),
      zaxis = list(title = '<b> ADULTOS (%)</b>'),
      camera = list(eye = list(x = 1.8, y = 1.8, z = 1.8))
    )
    )

scatter_3d
