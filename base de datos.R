# Cargar librerías necesarias
library(readr)
library(tidyverse)
library(FactoMineR)
library(factoextra)

# Leer la base
Muertes_ans <- read_csv("Muertes ans.csv")
Muertes <- data.frame(Muertes_ans)

# Limpieza y selección de variables
Muertes <- Muertes[-(218:245), ]
Muertes <- na.omit(Muertes)

# Renombrar columnas
colnames(Muertes) <- c("País", "Expectativa_de_vida_mujer", "Expectativa_de_vida_hombres", "Accidentes_de_transito",
                       "Enfermedades_cardiacas_cancer_diabetes_mujeres", "Enfermedades_cardiacas_cancer_diabetes_hombres", 
                       "Polución_del_aire_mujeres", "Polución_del_aire_hombres", "Envenenamiento_accidental_mujeres", 
                       "Envenenamiento_accidental_hombres", "Tasa_de_mortalidad_mujeres_adultas","Tasa_de_mortalidad_hombres_adultos",
                       "Tasa_de_mortalidad_infantil_mujeres", "Tasa_de_mortalidad_infantil_varones", "Tasa_de_mortalidad_neonatal",
                       "Tasa_de_mortalidad_infantil_temprana_mujeres", "Tasa_de_mortalidad_infantil_temprana_hombres","Numero_de_muertes_infantiles",
                       "Numero_de_muertes_neonatales","Tasa_de_suicidios_mujeres","Tasa_de_suicidios_hombres","Supervivencia_hasta_los_65_años(%)_mujeres",
                       "Supervivencia_hasta_los_65_años(%)_hombres")

# Eliminar filas con valores NA en cualquier columna
Muertes <- Muertes %>%
  filter(complete.cases(.))

# Traducir nombres de países al español
Muertes <- Muertes %>%
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

# Preparar base para ACP
Muertes_df <- Muertes %>% 
  select(-País) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()
rownames(Muertes_df) <- Muertes$País

# Verificar la base final
View(Muertes)
print(paste("Número de países después de la limpieza:", nrow(Muertes)))
print("Estructura de la base:")
str(Muertes)
