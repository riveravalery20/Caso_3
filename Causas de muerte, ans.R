library(readr)
Causas_muertes_ <- read_csv("C:/Users/etmel.DESKTOP-N79IL9B/Downloads/Causas muertes..csv")
View(Causas_muertes_)

library (tidyverse)
Causas_muertes_=Causas_muertes_ %>%select(-c(1,3,4,5,7,11,15,18))

Causas_muertes_ <- na.omit(Causas_muertes_)

#Limpieza de NA

Causas_muertes_ <- Causas_muertes_[-(218:239), ]


