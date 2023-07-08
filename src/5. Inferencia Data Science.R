
rm(list = ls())

# Paquetes necesarios
library(pacman)

p_load(tidyverse, infer)

# Cargue de los datos

datos <- data.frame(
  tiempo = c(55.6, 56.1, 61.8, 55.9, 51.4, 59.9, 54.3, 62.8, 58.5, 55.8,
             58.3, 60.2, 54.2, 50.1, 57.1, 57.5, 63.6, 59.3, 60.9, 61.8)) |> 
  mutate(condicion = 1) |> 
  bind_rows(
    data.frame(
  tiempo = c(55.1, 43.5, 51.2, 46.2, 56.7, 52.5, 53.5, 60.5, 52.1, 47.0,
             53.0, 53.8, 51.6, 53.6, 42.9, 52.0, 55.1, 57.1, 62.8, 54.8)) |> 
      mutate(condicion = 2)  
  )

#-------------------------------------
# INTERVALO DE CONFIANZA PARA M1 - M2

glimpse(datos)  #ver la estructura de los datos

datos$condicion <- factor(datos$condicion,
                          levels = c(1, 2),
                          labels = c("Cond1", "Cond2"))





