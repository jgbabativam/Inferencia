
rm(list = ls())

library(pacman)

p_load(tidyverse, janitor, #Limpieza de datos
       readxl,             #Leer archivos de excel 
       moderndive,         #seleccionar muestras
       infer,              #Métodos modernos de inferencia
       TeachingSampling)   #Métodos de muestreo

datos <- read_excel("data/Bootstrap.xlsx") |> 
         clean_names()


parametro <- mean(datos$cuanto_se_gasto_en_la_ultima_semana)
sd <- sd(datos$cuanto_se_gasto_en_la_ultima_semana)

variable <- datos |> 
            select(nombres, cuanto_se_gasto_en_la_ultima_semana) |> 
            mutate(individuo = 1:n())

espacio <- Support(N = nrow(datos), n = 3) |> 
           as_tibble() |> 
           mutate(muestra = 1:n()) |> 
           pivot_longer(cols = V1:V3,
                        names_to = "etiqueta",
                        values_to = "individuo")


