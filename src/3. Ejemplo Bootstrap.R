
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
                        values_to = "individuo") |> 
           left_join(variable, by = "individuo")


estimacion <- espacio |> 
              group_by(muestra) |> 
              summarise(media = mean(cuanto_se_gasto_en_la_ultima_semana),
                        sd = sd(cuanto_se_gasto_en_la_ultima_semana)) |> 
              mutate(li = media + qnorm(0.025)*sd,
                     ls = media + qnorm(0.975)*sd) |> 
              mutate(Indicador = ifelse(li < parametro & ls > parametro,
                                        1, 0))

ET <- mean(estimacion$media)  

confianza <- mean(estimacion$Indicador)

############## Bootstrap


muestra <- datos |> 
           rep_sample_n(size = nrow(datos),
                        replace = TRUE,
                        reps = 10000) |> 
           group_by(replicate) |> 
           summarise(media = mean(cuanto_se_gasto_en_la_ultima_semana))


q1 <- quantile(muestra$media, 0.025)
q2 <- quantile(muestra$media, 0.975)


se <- sd(muestra$media)
media <- mean(muestra$media)
  
li <- media + qnorm(0.025)*se
ls <- media + qnorm(0.975)*se  



ggplot(data = muestra, aes(x = media)) +
  geom_histogram(color = "white",
                 fill = "lightblue",
                 binwidth = 20000) +
  geom_vline(xintercept = q1, linetype = 2, color = "blue") +
  geom_vline(xintercept = q2, linetype = 2, color = "blue") +
  geom_vline(xintercept = li, linetype = 2, color = "red") +
  geom_vline(xintercept = ls, linetype = 2, color = "red")

