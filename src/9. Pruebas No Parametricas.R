
rm(list = ls())

library(tidyverse)


#################################################
### PRUEBAS PARA VERIFICAR LA NORMALIDAD

combust <- data.frame(x=c(11.5, 11.8, 12, 12.4, 12.5, 12.6, 
             12.8, 12.9, 13, 13.2)) %>% 
           mutate(Z = (x - 12)/1)

# Prueba de Kolmogorov-Smirnov

## Ho: X ~ Normal(12, 1).

ks.test(combust$Z, "pnorm")
#--- Como p-value es 0.12 entonces NO hay evidencia para rechazar Ho

## Ho: Los datos se ajustan a una distribución normal

shapiro.test(combust$x)
#--- Como p-value es 0.70 entonces NO hay evidencia para rechazar Ho
#--- EN conclusión se puede suponer que los datos se ajustan bien a una distribución normal


###################################################
####  PRUEBA DE INDEPENDENCIA

library(pacman)

p_load(haven)

datos <- read_sav("data/Social.sav")

table(haven::as_factor(datos$Ciudad))

table(as_factor(datos$P1), 
      as_factor(datos$P2))

###
### Ho: No hay asociación entre la opinión de la situación del país y su situación económica
### Ha: Existe asociación entre la opinión de la situación del país y su situación económica

chisq.test(as_factor(datos$P1), 
           as_factor(datos$P2))

#- Como el p-value es 2x10^(-16) entonces hay suficiente evidencia para rechazar Ho

glimpse(datos)

###
### Ho: La edad NO está asociada con la opinión de la situación económica del país
### Ha: La edad SI está asociada con la opinión de la situación económica del país

chisq.test(as_factor(datos$Edad),
           as_factor(datos$P1))

#Como p-value es 0.17, No hay evidencia suficiente para rechazar Ho

chisq.test(as_factor(datos$Estrato),
           as_factor(datos$P1))


p_load(FactoMineR, factoextra, foreign)

datos <- read.spss("data/Social.sav",
                   use.value.labels = T,
                   to.data.frame = T)
entra <- datos %>% 
         select(Estrato, Edad, P1, P2) %>% as_tibble()

res <- MCA(entra, graph = T)



###### EJERCICIO PÁG 380 - WALPOLE


datos <- data.frame(
  resistencia = c(88, 79, 84, 89, 81, 83, 82, 79, 82,
                  85, 88, 80, 85, 87, 80, 78, 87, 90,
                  83, 81,
                  75, 77, 86, 84, 80, 78, 83, 76, 81,
                  78, 78, 82, 80, 76, 85, 79, 80, 81, 
                  77, 78),
  aleacion = c(rep("A", 20), rep("B", 20))
)

#--- Gráficos para verificar normalidad.
glimpse(datos)

#---- directo par base R
qqnorm(datos$resistencia)

#---- Por ggplot

datos %>% 
  ggplot(aes(sample = resistencia)) +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line() +
  theme_classic()

  # Ho: La resistencia se distribuye normal
shapiro.test(datos$resistencia)

# p-value = 0.2318, No hay evidencia para rechazar la hipótesis de normalidad










