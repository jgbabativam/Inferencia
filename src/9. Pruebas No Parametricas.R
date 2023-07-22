
rm(list = ls())

library(tidyverse)

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



