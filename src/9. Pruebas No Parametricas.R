
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

####################################
##### Comparación de las varianzas


#--- Realice un boxplot para revisar la dispersión.


datos %>% 
  ggplot(aes(x = aleacion, y = resistencia)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(stat = "mean", color = "red") +
  labs(x = "Aleación") +
  theme_bw()

### Ho: \siga_A^2 = \sigma_B^2

var.test(resistencia ~ aleacion,
         data = datos)

## Como p-value = 0.4709, no hay evidencia suficiente para rechazar Ho

####---------PRUEBA DE MEDIAS

t.test(resistencia ~ aleacion,
       alternative = "greater",
       var.equal = TRUE, 
       mu = 0,
       conf.level = 0.95,
       data = datos)

# Dado p-value = 0.0004671, hay suficiente evidencia para rechazar Ho

## Manual

xb <- tapply(datos$resistencia, datos$aleacion, mean)
s2 <- tapply(datos$resistencia, datos$aleacion, var)
n <- tapply(datos$resistencia, datos$aleacion, length)

sp2 <- ((n[1]-1)*s2[1] + (n[2]-1)*s2[2])/(n[1]+n[2]-2)


Tcal = (xb[1] - xb[2])/(sqrt(sp2) * sqrt(1/n[1] + 1/n[2]))

p.value = pt(Tcal, df = n[1] + n[2] - 2, lower.tail = FALSE)


####--------- Intervalo de confianza

#### Enfoque tradicional

ic <- t.test(resistencia ~ aleacion,
       alternative = "two.sided",
       var.equal = TRUE, 
       mu = 0,
       conf.level = 0.95,
       data = datos)

ggplot() +
  geom_function(fun = dt, args = list(df = 38)) +
  xlim(-7, 7) +
  geom_vline(xintercept = 0, linetype = 2, color = "blue") +
  geom_vline(xintercept = ic$conf.int[1], linetype = 2) +
  geom_vline(xintercept = ic$conf.int[2], linetype = 2) +
  theme_classic()


###### INTERVALO DE CONFIANZA POR BOOTSTRAP
library(infer)

distNull <- datos |> 
           specify(formula = resistencia ~ aleacion) |> 
           generate(reps = 10000, type = "bootstrap") |> 
           calculate(stat = "diff in means",
                     order = c("A", "B"))

estim_puntual <- datos |> 
                 observe(formula = resistencia ~ aleacion,
                 stat = "diff in means",
                 order = c("A", "B"))


# Método del percentil

ic_perc_diffm <- distNull |> 
                 get_ci(level = 0.95, type = "percentile")

distNull |> 
  visualise() +
  shade_ci(endpoints = ic_perc_diffm, color = "hotpink") +
  geom_vline(xintercept = 0, linetype = 2)

  
# Prueba de permutación

distPer <- datos |> 
           specify(formula = resistencia ~ aleacion) |> 
           hypothesise(null = "independence") |> 
           generate(reps = 10000, type = "permute") |> 
           calculate(stat = "diff in means",
                     order = c("A", "B"))

distPer |> 
  visualise(bins = 10) +
  shade_p_value(obs_stat = estim_puntual, direction = "greater") +
  labs(title = "Distribución de permutación para la comparación entre la aleación A y B")


distPer |> 
  get_p_value(obs_stat = estim_puntual, 
              direction = "greater")


#####---------------- PRUEBA U

wilcox.test(formula = resistencia ~ aleacion,
            data = datos,
            alternative = "greater")











