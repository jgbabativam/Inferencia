rm(list = ls())

library(pacman)

p_load(tidyverse, 
       moderndive,
       patchwork)

#---- Generación del universo

set.seed(12345)

poblacion <- tibble(
  x = runif(10000)
) |> 
  mutate(id = 1:n()) |> 
  relocate(id)

#########################################
#  SELECCIONAR 1000 MUESTRAS DE TAMAÑO 25
#########################################

#----- Seleccion de las 1000 muestras de tamaño 25
n25 <- poblacion |> 
       rep_sample_n(size = 25, reps = 1000)

#--- Estimación puntual

prop25 <- n25 |> 
          count(replicate, color) |> 
          mutate(prop = n/25) |> 
          filter(color == "roja")

#----- Histograma

g25 <- prop25 |> 
       ggplot(aes(x = prop)) +
       geom_histogram(binwidth = 0.05, color = "white")

#########################################
######### para n = 50
  
#----- Seleccion de las 1000 muestras de tamaño 50

n50 <- poblacion |> 
       rep_sample_n(size = 50, reps = 1000)

#--- Estimación puntual

prop50 <- n50 |> 
          count(replicate, color) |> 
          mutate(prop = n/50) |> 
          filter(color == "roja")

#----- Histograma

g50 <- prop50 |> 
       ggplot(aes(x = prop)) +
       geom_histogram(binwidth = 0.05, color = "white")
  
  
#########################################
######### para n = 100

#----- Seleccion de las 1000 muestras de tamaño 100

n100 <- poblacion |> 
        rep_sample_n(size = 100, reps = 1000)

#--- Estimación puntual

prop100 <- n100 |> 
           count(replicate, color) |> 
           mutate(prop = n/100) |> 
           filter(color == "roja")

#----- Histograma

g100 <- prop100 |> 
        ggplot(aes(x = prop)) +
        geom_histogram(binwidth = 0.05, color = "white")


#### Comparativo

g25 | g50 | g100


#----- Variaciòn Muestral

sd(prop25$prop)
sd(prop50$prop)
sd(prop100$prop)



################# EJERCICIO DIAPOSITIVA 50

pnorm(5.5, mean = 5, sd = sqrt(5/125))

################# EJERCICIO DIAPOSITIVA 51

1 - pnorm(30, mean = 28, sd = 5/sqrt(40))

pnorm(30, mean = 28, sd = 5/sqrt(40), lower.tail = F)

##############################################
##### EJERCICIO Diapostiva 58

x <- c(1.9, 2.4, 3.0, 3.5, 4.2)
n <- length(x)

X2 <- (n - 1) * var(x) / 1^2
  
 ggplot() +
  geom_function(fun = dchisq, args = list(df = 4)) +
  xlim(0, 20) +
  geom_vline(xintercept = qchisq(0.025, df = 4), linetype = 2) +
  geom_vline(xintercept = qchisq(0.975, df = 4), linetype = 2) +
  geom_vline(xintercept = X2, linetype = 2, color = "red")  
  









