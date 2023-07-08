
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


# Forma 1 para comparar M1 y M2: ANOVA

modelo1 <- aov(formula = tiempo ~ condicion, data = datos)
summary(modelo1)

#---- Intervalo de confianza para M1 - M2

#¿Las varianzas son iguales?

s2 <- tapply(datos$tiempo, datos$condicion, var)
xb <- tapply(datos$tiempo, datos$condicion, mean)
de <- tapply(datos$tiempo, datos$condicion, sd)
ni <- tapply(datos$tiempo, datos$condicion, length)

cociente <- s2[1]/s2[2]

li <- cociente * 1/qf(0.975, ni[1] - 1, ni[2] - 1)
ls <- cociente * qf(0.975, ni[2] - 1, ni[1] - 1)

li; ls
#Debido a que el valor 1 pertenece al IC, entonces se puede suponer que las 
#varianzas son iguales. 

var.test(formula = tiempo ~ condicion, data = datos,
         conf.level = 0.95)


# VARIANZAS IGUALES: INTERVALO PARA M1 - M2

t.test(formula = tiempo ~ condicion, data = datos,
       var.equal = TRUE,
       conf.level = 0.98)

#Intervalo 98% (M1 - M2): (1.64 ; 8.37) 

# VARIANZAS NO IGUALES: INTERVALO PARA M1 - M2

t.test(formula = tiempo ~ condicion, data = datos,
       var.equal = FALSE,
       conf.level = 0.98)

#Intervalo 98% (M1 - M2): (1.62 ; 8.39)

#-------------------------------------------
#-------------------------------------------
# INTERVALO DE CONFIANZA PARA UNA PROPORCIÓN

datosp <- datos |> 
          mutate(Exito = factor(ifelse(tiempo > 60, 1, 2),
                                levels = c(1, 2),
                                labels = c(">60", "<=60")
                                ))

Exitos <- table(datosp$Exito)[">60"]

prop.test(x = Exitos, n = nrow(datosp), conf.level = 0.95)

#Intervalo de confianza para P: (9.6% a 36.1%)
# ¿Cuánto es el margen de error?: 13.2%
(ME <- (0.36137437 - 0.09614522) / 2)


#-------------------------------------------
#-------------------------------------------
# INTERVALO DE CONFIANZA DIFERENCIA DE P1 - P2

table(datosp$Exito, datosp$condicion)

prop.test(x = c(6, 2),
          n = c(20, 20),
          conf.level = 0.95)

#IC del 95% para P1 - P2: (-0.09 ; 0.49)
# Como el cero pertenece al IC, entonces no existe
# evidencia estadística para pensar que la proporción
# de hojas con tiempos de secado superiores a 60 min. se diferencian por
# condición ambiental

### Intervalo para M.
t.test(datos$tiempo, conf.level = 0.95)


########################################
#### BOOTSTRAP

datosp |> 
  specify(response = tiempo)

#--- Intervalo para la Media

Tn <- datosp |> 
      specify(formula = tiempo ~ NULL) |> 
      generate(reps = 10000, type = "bootstrap") |> 
      calculate(stat = "mean")
  

#Visualizar la distribución de Tn = Xbarra

Tn |> 
  visualise()

# Ahora creamos el intervalo de confianza

# Usando el método del percentil
Tn |> 
  get_confidence_interval(level = 0.95, type = "percentile")

ic_perc <- Tn |> 
           get_ci(level = 0.95, type = "percentile")

xbarra <- mean(datosp$tiempo)


Tn |> 
  visualise() + 
  shade_ci(endpoints = ic_perc, color = "hotpink", fill = "khaki") +
  geom_vline(xintercept = xbarra, linetype = 2)

# Usando el método del error estándar

ic_ee <- Tn |> 
         get_ci(level = 0.95, type = "se", point_estimate = xbarra)


#--- Visualización y comparación de IC

Tn |> 
  visualise() +
  shade_ci(endpoints = ic_ee, color = "blue") +
  shade_ci(endpoints = ic_perc, color = "hotpink") +
  theme_bw()


###############################################
### BOOTSTRAP: INTERVALO DE CONFIANZA PARA LA PROPORCIÓN

set.seed(12345)
Pn <- datosp |> 
      specify(response = Exito, success = ">60") |> 
      generate(reps = 10000, type = "bootstrap") |> 
      calculate(stat = "prop")


Pn |> 
  visualise()

# Intervalo de confianza con el método del error estándar
n <- nrow(datosp)
Exitos <- table(datosp$Exito)[">60"]
  
p_est <- Exitos/n

ic_ee_p <- Pn |> 
           get_ci(level = 0.95, type = "se", point_estimate = p_est)


# Intervalo de confianza con el método del percentil

ic_perc_p <- Pn |> 
             get_ci(level = 0.95, type = "percentile")

#Visualización

Pn |> 
  visualise() +
  shade_ci(endpoints = ic_ee_p, color = "blue", fill = "khaki") +
  shade_ci(endpoints = ic_perc_p, color = "red", fill = "white") +
  labs(title = "Intervalo de confianza para la proporción") +
  theme_bw()




