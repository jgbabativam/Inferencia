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


glimpse(datos)

datos$condicion <- factor(datos$condicion,
                          levels = c(1, 2),
                          labels = c("Cond1", "cond2"))

##---- Paso 1
#### Revisar si las varianzas se pueden considerar iguales
var.test(datos$tiempo ~ datos$condicion)
# El IC contiene el valor 1, de manera que se pueden suponer iguales

#---- Paso 2
#### Probar Hipótesis
#   Ho: M1 - M2 = 0
#   Ha: M1 - M2 != 0  -- Bilateral

t.test(datos$tiempo ~ datos$condicion,
       var.equal = TRUE,
       mu = 0,
       alternative = "two.sided")



xb <- tapply(datos$tiempo, datos$condicion, mean)
s2 <- tapply(datos$tiempo, datos$condicion, var)
n <- tapply(datos$tiempo, datos$condicion, length)

sp2 <- ((n[1]-1)*s2[1] + (n[2] - 1)*s2[2]) /(n[1] + n[2] - 2)

Test = (xb[1] - xb[2])/(sqrt(sp2)*sqrt(1/n[1] + 1/n[2]))

2*pt(Test, df = n[1] + n[2] - 2, lower.tail = F)


####  PRUEBA MULTINOMIAL
####   chi-cuadrado Pearson

# Ho: p =  c(9/16, 3/16, 3/16, 1/16)
# Ha p !=  c(9/16, 3/16, 3/16, 1/16)


observados <- c(315, 101, 108, 32)
n <- sum(observados)

res <- chisq.test(x = observados,
                  p = c(9/16, 3/16, 3/16, 1/16))
res

pvalue <- pchisq(q = 0.47, df = length(observados) - 1, 
                 lower.tail = FALSE)



###################################
####  EJERCICIO DE CARGOS: PAPER 1974

#### Hipótesis 
#### Existe discriminación por sexo 

### Ho: P1 - P2 = 0   (Independencia)
### Ha: P1 - P2 !=0   (Dependencia)

library(moderndive)
library(infer)

data("promotions")
View(promotions)

table(promotions$decision, promotions$gender)

promotions |> 
  ggplot(aes(x = gender, fill = decision)) +
  geom_bar(position = "fill") +
  labs(y = "Proporción")

# Estos resultados son evidencia suficiente para concluir
# que existe una discriminación de género?

# Prueba Multinomial
# Ho: p  = c(1/2, 1/2)
# Ha: p != c(1/2, 1/2)

# Estadístico Chi-cuadrado

observados <- c(21, 14)
res <- chisq.test(x = observados,
                  p = c(1/2, 1/2))

res

# Prueba de permutación

set.seed(20230715)

per1 <- promotions |> 
        mutate(aleatorio = runif(48)) |> 
        arrange(aleatorio) |> 
        select(gender) |> 
        rename(per1 = gender)


u1 <- bind_cols(promotions, per1)

u1 |> 
  ggplot(aes(x = per1, fill = decision)) +
  geom_bar(position = "fill") +
  labs(y = "Proporción")

###----- Prueba de permutación


distPer <- promotions |> 
           specify(formula = decision ~ gender, 
                   success = "promoted") |> 
           hypothesise(null = "independence") |> 
           generate(reps = 10000, type = "permute") |> 
           calculate(stat = "diff in props",
                     order = c("male", "female"))

observado <- promotions |> 
            observe(formula = decision~gender, 
                    success = "promoted",
                    stat = "diff in props",
                    order = c("male", "female"))



distPer |> 
  visualise(bins = 10) +
  shade_p_value(obs_stat = observado, direction = "greater") +
  labs(title = "Distribución de permutación")


distPer |> 
  get_p_value(obs_stat = observado, 
              direction = "greater")


##### Comparación con el IC

distNull <- promotions |> 
            specify(formula = decision ~ gender, 
                    success = "promoted") |> 
            generate(reps = 10000, type = "bootstrap") |> 
            calculate(stat = "diff in props",
                      order = c("male", "female"))


ic <- distNull |> 
  get_ci(level = 0.95, type = "percentile")

distNull |> 
  visualise(bins = 10) +
  shade_ci(endpoints = ic) +
  geom_vline(xintercept = 0, linetype = 2)











