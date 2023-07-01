rm(list = ls())

x <- c(9.8, 10.2, 10.4, 9.8, 10.0, 10.2, 9.6)

xb <- mean(x)
sd <- sd(x)
n <- length(x)

li <- xb + qt(0.025, df = n-1) * sd/sqrt(n)
ls <- xb + qt(0.975, df = n-1) * sd/sqrt(n)


t.test(x, conf.level = 0.95)

#===============================

datos <- data.frame(
  x = c(55.6, 56.1, 61.8, 55.9, 51.4, 59.9, 54.3, 62.8, 58.5, 55.8,
        58.3, 60.2, 54.2, 50.1, 57.1, 57.5, 63.6, 59.3, 60.9, 61.8)) |> 
  mutate(condicion = 1) |> 
  bind_rows(
      data.frame(
  x = c(55.1, 43.5, 51.2, 46.2, 56.7, 52.5, 53.5, 60.5, 52.1, 47.0,
           53.0, 53.8, 51.6, 53.6, 42.9, 52.0, 55.1, 57.1, 62.8, 54.8)) |> 
  mutate(condicion = 2)  
)
  
  
datos |> 
  group_by(condicion) |> 
  summarise(xb = mean(x),
            sd = sd(x),
            n = n())

3.66^2/5.01^2 * qf(0.025, 19, 19)
3.66^2/5.01^2 * qf(0.975, 19, 19)

#===== varianza combinada

S2p <- (19 * 3.66^2 + 19 * 5.01^2)/(20+20-2)

dif <- 57.8 - 52.8

dif + qt(0.01, df = 20 + 20 -2) * sqrt(S2p) * sqrt(1/20 + 1/20)
dif + qt(0.99, df = 20 + 20 -2) * sqrt(S2p) * sqrt(1/20 + 1/20)


t.test(x ~ condicion, data = datos, var.equal = TRUE,
       conf.level = 0.98)

# Proporciones

bina <- datos |> 
        mutate(y = ifelse(x > 60, 1, 0))
  
exitos <- sum(bina$y)
n <- nrow(bina)
  
p <- exitos/n  

p + qnorm(0.025) * sqrt(p * (1 - p)/n)
p + qnorm(0.975) * sqrt(p * (1 - p)/n)

prop.test(x = exitos, n = 40)
