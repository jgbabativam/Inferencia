rm(list = ls())

library(tidyverse)

set.seed(20230715)
x <- rnorm(10000, mean = 0.5, sd = 1) |> as_tibble()

hist(x$value)

res <- list()

for(i in 1:1000){
  set.seed(i)
  muestra <- x |> 
             sample_n(size = 100)
  
  a <- t.test(muestra$value, mu = 0, 
              alternative = "greater")
  
  res[[i]] <- a$p.value
  
}

pvalues <- unlist(res) |> as_tibble() |> 
           mutate(rechazo = ifelse(value < 0.05, 1, 0))

100*mean(pvalues$rechazo)

#alpha <- 100*mean(pvalues$rechazo)

alpha_50 <- 4.4; 
t02_50 <- 39.3;  t02_100 <- 60.3;
t04_50 <- 87.2;  t02_100 <- 98.6;
t04_50 <- 97.3;  t02_100 <- 100;


####### Teórico
options(scipen = 999)

conf <- 0.95 
k <- qnorm(conf)
sigma <- 1
n50 <- 50
c50 = k * sigma/sqrt(n50) + 0

n100 <- 100
c100 = k * sigma/sqrt(n100) + 0

potencia <- data.frame(
       mu = seq(0, 1, 0.1)
         ) |> 
  mutate(potencia_50 = 1 - pnorm(sqrt(n50)*(c50 - mu)/sigma),
         potencia_100 = 1 - pnorm(sqrt(n100)*(c100 - mu)/sigma))



potencia |> 
  pivot_longer(cols = -mu, names_to = "name", values_to = "potencia") |> 
  separate(name, c("tipo", "muestra"), sep = "_") |> 
  ggplot(aes(x = mu, y = potencia, group = muestra, color = muestra)) +
  geom_point(aes(shape = muestra)) + geom_line() +
  geom_hline(yintercept = 0.85, linetype = 2)


###### Qué pasa si los datos NO se distribuyen normal???

set.seed(20230715)
x <- rexp(10000, rate = 1) |> as_tibble()

hist(x$value)
mean(x$value)

res <- list()

for(i in 1:1000){
  set.seed(i)
  muestra <- x |> 
    sample_n(size = 50)
  
  a <- t.test(muestra$value, mu = 1, 
              alternative = "greater")
  
  res[[i]] <- a$p.value
  
}

pvalues <- unlist(res) |> as_tibble() |> 
  mutate(rechazo = ifelse(value < 0.05, 1, 0))

100*mean(pvalues$rechazo)

#alpha <- 100*mean(pvalues$rechazo)

alpha_50 <- 4.5;
alpha_100 <- 5.3;












