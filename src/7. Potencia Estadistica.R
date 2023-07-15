rm(list = ls())

library(tidyverse)

set.seed(20230715)
x <- rnorm(10000, mean = 0, sd = 1) |> as_tibble()

hist(x$value)

res <- list()

for(i in 1:1000){
  set.seed(i)
  muestra <- x |> 
             sample_n(size = 50)
  
  a <- t.test(muestra$value, mu = 0, 
              alternative = "greater")
  
  res[[i]] <- a$p.value
  
}

