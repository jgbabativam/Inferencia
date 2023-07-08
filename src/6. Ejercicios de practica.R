
rm(list = ls())

library(pacman)

p_load(tidyverse, infer)


datos <- data.frame(
    diametro = c(1.01, 0.97, 1.03, 1.04, 0.99, 0.98, 0.99, 1.01, 1.03)
)
