
rm(list = ls())

#--- Distribuciones de Probabilidad

# X ~ N(3, varianza = 5)
# Usando la regla P(X>1) = 1 - P(X <= 1)
1 - pnorm(q = 1, mean = 3, sd = sqrt(5))

# Haciendo directo P(X>1)

pnorm(q = 1, mean = 3, sd = sqrt(5), lower.tail = F)

# Haciendo con la estandarización

1 - pnorm(q = (1-3)/sqrt(5))


#----- P(X < q) = 0.2, cuando X ~ N(3, varianza = 5)

qnorm(p = 0.2, mean = 3, sd = sqrt(5))

##################################
### Distribución Binomial

#### P(X = 5), X~Binom(p=2/3, n = 5)

dbinom(x = 5, size = 5, prob = 2/3)

#### P(X >= 3), X~Binom(p=2/3, n = 5)
#### P(X > 2)

pbinom(q = 2, size = 5, prob = 2/3, lower.tail = F)

#### P(X >= 3) = 1 - P(X<3), X~Binom(p=2/3, n = 5)
#### P(X >= 3) = 1 - P(X<=2)

1 - pbinom(q = 2, size = 5, prob = 2/3)


