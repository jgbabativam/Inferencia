
rm(list = ls())

library(pacman)

p_load(tidyverse, janitor, #Limpieza de datos
       knitr,              #Edición
       nycflights13,
       skimr,
       patchwork)       #Conjunto de datos

data("flights")

View(flights)
glimpse(flights)
#kable(flights[1:5,])

skim(flights)

head(flights)
names(flights)


g1 <- flights |> 
      filter(carrier == "AS") |> 
      ggplot(aes(x = dep_delay, y = arr_delay)) +
      geom_point(alpha = 0.2, color = "blue") +
      labs(x = "Salidas tarde", y = "Llegadas tarde") +
      theme_bw()


g2 <- weather |> 
      ggplot(aes(x = time_hour, y = temp)) +
      geom_line()



g3 <- weather |> 
      ggplot(aes(x = temp)) +
      geom_histogram(fill = "lightblue",
                     color = "white")
    

g4 <- weather |> 
      ggplot(aes(x = factor(month) , y = temp))+
      geom_boxplot(fill = "lightblue", 
                   color = "blue")




g1 | (g2/g3)

g2 / (g1 | g3)



