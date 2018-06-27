library(tidyverse)
library(cluster)

##Cortando agrupaciones de bosques con funcion cutree()

#cargando los datos

proteinIntake <- read.csv("data/protein.csv")

#Escalando la informacion quitando el pais ya que este es factor y no numerico

proteinIntakeScaled <- as.data.frame(scale(proteinIntake[,-1]))

#Agregando el Pais

proteinIntakeScaled$Country <- proteinIntake$Country

#Usando agrupacion jeraquica aglomerativa

hc <- hclust(dist(proteinIntakeScaled, method = "euclidean"), method = "ward.D2")

#Categorizando la data en 4 grupos

fit <- cutree(hc, k= 4)

#Contando la cantidad de datos por grupo

table(fit)

#Visualizando la data

plot(hc)

#marcando los 4 grupos

rect.hclust(hc, k = 4 , border="red")

