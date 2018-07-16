library(tidyverse)
library(cluster)

#cargando los datos

proteinIntake <- read.csv("data/protein.csv")

#Escalando la informacion quitando el pais ya que este es factor y no numerico

proteinIntakeScaled <- as.data.frame(scale(proteinIntake[,-1]))

#Agregando el Pais

proteinIntakeScaled$Country <- proteinIntake$Country

#Usando agrupacion jeraquica aglomerativa

hc <- hclust(dist(proteinIntakeScaled, method = "euclidean"), method = "ward.D2")

#realizando el plot

plot(hc, hang = -0.01, cex = 0.7)


#Vinculacion unica 

hc2 <- hclust(dist(proteinIntakeScaled), method = "single")
plot(hc2, hang = -0.01, cex = 0.7)


#Agrupacion jerarquica divisiva (DIVISIVE ANALYSIS CLUSTERING) diana()

dv <- diana(proteinIntakeScaled, metric = "euclidean")
par(mfrow = c(1,1))
plot(dv)



