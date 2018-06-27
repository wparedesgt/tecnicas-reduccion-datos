library(tidyverse)
library(cluster)
library(devtools)
library(factoextra)


#Realizando análisis de cluster usando particionamiento de clustering

#Cargando la data

proteinIntake <- read.csv("data/protein.csv")

#nombres de fila, nombre de país y eliminar la variable País antes de normalizar

rownames(proteinIntake) <- proteinIntake$Country

#Eliminando las filas de los paises despues de nombrar cada linea

proteinIntake$Country <- NULL

#escalando los datos numericos

proteinIntakeScaled <- as.data.frame(scale(proteinIntake))


#usando kmeans

set.seed(22)
kmfit <- kmeans(proteinIntakeScaled, 4)

aggregate(proteinIntakeScaled, by=list(cluster=kmfit$cluster), mean)

fviz_cluster(kmfit, data = proteinIntakeScaled)

