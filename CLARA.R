#Clustering Large Aplication CLARA
library(tidyverse)
library(cluster)
library(factoextra)


#Cargando la data

proteinIntake <- read.csv("data/protein.csv")

#nombres de fila, nombre de país y eliminar la variable País antes de normalizar

rownames(proteinIntake) <- proteinIntake$Country

#Eliminando las filas de los paises despues de nombrar cada linea

proteinIntake$Country <- NULL

#escalando los datos numericos

proteinIntakeScaled <- as.data.frame(scale(proteinIntake))

#Calculando clara con k = 4 y tamaño de muestra (muestras = 5) en el marco de datos # escalado y revisando los medoides:

clarafit <- clara(proteinIntakeScaled, 4, samples = 5)

clarafit$medoids

#Fabricando el PLOT

fviz_cluster(clarafit)

