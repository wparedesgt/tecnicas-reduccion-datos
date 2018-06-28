library(tidyverse)
library(cluster)
library(factoextra)

#Partitioning around medoids PAM

#Realizando análisis de cluster usando particionamiento de clustering

#Cargando la data

proteinIntake <- read.csv("data/protein.csv")

#nombres de fila, nombre de país y eliminar la variable País antes de normalizar

rownames(proteinIntake) <- proteinIntake$Country

#Eliminando las filas de los paises despues de nombrar cada linea

proteinIntake$Country <- NULL

#escalando los datos numericos

proteinIntakeScaled <- as.data.frame(scale(proteinIntake))

#calculando  pam con k = 4 en el marco de datos escalado y vericando medoides

pamFit <- pam(proteinIntakeScaled, 4)

pamFit$medoids

#Realizando el PLOT

fviz_cluster(pamFit)

