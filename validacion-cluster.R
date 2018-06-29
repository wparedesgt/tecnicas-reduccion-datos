##Realización de validación de clúster

library(factoextra)
library(cluster)
library(fpc)
library(NbClust)

#cargando la data y creando un data frame escalado

proteinIntake <- read.csv("data/protein.csv")
rownames(proteinIntake)=proteinIntake$Country
proteinIntake$Country=NULL
proteinIntakeScaled = as.data.frame(scale(proteinIntake))

#calculando y visualizando el numero optimo de cluster

nb <- NbClust(proteinIntakeScaled, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "ward.D2", index ="all")

fviz_nbclust(nb) + theme_minimal()

#calculando el ancho de la silueta para kmeans clustering y resumiendo los detalles

km.res = kmeans(proteinIntakeScaled, 3)
sil.km <- silhouette(km.res$cluster, dist(proteinIntakeScaled))

#Sumando el analisis de silhouette

si.sum <- summary(sil.km )

#Promedio de la silueta por cada cluster

si.sum$clus.avg.widths

#Promedio total (promedio de todos los anchos de la silueta de los cluster)

si.sum$avg.width

#Tamaño de cada uno de los clusters

si.sum$clus.sizes

#Visualizando 

fviz_silhouette(sil.km)

#A continuación, calcularemos la suma de cuadrados dentro del clúster, índice de dunns para pam



