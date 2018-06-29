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

pam.res <- pam(proteinIntakeScaled, 3)
dd <- dist(proteinIntakeScaled, method ="euclidean")

#Estadistica para el Cluster PAM

pam_stats <- cluster.stats(dd, pam.res$cluster)

#Suma de cuadrados

pam_stats$within.cluster.ss

#(pam) ancho promedio de silueta del clúster

pam_stats$clus.avg.silwidths

#Index del cluster

pam_stats$dunn

#Comparando el resultado de las dos soluciones

res.stat <- cluster.stats(dd, km.res$cluster, pam.res$cluster)

res.stat$corrected.rand
res.stat$vi


#Clustering avanzado DBSCAN

data("multishapes", package = "factoextra")
dataPoints <- multishapes[, 1:2]
plot(dataPoints)

par(mfrow = c(1,1))
#Calculando el resultado de dbscan, checando el resultado y plotendolo

dsFit <- dbscan(dataPoints, eps = 0.15, MinPts = 5)
print(dsFit)


fviz_cluster(dsFit, dataPoints, geom = "point")



#Cluster algoritmo EM

