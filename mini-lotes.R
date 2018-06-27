library(tidyverse)
library(cluster)
library(devtools)
library(factoextra)
library(OpenImageR)
library(ClusterR)


#Cargando la imagen

img <- readImage("imagenes/bird.jpg")

#Cambiando el tamaño de la imagen

img_resize <- resizeImage(img, 350, 350, method = "bilinear")

#mostrando la imagen

imageShow(img_resize)

#vectorizandola despues de muestra

img_vector <- apply(img_resize, 3, as_vector)

#agrupando en mini lotes y predecir imagen vectorizada funcion MiniBatchKmeans

km_mb <- MiniBatchKmeans(img_vector, clusters = 5, batch_size = 50, num_init = 5, max_iters = 100, init_fraction = 0.2, initializer = "kmeans++", early_stop_iter = 10, verbose = FALSE)

#Creando la prediccion funcion predict_MBatchKMeans()

pr_mb <- predict_MBatchKMeans(img_vector, km_mb$centroids)
getcent_mb <- km_mb$centroids
new_img_mb <- getcent_mb[pr_mb, ]

#Creando la nueva imagen acorde a los vectores

dim(new_img_mb) <- c(nrow(img_resize), ncol(img_resize), 3)

imageShow(new_img_mb)
