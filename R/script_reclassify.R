#Library
library(raster)
library(rJava)
library(rgeos)
library(rgdal)

## Criar uma matriz onde estarão os valores do pixel atual e o que serão substituídos

matriz_reclass<-matrix(data=c(0,3,5,12,15,19,21,23,24,25,27,29,30,32,33,NA,1,5,8,2,3,4,12,7,9,NA,11,10,6,13),nrow=15,ncol=2)

##Leitura e transformação em objeto do raster

rj_raster1<-raster("./Data/spacial_data/rj_raster.tif")
plot(rj_raster1)

##Reclassificando o objeto raster

reclassify(x = rj_raster1, #objeto raster
           rcl =matriz_reclass, #matriz criada com os valores de origem e destino
           filename="rj_raster_calphi") #nome do arquivo de output


##Leitura do novo raster reclassificado
rj_raster_calphi<-raster("C:/Users/Rhian/Documents/Mestrado/Mestrado_serio/rj_raster_calphi.grd")
plot(rj_raster_calphi)

##Salvando em formato GTiff
writeRaster(rj_raster_calphi,filename = "rj_raster_calphi.tif",format="GTiff")
##Colocando o caminho da pasta que eu quero que ele fique
writeRaster(rj_raster_calphi,filename = "./Results/rj_raster_calphi.tif",format="GTiff")
