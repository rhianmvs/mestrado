library(sp)
library(rgdal)
library(raster)
library(reshape)
library(gdistance)
library(rgeos)
library(maptools)
library(rJava)
library(RArcInfo)

#imputando meu mapa do mapbiomas
rio_de_janeiro<-raster("C:/Users/Rhian/Documents/Biblioteca/Shapes&raster/Rio_de_Janeiro/mapbiomas-riodejaneiro-riodejaneiro-2018.tif")

rio_de_janeiro
plot(rio_de_janeiro)

##COMO VER OS VALORES DA TA

#reclassificando a TA (vi no ArcMap)
#precisa criar uma matriz com os valores antigos, os novos e o nº de linhas e colunas
matriz_reclass_calphi<-matrix(data=c(0,3,5,13,15,19,21,23,24,25,29,30,32,33,NA,1,2,3,4,4,4,5,6,7,8,9,2,10),nrow=14,ncol=2) #valores para caluromys philander

reclassify(x = rio_de_janeiro, #objeto raster
           rcl = matriz_reclass_calphi, #matriz criada com os valores de origem e destino
           filename="rj_raster_calphi") #nome do arquivo de output

##Leitura do novo raster reclassificado (precisa puxar ele na pasta de origem onde ele é salvo)
rj_raster_calphi<-raster("C:/Users/Rhian/Documents/Mestrado/Mestrado_serio/rj_raster_calphi.grd")
plot(rj_raster_calphi)

#Salvando em formato GTiff
writeRaster(rj_raster_calphi,filename = "rj_calphi.tif",format="GTiff")
#Colocando o caminho da pasta que eu quero que ele fique
writeRaster(rj_raster_calphi,filename = "./Results/spatial_data/rj_calphi.tif",format="GTiff")



##Calculando a área dos nodes
rj_calphi2<-raster("./Results/spatial_data/rj_calphi.tif")

rj_calphi2[rj_calphi2[]>1]<-NA  #tudo o que não for fragmento florestal fica de fora. Tudo > 1 vira NA

plot(rj_calphi2)

nodes <- clump(rj_calphi2) #diferencia as manchas existentes na paisagem
plot(nodes)

n.nodes <- max(unique(nodes)) #só um comando que mostra que o clump deu certo e possuem 3 manchas na paisagem modelo; preciso para o looping!!!

tamanhos <- vector()

for(i in 1: n.nodes){

  pixels.do.node <- which(nodes[]==i)
  tam.node <- length(pixels.do.node)  #conta quantos pixels tem na mancha
  tam.node.m2 <- tam.node*30*30 # pixel de 30m x 30m
  tamanhos[i] <- tam.node.m2            #coloca os valores no objeto "tamanhos" que está vazio

}

tamanhos # em m2

# salvando os arquivos nodes
write.table(tamanhos,file="./Results/spatial_data/tamanho_nodes_m2",sep="\t")



## Distancias-euclidianas (so pra testar)
mF <- nodes

dF <- as.data.frame(mF, xy=TRUE, na.rm=TRUE)
dF <- dF[dF[,3] > 0,]
pd <- pointDistance(dF[,1:2], lonlat=FALSE)


pd <- as.matrix(as.dist(pd))
diag(pd) <- NA

a <- aggregate(pd, dF[,3,drop=FALSE], min, na.rm=TRUE)
a <- t(a[,-1])
a <- aggregate(a, dF[,3,drop=FALSE], min, na.rm=TRUE)[, -1]
diag(a) <- 0
a # distancias euclidianas minimas entre as manchas par-a-par


## Distancias-custo
mg <- paisagem

# and the transition matrix
tr1 <- transition(1/mg, transitionFunction=mean, directions=8)
tr1C <- geoCorrection(tr1, type="c")
plot(raster(tr1C))

cd <- as.matrix(costDistance(tr1C, as.matrix(dF[,1:2])))
b <- aggregate(cd, dF[,3,drop=FALSE], min, na.rm=TRUE)
b <- t(b[,-1])
b <- aggregate(b, dF[,3,drop=FALSE], min, na.rm=TRUE)[, -1]
diag(b) <- 0
b


### salvando os arquivos distances
XXXXX















###########################
#### Script for PC in R####
###########################

# o coneforWin64.exe tem que estar na mesma pasta onde os nodes e distances est?o
shell("coneforWin64.exe -nodeFile nodes_ -conFile dists_ -* -t dist notall -confProb 820.16 0.5 -PC onlyoverall")




