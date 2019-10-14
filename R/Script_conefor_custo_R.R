library(sp)
library(rgdal)
library(raster)
library(reshape)
library(gdistance)
library(rgeos)
library(maptools)
library(rJava)
library(RArcInfo)

rio_de_janeiro<-raster("C:/Users/Rhian/Documents/Biblioteca/Shapes&raster/Rio_de_Janeiro/mapbiomas-riodejaneiro-riodejaneiro-2018.tif") #imputando meu mapa do mapbiomas

rio_de_janeiro


##preciso saber como eu vejo a tabela de atributos com os valores dos pixels

plot(rio_de_janeiro)

paisagem2 <- paisagem
paisagem2[paisagem2[]>1]<-NA  #tudo o que não for fragmento florestal fica de fora. Tudo > 1 vira NA

plot(paisagem2)

manchas <- clump(paisagem2) #diferencia as manchas existentes na paisagem
plot(manchas)

n.manchas <- max(unique(manchas)) #só um comando que mostra que o clump deu certo e possuem 3 manchas na paisagem modelo; preciso para o looping!!!

tamanhos <- vector()

for(i in 1: n.manchas){

  pixels.da.mancha <- which(manchas[]==i)
  tam.mancha <- length(pixels.da.mancha)  #conta quantos pixels tem na mancha
  tam.mancha.m2 <- tam.mancha*30*30 # pixel de 30m x 30m
  tamanhos[i] <- tam.mancha.m2            #coloca os valores no objeto "tamanhos" que está vazio

}

tamanhos # em m2


### salvando os arquivos nodes
XXXXX







## Distancias-euclidianas (so pra testar)
mF <- manchas

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




