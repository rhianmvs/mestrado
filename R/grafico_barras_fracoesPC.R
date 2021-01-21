##Grafico de barras fracoes de dPC dos fragmentos mais relevantes

library(dplyr)
library(tidyverse)
library(gapminder)
library(forcats)

read.csv("./fragmentos_relevantes.csv")

frag_relev<-read.csv("./fragmentos_relevantes.csv")


#############################################################

#Soma dos valores das frações de dPC por cenario

dPCintra<-frag_relev %>% group_by(cenário) %>%
     summarise(Total = sum(dPCintra), Count = n())

dPCflux<-frag_relev %>% group_by(cenário) %>%
  summarise(Total = sum(dPCflux), Count = n())

dPCconn<-frag_relev %>% group_by(cenário) %>%
  summarise(Total = sum(dPCconnect), Count = n())

#############################################################

#Juntando as tabelas

a<-rbind(dPCintra$Total, dPCflux$Total, dPCconn$Total)
row.names(a) <- c("dPCintra", "dPCflux", "dPCconnector")

cores_bar<-c("black", "darkgray", "white")

barplot(a,
        xlab = "Capacidade de dispersão",
        ylim = c(0,200),
        col = cores_bar,
        legend = T, args.legend = list(x = "topleft"))

 