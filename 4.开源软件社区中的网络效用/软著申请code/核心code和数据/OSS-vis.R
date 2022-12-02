library(igraph)
library(tidyverse)
library(readxl)
library(data.table)
library(systemfit);
library(sna)
library(ggplot2)
library(ggthemes)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

normalize_01 <- function(x) (x - min(x)) / (max(x) - min(x)) + 0.25

windowsFonts(
  en = windowsFont("Times New Roman")
)

#根据OSS-ZUO.R文件中的size来找MM和PP最大值的两个网络进行可视化


#1.MM network

#OssMMNet2004_7_10909.net

g<-read.graph("finaldatfiles1010/OssMMNet2004_7_10909.net", "pajek")
summary(g)

iso <- V(g)[igraph::degree(g)==0]
g <- igraph::delete.vertices(g, iso)
g <- igraph::delete.vertices(g,c("110:FSPhoto","58:CMFNewsletter","201:PortalTransport",
                                 "218:SimpleBlog","64:CMFPhotoAlbum","63:CMFPhoto",
                                 "12:AmazonTool","23:CMFBibliographyAT"
                                 ))

V(g)$size <- normalize_01(igraph::degree(g)) * 10
#V(g)$size <-degree(g)

E(g)$width<-normalize_01(igraph::degree(g)) * 10


png("vis-mm.png",width=28, height=21,units = "cm",res=300)
plot.igraph(g, 
            layout= layout.fruchterman.reingold(g,niter = 250),
            asp = 0,
            vertex.color = "pink",#rgb(0.8,0.4,0.3,0.8),
            vertex.frame.color = "white",
            vertex.label.color = "black",
            vertex.label.family = "en",
            vertex.label.cex=0.8,
            edge.arrow.size=0.6,
            edge.curved = 0.5,
            edge.color="black"
)
dev.off()




#2.pp network visualization
g<-read.graph("finaldatfiles1010/OssPPNet2005_2_10909.net", "pajek")


iso <- V(g)[igraph::degree(g)==0]
g <- igraph::delete.vertices(g, iso)
g <- igraph::delete.vertices(g,c("127:okuji","126:ueck"))

V(g)$size <- normalize_01(igraph::degree(g)) * 10
#V(g)$size <-degree(g)

E(g)$width<-normalize_01(igraph::degree(g)) * 10


png("vis-pp.png",width=28, height=21,units = "cm",res=300)
plot.igraph(g, 
            layout= layout.auto(g),
            asp = 0,
            vertex.color = "pink",#rgb(0.8,0.4,0.3,0.8),
            vertex.frame.color = "white",
            vertex.label.color = "black",
            vertex.label.family = "en",
            vertex.label.cex=0.8,
            edge.arrow.size=0.6,
            edge.curved = 0.5,
            edge.color="black"
)
dev.off()























