library(igraph)
library(tidyverse)
library(readxl)
library(data.table)
library(systemfit);
library(sna)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#------------------------#
#---1.读取一个网络数据---#
#------------------------#
data_mm<-read_excel("data_mm.xlsx") %>% 
  subset(!(pid==820 & year==2002 & month==3)) %>% 
  select(pid,type,year,commits,mdegree=outdm,mbetween=bem,mclose=clm,
         mweight=weim,diameter,networksize=nodenum,density,connect,age=month) %>% 
  mutate(mdegree2=mdegree^2) %>%
  mutate(mbetween2=mbetween^2) %>%
  mutate(mclose2=mclose^2) %>%
  mutate(networksize2=networksize^2) %>% 
  mutate(age1=age) %>% 
  select(pid,type,year,month=age,commits,mdegree,mdegree2,mbetween,mbetween2,mclose,mclose2,
         mweight,diameter,networksize,networksize2,age=age1,density,connect) %>% 
  mutate(filename=paste("Oss",type,"Net",year,"_",month,"_",pid,".net",sep="")) %>% 
  select(pid,filename,everything()) %>% 
  data.table() %>%
  .[,age:=seq(.N),by=pid] 

write.csv(data_mm,"data_mm.csv",row.names = F)
  

data_pp<-read_excel("data_pp.xlsx")%>% 
  subset(!(pid==820 & year==2002 & month==3)) %>% 
  subset(month!=0)%>% 
  select(pid,type,year,commits,mdegree=outdm,mbetween=bem,mclose=clm,
         mweight=weim,diameter,networksize=nodenum,density,connect,age=month) %>% 
  mutate(mdegree2=mdegree^2) %>%
  mutate(mbetween2=mbetween^2) %>%
  mutate(mclose2=mclose^2) %>%
  mutate(networksize2=networksize^2) %>% 
  mutate(age1=age) %>% 
  select(pid,type,year,month=age,commits,mdegree,mdegree2,mbetween,mbetween2,mclose,mclose2,
         mweight,diameter,networksize,networksize2,age=age1,density,connect)%>% 
  mutate(filename=paste("Oss",type,"Net",year,"_",month,"_",pid,".net",sep="")) %>% 
  select(pid,filename,everything())

names(data_mm)


#删除id为820,2002年3月的数据

#------------------------#                                               
#-------2.指标计算-------#
#------------------------#
#具体网络数据在finaldatfiles1010里
list.files("finaldatfiles1010")

#(1)计算MM网络中的平均度
MMdegree<-as.numeric()

for(i in 1:nrow(data_mm)){
  
  filename<-paste("finaldatfiles1010/",data_mm$filename[i],sep="") 
  
  g<-read.graph(filename, "pajek")
  
  mdegree<-degree(g) %>% 
    mean()
  
  print(paste("the mean degree of",i,"is: ",mdegree,sep = " "))
  
  MMdegree[i]<-mdegree
}

sum(is.na(MMdegree))

data_mm<-data_mm %>% 
  mutate(mdegree=round(MMdegree,digits=3)) %>% 
  mutate(mdegree2=round(MMdegree^2,digits=3))


#(2)计算PP网络中的平均度
PP_update<-data.frame()

for(i in 1:nrow(data_pp)){
  
  filename<-paste("finaldatfiles1010/",data_pp$filename[i],sep="") 
  
  g<-read.graph(filename, "pajek")
  
  mdegree<-degree(g) %>% 
    mean()
  
  mbetween<- betweenness(g) %>% 
    mean()
  
  mclose<-closeness(g) %>% 
    mean()
    
  mweight<-E(g)$weight%>% 
    mean()
  
  diameter<-diameter(g,unconnected=TRUE)
    
  networksize<-vcount(g)
  
  density<-graph.density(g)
  
  gg <- read.paj(filename, simplify=FALSE, debug=FALSE, verbose=FALSE)  
  connect<-connectedness(gg)
  
  print(paste("the mean degree of",i,"is: ",mdegree,sep = " "))
  
  temp<-data.frame(filename,mdegree,mbetween,mclose,mweight,diameter,networksize,density,connect)
  
  PP_update<-rbind(PP_update,temp)
}

sum(is.na(PPdegree))

data_pp<-data_pp %>% 
  mutate(mdegree=round(PPdegree,digits=3)) %>% 
  mutate(mdegree2=round(PPdegree^2,digits=3))

unique(data_pp$pid) %>% 
  length()

unique(data_mm$pid) %>% 
  length()

#(3)全部数据
#行合并；按pid,year,month
data_ppmm<-inner_join(data_mm,data_pp,by=c("pid","year","month"))

unique(data_ppmm$pid) %>% 
  length()

write.csv(data_mm,"clean_mm.csv",row.names = F)
write.csv(data_pp,"clean_pp.csv",row.names = F)
write.csv(data_ppmm,"clean_ppmm.csv",row.names = F)

#(4)全部指标(老吴)
# 【y】
Commit


#2.1 【x1,x1^2】developer networks  centralization
gg <- read.paj("finaldatfiles1010/OssMMNet1999_0_292.net", simplify=FALSE, debug=FALSE, verbose=FALSE);
cen <- centralization(gg, g=1, degree);

dens <- graph.density(gig);
dens <- gden(gig);

#2.3 【x3,x3^2】networks betweenness
be <- betweenness(gig);
#ebe <- edge.betweenness(gig);
bem <- mean(be);

#2.5 【x5,x5^2】networks closeness
outd <- closeness(gig,mode="out");
outdm <- mean(outd);

cl <- closeness(gig);
clm <- mean(cl);

##2.7 【x7】 the average edge weights
wei <- E(gig)$weight;
weim <- mean(wei);
weiavg <- sum(wei) / vcount(gig);

#2.8 【x8】Diameterthe length of the longest geodesic calculated by using a breadth-first search like method
dia <- diameter(gig,unconnected=TRUE);
diau <- diameter(gig,unconnected=FALSE);

#2.9 【x9,x9^2】network sizes
augr <- graph.automorphisms(gig)$group_size;

#2.11 【x11】project age


