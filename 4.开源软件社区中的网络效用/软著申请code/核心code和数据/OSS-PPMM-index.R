suppressPackageStartupMessages(suppressWarnings({
  library(igraph)
  library(tidyverse)
  library(readxl)
  library(data.table)
  library(systemfit);
  library(rmarkdown)
  library(htmltools)
  library(sna)
  library(psych)
  library(ggcor)
  library(corrplot)
}))

setwd("C:/Users/sim509/Desktop/zuo1_code/OSS/Rtest")
getwd()

#------------------------#                                               
#-------1.收集数据-------#
#------------------------#
data_pp<-read.csv("result/data_pp.csv")
data_mm<-read.csv("result/data_mm.csv")

data_ppmm<-inner_join(data_pp,data_mm,by=c("pid","year","month")) %>% 
  subset(pid!=779 & pid!=648 & pid!=470 & pid!=292 & pid!=122) %>% 
  mutate(time=time.y) %>% 
  select(-time.x,-time.y,-begdate.x,-begdate.y,-filename.y) %>% {#<----
    names(.)<-name_update
    .
  } %>% 
  select(-commits_mm) %>% 
  select(-age_pp,-age_mm) %>% 
  data.table() %>% 
  .[,minyear:=min(year),by=pid] %>% 
  .[,month1:=(year-minyear)*12+month,by=pid] %>%
  .[,minmonth:=min(month1),by=pid] %>%
  .[,age:=month1-minmonth,by=pid] %>% 
  select(-minyear,-month1,-minmonth)
  
name_update<-names(data_ppmm) %>% 
  gsub(".x","_pp",.) %>% 
  gsub(".y","_mm",.) %>% 
  gsub("_mmpe_pp","type_pp",.) %>% 
  gsub("_mmpe_mm","type_mm",.) %>% 
  gsub("filename.x","filename",.) %>% 
  gsub("commits_pp","commits",.)



write.csv(data_ppmm,"result/data_ppmm_update.csv",row.names = F)

names(data_ppmm)
#描述性统计分析
des<-data_ppmm %>% 
  select(DEGREE_PP=mdegree_pp,BETWEEN_PP=mbetween_pp,CLOSEN_PP=mclose_pp,WEIGHTS_PP=mweight_pp,
         DIAMETER_PP=diameter_pp,SIZE_PP=networksize_pp,CONNECT_PP=connect_pp,
         DEGREE_MM=mdegree_mm,BETWEEN_MM=mbetween_mm,CLOSEN_MM=mclose_mm,WEIGHTS_MM=mweight_mm,
         DIAMETER_MM=diameter_mm,SIZE_MM=networksize_mm,CONNECT_MM=connect_mm,AGE=age
         ) %>% 
  describe() %>% 
  mutate(var=row.names(.)) %>% 
  select(var,3,4,8,9) %>% 
  data.frame()

des

write.csv(des,"result/data_ppmm_des.csv",row.names = F)



# robust test
data_ppmm<-read.csv("result/result_update/data_ppmm_update.csv")


id<-data_ppmm %>% 
  select(pid,year,month) %>% 
  mutate(date=paste(year,month,"01",sep="-")) %>% 
  mutate(date=as.Date(date)) %>% 
  subset(date>as.Date("2003-08-01")) %>% 
  select(pid) %>% 
  unique() %>% 
  as.vector()

data_ppmm_robust<-data_ppmm %>% 
  subset(pid %in% id$pid)

