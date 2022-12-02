suppressPackageStartupMessages(suppressWarnings({
  library(igraph)
  library(tidyverse)
  library(readxl)
  library(data.table)
  library(systemfit);
  library(rmarkdown)
  library(htmltools)
}))

setwd("C:/Users/sim509/Desktop/zuo1_code/OSS/Rtest")
getwd()

# 一、原始模型

## 1.MM网络结果

data_mm<-read.csv("clean_mm.csv") %>% 
  subset(pid!=779 & pid!=648 & pid!=470 & pid!=292 & pid!=122)
head(data_mm)

mm_commit<-commits~mdegree+mdegree2+mbetween+mbetween2+mclose+mclose2+
  mweight+diameter+networksize+networksize2+age#+density+connect

system<-list(commit=mm_commit)

fitols<-systemfit(system,data=data_mm)

summary(fitols)

## 2.PP网络结果

data_pp<-read.csv("clean_pp.csv")%>% 
  subset(pid!=779 & pid!=648 & pid!=470 & pid!=292 & pid!=122)
head(data_pp)

pp_commit<-commits~mdegree+mdegree2+mbetween+mbetween2+mclose+mclose2+
  mweight+diameter+networksize+networksize2+age#+density+connect

system<-list(commit=pp_commit)

fitols<-systemfit(system,data=data_pp)

summary(fitols)

## 3.PPMM网络结果

data_ppmm<-read.csv("clean_ppmm.csv") %>% 
  subset(pid!=779 & pid!=648 & pid!=470 & pid!=292 & pid!=122)
head(data_ppmm)

summary(data_ppmm)

#x-->MM network; y-->PP network
ppmm_commit<-commits.x~mdegree.x+mdegree2.x+mbetween.x+mbetween2.x+mclose.x+mclose2.x+
  mweight.x+diameter.x+networksize.x+networksize2.x+#density.x+connect.x+
  mdegree.y+mdegree2.y+mbetween.y+mbetween2.y+mclose.y+mclose2.y+
  mweight.y+diameter.y+networksize.y+networksize2.y+#density.y+connect.y+
  +age.x

system<-list(commit=ppmm_commit)

fitols<-systemfit(system,data=data_ppmm)

summary(fitols)




# 二、新模型

## 1.MM网络结果

data_mm<-read.csv("clean_mm.csv") %>% 
  subset(pid!=779 & pid!=648 & pid!=470 & pid!=292 & pid!=122)
head(data_mm)

mm_commit<-commits~mdegree+mdegree2+mbetween+mbetween2+mclose+mclose2+
  mweight+diameter+networksize+networksize2+density+connect+age

system<-list(commit=mm_commit)

fitols<-systemfit(system,data=data_mm)

summary(fitols)

## 2.PP网络结果

data_pp<-read.csv("clean_pp.csv")%>% 
  subset(pid!=779 & pid!=648 & pid!=470 & pid!=292 & pid!=122)
head(data_pp)

pp_commit<-commits~mdegree+mdegree2+mbetween+mbetween2+mclose+mclose2+
  mweight+diameter+networksize+networksize2+density+connect+age

system<-list(commit=pp_commit)

fitols<-systemfit(system,data=data_pp)

summary(fitols)

## 3.PPMM网络结果

data_ppmm<-read.csv("clean_ppmm.csv") %>% 
  subset(pid!=779 & pid!=648 & pid!=470 & pid!=292 & pid!=122)
head(data_ppmm)

#x-->MM network; y-->PP network
ppmm_commit<-commits.x~mdegree.x+mdegree2.x+mbetween.x+mbetween2.x+mclose.x+mclose2.x+
  mweight.x+diameter.x+networksize.x+networksize2.x+density.x+connect.x+
  mdegree.y+mdegree2.y+mbetween.y+mbetween2.y+mclose.y+mclose2.y+
  mweight.y+diameter.y+networksize.y+networksize2.y+density.y+connect.y+
  +age.x

system<-list(commit=ppmm_commit)

fitols<-systemfit(system,data=data_ppmm)

summary(fitols)







