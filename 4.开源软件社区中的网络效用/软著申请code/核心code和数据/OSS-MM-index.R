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
  library(lubridate)
}))

setwd("C:/Users/sim509/Desktop/zuo1_code/OSS/Rtest")
getwd()

#------------------------#                                               
#-------1.收集数据-------#
#------------------------#

data_mm<-read_excel("data_mm.xlsx")%>% 
  subset(!(pid==820 & year==2002 & month==3)) %>% 
  subset(month!=0)%>% 
  mutate(filename=paste("Oss",type,"Net",year,"_",month,"_",pid,".net",sep=""))

#------------------------#                                               
#-------2.指标计算-------#
#------------------------#
#具体网络数据在finaldatfiles1010里
#list.files("finaldatfiles1010")

MM_update<-data.frame()
a<-Sys.time()

for(i in 1:nrow(data_mm)){
  
  filename<-paste("finaldatfiles1010/",data_mm$filename[i],sep="") 
  
  g<-igraph::read.graph(filename, "pajek")
  
  mdegree<-igraph::degree(g) %>% 
    mean()
  
  mbetween<- igraph::betweenness(g) %>% 
    mean()
  
  mclose<-igraph::closeness(g) %>% 
    mean()
  
  mweight<-E(g)$weight%>% 
    mean()
  
  diameter<-diameter(g,unconnected=TRUE)
  
  networksize<-vcount(g)
  
  density<-graph.density(g)
  
  gg <- read.paj(filename, simplify=FALSE, debug=FALSE, verbose=FALSE)  
  connect<-connectedness(gg)
  
  temp<-data.frame(filename,mdegree,mbetween,mclose,mweight,diameter,networksize,density,connect)
  
  MM_update<-rbind(MM_update,temp)
  
  b<-Sys.time()
  print(paste(i,", time:",b-a,sep = " "))
}

data_mm1<-data_mm %>%
  select(pid,type,year,month,begdate,commits) %>% 
  cbind(MM_update) %>% 
  select(pid,filename,type,year,month,begdate,commits,mdegree,mbetween,mclose,
         mweight,diameter,networksize,density,connect) %>% 
  mutate(mdegree2=mdegree^2) %>%
  mutate(mbetween2=mbetween^2) %>%
  mutate(mclose2=mclose^2) %>%
  mutate(networksize2=networksize^2) %>% 
  data.table() %>% 
  .[,minyear:=min(year),by=pid] %>% 
  .[,month1:=(year-minyear)*12+month,by=pid] %>%
  .[,minmonth:=min(month1),by=pid] %>%
  .[,age1:=month1-minmonth,by=pid] %>%
  mutate(time=(year-year(min(.$begdate)))*12+month-month(min(.$begdate))) %>% 
  select(pid,filename,type,year,month,begdate,time,commits,age=age1,mdegree,mdegree2,mbetween,mbetween2,
         mclose,mclose2,mweight,diameter,networksize,networksize2,density,connect)

write.csv(data_mm1,"result/data_mm.csv",row.names = F)

data_mm1<-read.csv("result/data_mm.csv")%>% 
  subset(pid!=779 & pid!=648 & pid!=470 & pid!=292 & pid!=122)

write.csv(data_mm1,"result/data_mm_update.csv",row.names = F)
  
#------------------------------#                                               
#-------3.描述性统计分析-------#
#------------------------------#
#（1）描述性统计分析
des<-describe(data_mm1) %>% 
  mutate(var=row.names(.)) %>% 
  select(var,3,4,5,8,9) %>% 
  .[-c(1:6),] %>% 
  data.frame()

des

write.csv(des,"result/data_mm_des.csv",row.names = F)

#（2）相关性分析
cor<-data_mm1 %>% 
  .[,-c(1:7)] %>% 
  select(commits,everything()) 

cor1<-cor %>% 
  cor()

write.csv(cor1,"result/data_mm_cor.csv",row.names = F)

#（3）相关性分析可视化
coefficients<-correlate(cor,method = 'spearman') %>% 
  .$r %>% 
  cor_tbl(type = "upper",show.diag = FALSE) %>% 
  mutate(order=1:91) %>% 
  arrange(desc(r)) %>% 
  mutate(self_color=colorRampPalette(c("red","gray"))(91)) %>% 
  arrange(order)

quickcor(cor,cor.test=T)+
  geom_square(data=get_data(type="lower",show.diag = F))+
  scale_fill_gradientn(colors=c("gray","red"))+
  geom_mark(data=get_data(type="upper", show.diag = FALSE),color=coefficients$self_color)+
  geom_diag_label(size=3,color="black")+
  remove_axis()+
  guides(color=FALSE) +
  theme(legend.title=element_blank()) 

ggsave("result/data_mm_correlations.png",width=20, height=20,units="cm",dpi = 600)

# #-------------------------#                                               
# #-------4负二项回归-------#
# -------------------------#
# # Negative binomial regression analysis
# # Below we use the glm.nb function from the MASS package to estimate a negative binomial regression.
# library(MASS)
# #library(jtools)
# 
# #-------------随机效应--------------#
# 
# #(1)旧模型
# fit<-glm.nb(commits~mdegree+mdegree2+mbetween+mbetween2+mclose+mclose2+
#               mweight+diameter+networksize+networksize2+age,data=data_mm1)
# 
# summary(fit)
# #j_summ(fit, exp = F)
# 
# #(2)新模型
# fit_new<-glm.nb(commits~mdegree+mdegree2+mbetween+mbetween2+mclose+mclose2+
#               mweight+diameter+networksize+networksize2+density+connect+age,data=data_mm1)
# 
# summary(fit_new)




