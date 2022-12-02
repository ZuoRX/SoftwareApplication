library(systemfit);
library(foreign);

temp <- read.spss("D://OSS//results//temp_y_mm_dyn.sav", to.data.frame=TRUE);

eqCommit<-WEIM~ENDSTART+MAXSTART+MEASTART+DENSITY+EFF+NODENUM+EDGENUM+CENTRAL+TRANS+AVPATH+DIAMETER+CLIQUE
system<-list(commitLoad=eqCommit)
inst<-~COMMITS~ENDSTART+MAXSTART+MEASTART

#OLSestimation:
fitols<-systemfit(system,data=temp)
round(coef(summary(fitols)),digits=4)
print(fitols)

#2SLSestimation:
#fit2sls<-systemfit(system,method="2SLS",inst=inst,data=temp)
#round(coef(summary(fit2sls)),digits=4)
#print(fit2sls)

#3SLSestimation:
#fit3sls<-systemfit(system,method="3SLS",inst=inst,data=temp)
#round(coef(summary(fit3sls)),digits=4)
#print(fit3sls)


