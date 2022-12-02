library(systemfit);
library(foreign);

temp <- read.spss("D://OSS//results//vertex1010_mm.sav", to.data.frame=TRUE);

eqCommitLoad<-LOADVE~BETWEEN+CLOSEN+DEGREE+PAGERANK+STRESSCE+GRAPHCEN
system<-list(commitLoad=eqCommitLoad)
inst<-~BETWEEN+CLOSEN+DEGREE+PAGERANK+STRESSCE+GRAPHCEN+PREST+EVCENT

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


