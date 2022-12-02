library(systemfit);
library(foreign);

temp <- read.spss("D://OSS//results//temp_mm.sav", to.data.frame=TRUE);

#temp1<-temp[7]+1
#temp[7]<-log(temp1)

#eqCommit<-COMMITS~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL
#eqCommit<-COMMITS~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL+TRANS+AVPATH+DIAMETER+CLIQUE+BEM+BEV+CLM+CLV+PRM+PRV+AUGR+AUNN+EVCENTM+MUTUAL+PRESTM+GCENTM+STRESSM
eqCommit<-COMMITS~DENSITY+NODENUM+AVPATH+CENTRAL+CLIQUE+BEM
system<-list(commit=eqCommit)
#inst<-~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL
#inst<-~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL+TRANS+AVPATH+DIAMETER+CLIQUE+OUTDM+OUTDV+PRM+PRV+BEM+BEV+CLM+CLV+AUGR+AUNN+AUNB+AUNC+EVCENTM+EVCENTV+WEIAVG+PRESTM+GCENTM+STRESSM
inst<-~DENSITY+NODENUM+CENTRAL+AVPATH+CLIQUE+BEM+CLM+BEV+CLV

#OLS estimation:
fitols<-systemfit(system,data=temp)
round(coef(summary(fitols)),digits=4)
print(fitols)

#2SLS estimation:
fit2sls<-systemfit(system,method="2SLS",inst=inst,data=temp)
round(coef(summary(fit2sls)),digits=4)
print(fit2sls)

#3SLS estimation:
fit3sls<-systemfit(system,method="3SLS",inst=inst,data=temp)
round(coef(summary(fit3sls)),digits=4)
print(fit3sls)


