library(systemfit);
library(foreign);

temp <- read.spss("D://OSS//results//temp_pp.sav", to.data.frame=TRUE);

#eqCommit<-COMMITS~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL
#eqCommit<-COMMITS~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL+TRANS+AVPATH+DIAMETER+CLIQUE+BEM+BEV+CLM+CLV+PRM+PRV+AUGR+AUNN+EVCENTM+MUTUAL+PRESTM+GCENTM+STRESSM
eqCommit<-WEIV~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL+TRANS+AVPATH+DIAMETER+CLIQUE+BEM+BEV+CLM+CLV+PRM+PRV+AUGR+AUNN+EVCENTM+PRESTM+STRESSM+PRESTV+STRESSV+CONNECT+LUBNESS
system<-list(commit=eqCommit)
#inst<-~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL
#inst<-~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL+TRANS+AVPATH+DIAMETER+CLIQUE+OUTDM+OUTDV+PRM+PRV+BEM+BEV+CLM+CLV+AUGR+AUNN+AUNB+AUNC+EVCENTM+EVCENTV+WEIAVG+PRESTM+GCENTM+STRESSM
inst<-~DENSITY+EFF+NODENUM+EDGENUM+CENTRAL+TRANS+AVPATH+DIAMETER+CLIQUE+BEM+BEV+CLM+CLV+PRM+PRV+AUGR+AUNN+EVCENTM+PRESTM+STRESSM+PRESTV+STRESSV+CONNECT+LUBNESS

#OLSestimation:
fitols<-systemfit(system,data=temp)
round(coef(summary(fitols)),digits=4)
print(fitols)

#2SLSestimation:
fit2sls<-systemfit(system,method="2SLS",inst=inst,data=temp)
round(coef(summary(fit2sls)),digits=4)
print(fit2sls)

#3SLSestimation:
fit3sls<-systemfit(system,method="3SLS",inst=inst,data=temp)
round(coef(summary(fit3sls)),digits=4)
print(fit3sls)


