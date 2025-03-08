load("modelselection_df.Rdata")
library(imv)

library(mirt)
##1pl
m1<-mirt(df,1,'Rasch')
##2pl
ni<-ncol(df)
s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",
          sep="") 
model<-mirt.model(s)
m2<-mirt(df,model,itemtype="2PL",method="EM",technical=list(NCYCLES=10000))
##3pl
s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0),(1-",ni,", g, expbeta, 2, 17)",
         sep="") 
model<-mirt.model(s)
m3<-mirt(df,model,itemtype="3PL",method="EM",technical=list(NCYCLES=10000))

L<-list(m1,m2,m3)
tab<-list()
for (i in 1:length(L)) tab[[i]]<-c(L[[i]]@Fit$AIC,L[[i]]@Fit$BIC)
tab<-do.call("rbind",tab)
apply(tab,2,which.min)


##imv
set.seed(101010)
mean(imv.mirt(m1,m2,remove.nonvarying.items=FALSE,nfold=10))
mean(imv.mirt(m2,m3,remove.nonvarying.items=FALSE,nfold=10))

source("00funs.R")
set.seed(101010)
imv.mirt.local(m1,m2,remove.nonvarying.items=FALSE) #for getting the rmse of fitted/observed

##imv for 2pl with lower asymptote
s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",
          sep="") 
model<-mirt.model(s)
m2a<-mirt(df,model,itemtype="2PL",method="EM",technical=list(NCYCLES=10000),guess=.5)
mean(imv.mirt(m2,m2a,remove.nonvarying.items=FALSE,nfold=10))

