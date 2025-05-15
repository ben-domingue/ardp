guess<-c(`mental-rotation`=.5,trog=0.25,vocab=0.25)

source("/home/bdomingu/Dropbox/projects/ardp/src/00funs.R")
nf<-10
estfun<-function(fn,nf=10,guess=guess) {
    G<-guess[grep(strsplit(fn,"__")[[1]][1],names(guess))]
    print(fn)
    library(imv)
    library(mirt)
    load(paste(fn,".Rdata",sep=''))
    ##1pl
    m1<-mirt(df,1,'Rasch')
    m1a<-mirt(df,1,'Rasch',guess=G)
    ##2pl
    ni<-ncol(df)
    s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",
             sep="") 
    model2<-mirt.model(s)
    m2<-mirt(df,model2,itemtype="2PL",method="EM",technical=list(NCYCLES=10000))
    m2a<-mirt(df,model2,itemtype="2PL",method="EM",technical=list(NCYCLES=10000),guess=G)
    ##3pl
    s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0),(1-",ni,", g, expbeta, 2, 17)",
             sep="") 
    model3<-mirt.model(s)
    m3<-mirt(df,model3,itemtype="3PL",method="EM",technical=list(NCYCLES=10000))
    ##IC
    L<-list(m1=m1,m1a=m1a,m2=m2,m2a=m2a,m3=m3)
    tab<-list()
    for (i in 1:length(L)) tab[[i]]<-c(L[[i]]@Fit$AIC,L[[i]]@Fit$BIC)
    tab.ic<-do.call("rbind",tab)
    rownames(tab.ic)<-names(L)
    apply(tab.ic,2,which.min)
    ##imv
    om12<-imv.mirt.local(m1,m2,remove.nonvarying.items=TRUE,nfold=nf,model2=model2) 
    om23<-imv.mirt.local(m2,m3,remove.nonvarying.items=TRUE,nfold=nf,model2=model2,model3=model3) 
    om1g<-imv.mirt.local(m1a,m2,remove.nonvarying.items=TRUE,nfold=nf,model2=model2,G=G)
    om2g<-imv.mirt.local(m2,m2a,remove.nonvarying.items=TRUE,nfold=nf,model2=model2,G=G)
    tab.om<-rbind(om12,om23,om1g,om2g)
    ##
    list(tab.ic=tab.ic,tab.om=tab.om)
}


lf<-c(#"egma-math__co_pilot","egma-math__de_pilot",
      "mental-rotation__co_pilot","mental-rotation__de_pilot",
      "trog__co_pilot","trog__de_pilot",
      "vocab__co_pilot",
      "vocab__de_pilot"
      )


library(parallel)
out<-mclapply(lf,estfun,nf=nf,mc.cores=length(lf),guess=guess)
save(out,file="02.Rdata")

## out<-list()
## for (fn in lf) {
##     out[[fn]]<-estfun(fn,nf=nf)
## }


##ic table
ic<-lapply(out,function(x) x$tab.ic)
ic<-do.call("cbind",ic)
library(xtable)
xtable(ic)

apply(ic,2,which.min)

##om table
om<-lapply(out,function(x) x$tab.om[,1])
om<-do.call("cbind",om)
library(xtable)
xtable(om,digits=4)

##the RMSE values
om<-lapply(out,function(x) x$tab.om[,-1])
om<-do.call("cbind",om)







## load("trog__co_pilot.Rdata")
## m<-mirt::mirt(df,1,'Rasch')
## itemfit(m, empirical.plot = 3)
