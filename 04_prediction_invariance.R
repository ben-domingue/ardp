source("/home/bdomingu/Dropbox/projects/ardp/src/00funs.R")
load("vocab__co_pilot.Rdata")
x1<-df
load("vocab__de_pilot.Rdata")
x2<-df
n1<-names(x1)
n2<-names(x2)
nn<-intersect(n1,n2)
x1<-x1[,nn]
x2<-x2[,nn]
group<-c(rep(1,nrow(x1)),rep(2,nrow(x2)))
table(group)
df<-data.frame(rbind(x1,x2))

L<-split(df,group)
z<-lapply(L,colMeans,na.rm=TRUE)
plot(z[[1]],z[[2]],xlim=0:1,ylim=0:1); abline(0,1)

#guess<-c(`mental-rotation`=.5,trog=0.25,vocab=0.25)
G<-0.5

out<-list()
##random grouping
for (i in 1:10) {
    group.ran<-sample(1:2,nrow(df),replace=TRUE)
    out[[paste("randomG",i,sep='')]]<-groupdiff(df,group.ran,g0=G)
}


##should now be easy to plug in meaningful group data
out$state<-groupdiff(df,group,g0=G)


##simstudy
sim<-function(gsd,N,thdiff) {
    group<-c(rep(1,N),rep(2,N))
    th1<-rnorm(N,0,1.3)
    th2<-rnorm(N,thdiff,1.3)
    b1<-rnorm(50,sd=.7)
    b2<-rnorm(50,mean=b1,sd=gsd)
    simfun<-function(th,b) {
        k<-outer(th,b,'-')
        p<-1/(1+exp(-1*k))
        resp<-p
        for (i in 1:ncol(p)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
        resp<-data.frame(resp)
        resp
    }
    test<-FALSE
    while (!test) {
        r1<-simfun(th1,b1)
        r2<-simfun(th2,b2)
        df<-data.frame(rbind(r1,r2))
        nn<-apply(df,2,function(x) length(unique(x)))
        test<-all(nn==2)
    }
    G<-0
    group.ran<-sample(1:2,nrow(df),replace=TRUE)
    out<-list()
    ##random grouping
    om.ran<-groupdiff(df,group.ran,g0=G)
    om.true<-groupdiff(df,group,g0=G)
    c(om.ran,om.true)
}

## out<-list()
## for (N in c(100,500)) for (gsd in c(0,.5,1)) for (thdiff in c(0,.5)) {
##                           for (i in 1:5) {
##                               out[[paste(N,gsd,thdiff,i)]]<-c(gsd,N,thdiff,sim(gsd,N,thdiff))
##                           }
##                       }
## z<-do.call("rbind",out)

## L<-split(data.frame(z),paste(z[,1],z[,3],z[,2]))
## yl<-range(c(z[,4],z[,5]))
## plot(NULL,xlim=yl,ylim=c(1,length(L)),xlab='omega',ylab='',yaxt='n')
## abline(v=0)
## par(mgp=c(2,1,0),mar=c(3,10,1,1))
## for (i in 1:length(L)) {
##     x<-L[[i]]
##     points(x[,4],rep(i,nrow(x)))
##     points(x[,5],rep(i,nrow(x)),col='red')
##     mtext(side=2,las=2,names(L)[i],at=i,line=0)
## }

N<-150
gsd<-sort(runif(100,0,1))
library(parallel)
z0<-mclapply(gsd,sim,N=N,thdiff=0,mc.cores=10)
z0<-do.call("rbind",z0)
z1<-mclapply(gsd,sim,N=N,thdiff=.5,mc.cores=10)
z1<-do.call("rbind",z1)

m<-loess(z0[,2]~gsd)
plot(gsd,m$fitted,type='l',col='red',xlab=expression(sigma),ylab="IMV")
m<-loess(z0[,1]~gsd)
lines(gsd,m$fitted)
m<-loess(z1[,2]~gsd)
lines(gsd,m$fitted,type='l',col='red',lty=2)
m<-loess(z1[,1]~gsd)
lines(gsd,m$fitted,lty=2)
