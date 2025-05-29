##simstudy
source("/home/bdomingu/Dropbox/projects/ardp/src/00funs.R")
simfun<-function(th,b) {
    k<-outer(th,b,'-')
    p<-1/(1+exp(-1*k))
    resp<-p
    for (i in 1:ncol(p)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
    resp<-data.frame(resp)
    resp
}
sim<-function(gsd,N,thdiff) {
    group<-c(rep(1,N),rep(2,N))
    th1<-rnorm(N,0,1.3)
    th2<-rnorm(N,thdiff,1.3)
    b1<-rnorm(50,sd=.7)
    b2<-rnorm(50,mean=b1,sd=gsd)
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

N<-1000

gsd<-sort(runif(100,0,1))
library(parallel)
z0<-mclapply(gsd,sim,N=N,thdiff=0,mc.cores=10)
z0<-do.call("rbind",z0)
z1<-mclapply(gsd,sim,N=N,thdiff=.5,mc.cores=10)
z1<-do.call("rbind",z1)

pdf("/home/bdomingu/Dropbox/Apps/Overleaf/ardp_manuscript/invsim.pdf",width=5,height=3)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))

##scatterplot
group<-c(rep(1,N),rep(2,N))
th1<-rnorm(N,0,1.3)
th2<-rnorm(N,0,1.3)
b1<-rnorm(50,sd=.7)
b2<-rnorm(50,mean=b1,sd=0.4)
r1<-simfun(th1,b1)
r2<-simfun(th2,b2)
plot(colMeans(r1),colMeans(r2),,xlim=0:1,ylim=0:1,pch=19,
     xlab="Group 1",ylab="Group 2")
abline(0,1)


m<-loess(z0[,2]~gsd)
plot(gsd,m$fitted,type='l',col='red',xlab=expression(sigma),ylab="IMV",lwd=2)
m<-loess(z0[,1]~gsd)
lines(gsd,m$fitted,lwd=2)
m<-loess(z1[,2]~gsd)
lines(gsd,m$fitted,type='l',col='red',lty=2,lwd=2)
m<-loess(z1[,1]~gsd)
lines(gsd,m$fitted,lty=2,lwd=2)
legend("topleft",bty='n',fill=c("red","black"),c("Real","Random"))
legend("left",bty='n',title=expression(mu),c('0','0.5'),lwd=2,lty=c(1,2))
dev.off()
