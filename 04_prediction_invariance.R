source("/home/bdomingu/Dropbox/projects/ardp/src/00funs.R")
                                        #guess<-c(`mental-rotation`=.5,trog=0.25,vocab=0.25)
G<-0.25

xx<-list()
for (nm in c("trog","vocab")) {
    load(paste(nm,"__co_pilot.Rdata",sep=''))
    x1<-df
    load(paste(nm,"__de_pilot.Rdata",sep=''))
    x2<-df
    n1<-names(x1)
    n2<-names(x2)
    nn<-intersect(n1,n2)
    x1<-x1[,nn]
    x2<-x2[,nn]
    group<-c(rep(1,nrow(x1)),rep(2,nrow(x2)))
    table(group)
    df<-data.frame(rbind(x1,x2))
    ##
    out<-list()
    L<-split(df,group)
    out$z<-lapply(L,colMeans,na.rm=TRUE)
    ##
    tmp<-list()
    for (jj in 1:10) tmp[[jj]]<-groupdiff(df,group,g0=G)
    out$state<-colMeans(do.call("rbind",tmp))
    ##random grouping
    ## for (i in 1:3) {
    ##     group.ran<-sample(1:2,nrow(df),replace=TRUE)
    ##     out[[paste("randomG",i,sep='')]]<-groupdiff(df,group.ran,g0=G)
    ## }
    xx[[nm]]<-out
    print(table(group))
}


                                        #pdf("/home/bdomingu/Dropbox/Apps/Overleaf/ardp_manuscript/invemp.pdf",width=5,height=2.3)
postscript("/home/bdomingu/Dropbox/Apps/Overleaf/ardp_manuscript/imvemp.eps",width=5,height=2,horizontal = FALSE, onefile = FALSE, paper = "special")
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(xx$trog$z[[1]],xx$trog$z[[2]],xlim=0:1,ylim=0:1,pch=19,cex=.6,
     xlab="Colombia",ylab="Germany")
abline(0,1)
mtext(side=3,line=0,"TROG")
                                        #legend("bottomright",bty='n',title="IMV",legend=format(xx$trog$state,digits=2))
legend("bottomright",bty='n',cex=.7,
       legend=c(
           paste("Same",format(xx$trog$state[2],digits=3)),
           paste("Other",format(xx$trog$state[1],digits=3))
       )
       )
plot(xx$vocab$z[[1]],xx$vocab$z[[2]],xlim=0:1,ylim=0:1,pch=19,cex=.6,
     xlab="Colombia",ylab="Germany")
abline(0,1)
mtext(side=3,line=0,"Vocabulary")
legend("bottomright",bty='n',cex=.7,
       legend=c(
           paste("Same",format(xx$vocab$state[2],digits=3)),
           paste("Other",format(xx$vocab$state[1],digits=3))
       )
       )
dev.off()
