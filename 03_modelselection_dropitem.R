

remove.item<-function(nrem,
                      mod1,model,
                      nfold=5) {
    f<-function(x) {
        id <- 1:nrow(x)
        L <- list()
        for (i in 1:ncol(x)) L[[i]] <- data.frame(id = id, item = colnames(x)[i], 
                                                  resp = x[, i])
        x <- data.frame(do.call("rbind", L))
        x <- x[!is.na(x$resp), ]
    }
    x1<-f(mod1@Data$data)
    om<-numeric()
    for (iii in 1:5) {
        ##2pl
        ## item.rem<-sample(1:ncol(df),nrem)
        ## df.new<-df[,-(item.rem)]
        ## ni<-ncol(df.new)
        ## s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",
        ##          sep="") 
        ## model.new<-mirt.model(s)
        ## mod.new<-mirt(df.new,model.new,itemtype="2PL",method="EM",technical=list(NCYCLES=10000))
        ##rasch
        item.rem<-sample(1:ncol(df),nrem)
        df.new<-df[,-(item.rem)]
        mod.new<-mirt(df.new,1,"Rasch")
        ##
        x2<-f(mod.new@Data$data)
        x1$group<-sample(1:nfold,nrow(x1),replace=TRUE)
        tmp<-x1[,c("id","item","group")]
        x2<-merge(x2,tmp)
        ##
        getcall <- function(mod) {
            call <- mod@Call
            call <- deparse(call)
            call <- gsub("data\\s*=\\s*[^,]+", "data = train", call)
            call <- parse(text = call)
            call
        }
        c1 <- getcall(mod1)
        c2 <- getcall(mod.new)
        data<-list(x1,x2)
        calls<-list(c1,c2)
        i<-1 #don't iterate over fold, iterate over removed items
        out<-list()
        for (ii in 1:2) {
            x<-data[[ii]]
            train <- makeresponse(x[x$group != i, ],
                                  remove.nonvarying.items=TRUE,
                                  remove.allNA.rows=TRUE
                                  )
            id <- train$id
            train$id <- NULL
            mm1 <- eval(calls[[ii]])
            th1 <- do.call("fscores", c(list(object = mm1), fscores.options=(list(method = "EAP"))))
            ##
            ll<-list()
            items <- unique(x$item)
            for (j in 1:length(items)) {
                item <- items[j]
                it <- extract.item(mm1, item)
                pp1 <- probtrace(it, th1[, 1])
                ll[[j]] <- data.frame(id = id, item = item, pr1 = pp1[,2])
            }
            y <- data.frame(do.call("rbind", ll))
            test <- x[x$group == i, ]
            out[[ii]] <- merge(test, y, all.x = TRUE)
        }
        ##
        tmp<-out[[1]][,c("id","item","pr1")]
        names(tmp)[3]<-'pr.full'
        tmp<-merge(out[[2]],tmp)
        om[[iii]] <- imv.binary(tmp$resp,tmp$pr1,tmp$pr.full)
    }
    mean(om)
}

library(imv)
library(mirt)
source("/home/bdomingu/Dropbox/projects/ardp/src/00funs.R")
load("mental-rotation.Rdata")

m2<-mirt(df,1,'Rasch')

#out<-list()
#for (nrem in c(1,5,10,15,25,35,45,55,65)) out[[as.character(nrem)]]<-remove.item(nrem,m2)
library(parallel)
nrem<-sort(sample(1:10,50,replace=TRUE))
out<-mclapply(nrem,remove.item,mod1=m2,model=model,mc.cores=10)

#pdf("/home/bdomingu/Dropbox/Apps/Overleaf/ardp_manuscript/imv_nrem.pdf",width=3,height=3)
z<-data.frame(nr=nrem,om=unlist(out))
plot(z,xlab='# items removed',ylab='IMV',pch=19,cex=1,col='gray')
abline(a=.0064,0)
m<-loess(om~nr,z)
lines(z$nr,fitted(m),col='red')
                                        #dev.off()
