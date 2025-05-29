library(splines)
library(imv)
library(merTools)
library(lme4)
library(tidyverse)

dat<-readRDS("task_data_nested.rds") ##https://github.com/levante-framework/levante-pilots/blob/main/01_fetched_data/task_data_nested.rds

##hf data
ii<-grep("hf",dat$item_task)
hf<-list()
for (i in ii) { 
    z<-dat[i,]
    df<-as.data.frame(z$data[[1]])
    df$rt<-log(df$rt_numeric/1000)
    ##
    df$shape<-df$item
    df$item<-paste(df$item,df$item_group,sep="__")
    df$resp<-ifelse(df$response==df$answer,1,0)
    df$item<-factor(df$item,levels=c("heart__hearts","flower__flowers","heart__heartsflowers","flower__heartsflowers"))
    ##
    hf[[paste("hf",dat$site[i])]]<-df
}
hf<-hf[names(hf) %in% c("hf co_pilot","hf de_pilot")]
lapply(hf,function(x) by(x$resp,x$item,mean))


##vocab
ii<-grep("vocab",dat$item_task)
voc<-list()
for (i in ii) {
    z<-dat[i,]
    df<-as.data.frame(z$data[[1]])
    df$rt<-log(df$rt_numeric/1000)
    df$resp<-ifelse(df$response==df$answer,1,0)
    ##
    #mm<-by(df$resp,df$item,mean,na.rm=TRUE)
    #items<-names(mm)[mm>.01 & mm<.99]
                                        #items<-sample(unique(df$item),25)
    #df<-df[df$item %in% items,]
    mm<-by(df$resp,df$item,mean,na.rm=TRUE)
    lev<-names(mm)[which.min(abs(mm-.5))]
    df$item<-factor(df$item)
    df$item<-relevel(df$item,lev)
    ##
    voc[[paste("voc",dat$site[i])]]<-df
}
voc<-voc[names(voc) %in% c("voc co_pilot","voc de_pilot")]

lapply(LL,nrow)
LL<-c(hf,voc)
f<-function(x) {
    x<-x[!is.na(x$resp),]
    x<-x[!is.na(x$rt),]
    x<-x[is.finite(x$rt),]
    x
}
LL<-lapply(LL,f)
lapply(LL,nrow)

##add analysis showing imv of prediciton of responses with/without item-level information and show it is much lower here than for other data
add.item<-function(df,nfold=5) {
    if (nfold==0) {
        test<-FALSE
        while (!test) {
            fold<-rbinom(nrow(df),1,.2)
            z<-df$user_id[fold==0]
            test1<-length(unique(z))==length(unique(df$user_id))
            z<-df$item[fold==0]
            test2<-length(unique(z))==length(unique(df$item))
            test<-test1 & test2
        }
        df$fold<-fold
        nfold<-1
    } else {
        df$fold<-sample(1:nfold,nrow(df),replace=TRUE)
    }
    ##
    om<-numeric()
    for (fold in 1:nfold) {
        df0<-df[df$fold!=fold,]
        m0<-glmer(resp~(1|user_id),df0,family='binomial')
        m1<-glmer(resp~(1|item)+(1|user_id),df0,family='binomial') 
        df0<-df[df$fold==fold,]
        df0<-df0[,c("user_id","item","resp","rt")]
        p0<-predictInterval(m0,df0,type='probability')
        p1<-predictInterval(m1,df0,type='probability')
        ##
        om[[fold]]<-imv::imv.binary(df0$resp,p0[,1],p1[,1])
    }
    mean(om)
}
library(parallel)
om0<-mclapply(LL,add.item,mc.cores=10,nfold=0)


##
add.rt<-function(df,nfold=5) {
    if (nfold==0) {
        test<-FALSE
        while (!test) {
            fold<-rbinom(nrow(df),1,.2)
            z<-df$user_id[fold==0]
            test1<-length(unique(z))==length(unique(df$user_id))
            z<-df$item[fold==0]
            test2<-length(unique(z))==length(unique(df$item))
            test<-test1 & test2
        }
        df$fold<-fold
        nfold<-1
    } else {
        df$fold<-sample(1:nfold,nrow(df),replace=TRUE)
    }
    ##
    om<-numeric()
    for (fold in 1:nfold) {
        df0<-df[df$fold!=fold,]
        ##
        m0<-glmer(resp~(1|item)+(1|user_id),df0,family='binomial')
        ##
        nspl<-3
        basis<-bs(df0$rt,nspl)
        for (i in 1:nspl) df0[[paste("spl",i,sep='')]]<-basis[,i]
        fm<-paste("resp~(1|item)+(1|user_id)+",paste("spl",1:nspl,sep="",collapse="+"),sep='')
        m1<-glmer(fm,df0,family='binomial') 
        ##
        df0<-df[df$fold==fold,]
        df0<-df0[,c("user_id","item","resp","rt")]
        p0<-predictInterval(m0,df0,type='probability')
        tmp<-predict(basis,df0$rt)
        for (i in 1:nspl) df0[[paste("spl",i,sep='')]]<-tmp[,i]
        p1<-predictInterval(m1,df0,type='probability')
        ##
        om[fold]<-imv::imv.binary(df0$resp,p0[,1],p1[,1])
    }
    mean(om)
}
library(parallel)
om1<-mclapply(LL,add.rt,mc.cores=10,nfold=0)

tab<-cbind(unlist(om0),unlist(om1))
xtable::xtable(tab,digits=4)

