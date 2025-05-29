library(tidyverse)
##dat<-readRDS("task_data_prepped.rds") ##https://github.com/levante-framework/levante-pilots/blob/main/02_scoring_outputs/irt_data/task_data_prepped.rds
dat<-readRDS("independent_task_data_prepped.rds") ##https://github.com/levante-framework/levante-pilots/blob/3070382bd41326445deb936713f12aae5deb7729/02_scoring_outputs/irt_data/independent_task_data_prepped.rds#L2


for (iii in 1:nrow(dat)) {
    df<-dat[iii,]$data_prepped[[1]]
    ##collapsing multiple items
    nms<-strsplit(names(df),"_")
    f<-function(z) paste(z[1:(length(z)-1)],collapse='-')
    z<-sapply(nms,f)
    L<-list()
    for (nm in unique(z)) {
        ii<-z==nm
        tmp<-df[,which(ii),drop=FALSE]
        L[[nm]]<-rowMeans(tmp,na.rm=TRUE)
    }
    df<-data.frame(do.call("cbind",L))
    for (i in 1:ncol(df)) df[,i]<-ifelse(is.nan(df[,i]),NA,df[,i])
    ##data qc
    cm<-colMeans(df,na.rm=TRUE)
    if (nrow(df)<400) df<-df[,cm>.03 & cm<.97]
    rm<-rowMeans(!is.na(df))
    df<-df[rm>.25,]
    nn<-colSums(!is.na(df))
    df<-df[,nn>25]
    vals<-unique(unlist(df))
    vals<-vals[!is.na(vals)]
    if (length(vals)==2) {
        f<-function(x) {
            x<-x[!is.na(x)]
            t0<-sum(x==0)
            t1<-sum(x==1)
            min(c(t0,t1))
        }
        m<-apply(df,2,f)
        df<-df[,m>=10]
    }
    ##
    nm<-paste(dat$task_id[iii],dat$site[iii],sep="__")
    save(df,file=paste(nm,".Rdata",sep=''))
    print(nm)
    print(dim(df))
}


for (nm in c("egma-math","mental-rotation","trog","vocab")) {
    i<-grep(nm,dat$task_id)
    print(nm)
    z<-dat$guess[i]
    print(lapply(z,table))
}
