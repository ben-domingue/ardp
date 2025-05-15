groupdiff<-function(df,group,
                    g0=0,
                    group.key=1) {
    library(imv)
    library(mirt)
    library(irw)
    ##fitting model in group 1
    ni<-ncol(df)
    s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 0.3)",
             sep="") 
    model<-mirt.model(s)
    df1<-df[group==group.key,]
    mod.ins<-mirt(df1,model,itemtype="2PL",method="EM",technical=list(NCYCLES=10000),guess=g0) 
    ##get test data in shape
    df2<-df[group!=group.key,]
    L<-list()
    for (i in 1:ncol(df2)) L[[i]]<-data.frame(id=rownames(df2),item=names(df2)[i],resp=df2[,i])
    df2l<-data.frame(do.call("rbind",L))
    df2l$train<-rbinom(nrow(df2l),1,.75)
    df2.tr<-makeresponse(df2l[df2l$train==1,],remove.nonvarying.items=FALSE)
    id<-df2.tr$id    
    ##predictions based on group 1
    df2.tr$id<-NULL
    th<-fscores(mod.ins,response.pattern=df2.tr)
    L<-list()
    for (i in 1:ncol(df2.tr)) {
        mm<-extract.item(mod.ins,names(df2.tr)[i])
        p<-probtrace(mm,th[,1])[,2]
        L[[i]]<-data.frame(id=id,item=names(df2.tr)[i],pr=p)
    }
    pr1<-data.frame(do.call("rbind",L))
    pred<-merge(df2l[df2l$train==0,],pr1) ##only thing you need
    ##fit model in group 2
    m0<-mirt(df2.tr,model,itemtype="2PL",method="EM",technical=list(NCYCLES=10000),guess=g0) 
    ##predictions in group 2
    th<-fscores(m0,response.pattern=df2.tr)
    L<-list()
    for (i in 1:ncol(df2.tr)) {
        mm<-extract.item(m0,names(df2.tr)[i])
        p<-probtrace(mm,th[,1])[,2]
        L[[i]]<-data.frame(id=id,item=names(df2.tr)[i],p2=p)
    }
    pr1<-data.frame(do.call("rbind",L))
    ##
    pred<-merge(pred,pr1) ##only thing you need
    pred<-pred[!is.na(pred$resp),]
    imv::imv.binary(pred$resp,pred$pr,pred$p2)
}

makeresponse<-function(x,
                       remove.nonvarying.items=TRUE,
                       remove.allNA.rows=TRUE
                       ) {
    ##make IR matrix
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
        z<-L[[i]]
        index<-match(z$id,id)
        resp<-rep(NA,length(id))
        resp[index]<-z$resp
        out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    if (remove.nonvarying.items) {
        nr<-apply(resp,2,function(x) length(table(x)))
        resp<-resp[,nr>1]
    }
    if (remove.allNA.rows) resp<-resp[rowSums(!is.na(resp))>1,]
    resp
}



imv.mirt.local<-function (mod1, mod2, nfold = 5, fscores.options = (list(method = "EAP")), 
                          model2=NULL,
                          model3=NULL,
                          G=NULL, #the guessing parameter which will be used
                          whole.matrix = TRUE, #should ensure every person/item is in every iteration of the training data
                          ...) 
{
    library(mirt)
    kk <- mod1@Data$K
    if (!all(kk == 2)) 
        stop("only works for dichotomous responses")
    x <- mod1@Data$data
    x2 <- mod2@Data$data
    if (!identical(x, x2)) 
        stop("Models run on different data")
    id <- 1:nrow(x)
    L <- list()
    for (i in 1:ncol(x)) L[[i]] <- data.frame(id = id, item = colnames(x)[i], 
                                              resp = x[, i])
    x <- data.frame(do.call("rbind", L))
    x <- x[!is.na(x$resp), ]
    np <- length(unique(x$id))
    ni <- length(unique(x$item))
    if (whole.matrix) {
        counter<-1
        test<-FALSE
        while (!test & counter<100) {
            x$group<-sample(1:nfold,nrow(x),replace=TRUE)
            nps<-nis<-numeric()
            for (ii in 1:nfold) {
                train <- makeresponse(x[x$group != ii, ], remove.nonvarying.items=TRUE)
                nps[ii]<-nrow(train)
                nis[ii]<-ncol(train)-1 #no items
            }
            test1<-all(nps==np)
            test2<-all(nis==ni)
            test<-test2 & test1
            counter<-counter+1
        }
    } else x$group<-sample(1:nfold,nrow(x),replace=TRUE)
    if (!test & counter >= 100) 
        stop("sample sizes don't support whole.matrix=TRUE")
    getcall <- function(mod) {
        call <- mod@Call
        call <- deparse(call)
        call <- gsub("data\\s*=\\s*[^,]+", "data = train", call)
        call <- parse(text = call)
        call
    }
    c1 <- getcall(mod1)
    c2 <- getcall(mod2)
    rmse1<-rmse2<-om <- numeric()
    for (i in 1:nfold) {
        train <- makeresponse(x[x$group != i, ], ...)
        id <- train$id
        train$id <- NULL
        mm1 <- eval(c1)
        mm2 <- eval(c2)
        th1 <- do.call("fscores", c(list(object = mm1), fscores.options))
        th2 <- do.call("fscores", c(list(object = mm2), fscores.options))
        ll <- list()
        items <- unique(x$item)
        for (j in 1:length(items)) {
            item <- items[j]
            it <- extract.item(mm1, item)
            pp1 <- probtrace(it, th1[, 1])
            it <- extract.item(mm2, item)
            pp2 <- probtrace(it, th2[, 1])
            ll[[j]] <- data.frame(id = id, item = item, pr1 = pp1[, 
                2], pr2 = pp2[, 2])
        }
        y <- data.frame(do.call("rbind", ll))
        test <- x[x$group == i, ]
        y <- merge(test, y, all.x = TRUE)
        om[i] <- imv.binary(y$resp, y$pr1, y$pr2)
        rms<-function(x) sqrt(mean((x)^2))
        rmse1[i]<-rms(y$resp-y$pr1)
        rmse2[i]<-rms(y$resp-y$pr2)
    }
    return(c(mean(om),mean(rmse1),mean(rmse2)))
}
