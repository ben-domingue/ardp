
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



imv.mirt.local<-function (mod1, mod2 = NULL, nfold = 5, fscores.options = (list(method = "EAP")), 
   whole.matrix = TRUE, ...) 
{
    library(mirt)
    kk <- mod1@Data$K
    if (!all(kk == 2)) 
        stop("only works for dichotomous responses")
    x <- mod1@Data$data
    if (is.null(mod2)) {
        return(imv0mirt(...))
    }
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
    test <- FALSE
    if (whole.matrix) {
        counter <- 1
        while (!test & counter < 100) {
            x$group <- sample(1:nfold, nrow(x), replace = TRUE)
            lll <- split(x, x$group)
            nps <- sapply(lll, function(x) length(unique(x$id)))
            test1 <- all(nps == np)
            nis <- sapply(lll, function(x) length(unique(x$item)))
            test2 <- all(nis == ni)
            test <- test2 & test1
        }
    } else x$group <- sample(1:nfold, nrow(x), replace = TRUE)
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
