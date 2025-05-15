lf<-c("egma-math__co_pilot","egma-math__de_pilot",
      "mental-rotation__co_pilot","mental-rotation__de_pilot",
      "trog__co_pilot","trog__de_pilot",
      "vocab__co_pilot",
      "vocab__de_pilot"
      )

z<-list()
for (fn in lf) {
    load(paste(fn,".Rdata",sep=''))
    np<-nrow(df)
    ni<-ncol(df)
    x<-unlist(df)
    na<-sum(is.na(x))/length(x)
    x<-x[!is.na(x)]
    m<-mean(x)
    nc<-all(x %in% 0:1)
    den<-length(x)/(np*ni)
    z[[fn]]<-c(np=np,ni=ni,bernoulli=nc,den=den,m=m)
}
tab<-do.call("rbind",z)

xtable(tab)


