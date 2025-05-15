source("/home/bdomingu/Dropbox/projects/ardp/src/00funs.R")

irw::irw_fetch("gilbert_meta_1")
# load("~/Dropbox/projects/irw/data/pub/gilbert_meta_1.Rdata")
resp<-irw::irw_long2resp(df)
#df$treat<-df$std_baseline>median(df$std_baseline,na.rm=TRUE)
df<-df[,c("id","treat")]
df<-merge(resp,df)
df$id<-NULL
group<-group<-df$treat
df$treat<-NULL

groupdiff(df,group,g0=0,group.key=0)
