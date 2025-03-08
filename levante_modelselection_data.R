library(tidyverse)
load("task_data_prepped.Rdata") ##https://github.com/levante-framework/levante-pilots/blob/main/02_score_data/fit_irt.qmd


## tab<-list()
## for (i in 1:nrow(task_data_prepped)) tab[[i]]<-dim(task_data_prepped[i,]$data_prepped[[1]])
## do.call("rbind",tab)

## ##  [7,]  296   63
## ##  [8,]  241   88
## ##  [9,]  361   99
## ## [12,]  283   54
## ## [13,]  222   81
## ## [14,]  209   75
## ## [18,]  219   52
## ## [21,]  238   85
## ## [23,]  247   60
## ## [24,]  239   60

## ii<-c(7:9,12:14,18,21,23,24)
## for (i in ii) {
##     df<-task_data_prepped[i,]$data_prepped[[1]]
##     na<-sum(is.na(df))/(nrow(df)*ncol(df))
##     m<-mean(colMeans(df,na.rm=TRUE))
##     print(task_data_prepped[i,]$task_id)
##     print(c(i,na,m))
## }

## ## [1] "mental-rotation"
## ## [1] 13.00  0.18  0.77
## ## [1] "hearts-and-flowers"
## ## [1] 23.000  0.034  0.784
## ## [1] "hearts-and-flowers"
## ## [1] 24.000  0.019  0.781

df<-task_data_prepped[13,]$data_prepped[[1]]
##data qc
cm<-colMeans(df,na.rm=TRUE)
df<-df[,cm>.05 & cm<.95]
rm<-rowMeans(!is.na(df))
df<-df[rm>.25,]
nn<-colSums(!is.na(df))
df<-df[,nn>10]

save(df,file="modelselection_df.Rdata")

