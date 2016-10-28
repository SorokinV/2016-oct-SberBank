require(data.table)
require(plyr)

### read main datas

trs = read.csv("./Data/transactions.csv",stringsAsFactors = FALSE)
str(trs)
trs = as.data.table(trs)

### build date and time

trs[,c('day','time'):=tstrsplit(tr_datetime,' ',fixed=TRUE,type.convert=TRUE)]

#ds.max = max(trsm$day)
#ds.max = ds.max - 30  ###

hhh = tstrsplit(trs$time,":",type.convert = TRUE)
timeX =(hhh[[1]]*60+hhh[[2]])*60+(hhh[[3]]%%60);
trs$time = timeX

rm(hhh,timeX)

trs$tr_datetime = NULL

trs$wday = (trs$day+5)%%7

### Build main datas

trsm = as.data.table(subset(trs,amount<0))
trsp = as.data.table(subset(trs,amount>0))

###