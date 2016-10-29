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

trs$wday  = (trs$day+5)%%7

x0        = as.Date('2008-08-01','%Y-%m-%d')

months1   = c(31,28,31,30,31,30,31,31,30,31,30,31)
months2   = c(31,29,31,30,31,30,31,31,30,31,30,31)
ax        = trs$day+x0
trs$mday  = as.numeric(strftime(ax,'%d'))
trs$month = as.numeric(strftime(ax,'%m'))
trs$tmday = months1[trs$month]-trs$mday # tail days from month end 

rm(ax,months1,months2)

### Build main datas

trsm = as.data.table(subset(trs,amount<0))
trsp = as.data.table(subset(trs,amount>0))

###