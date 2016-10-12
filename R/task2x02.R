require(plyr)
require(tidyr)
require(caTools)
require(xgboost)

#
# get minus transaction
#

trsm = subset(trs,trs$amount<0.0)
dsm  = as.numeric(ds[trs$amount<0.0])
trsm$day = dsm
dsmw = dsm %% 7

agg.mcc2 = 
  ddply(trsm,.(mcc_code,day),summarise,
        ssum=sum(amount),
        nn=length(amount),
        mmean=mean(amount),
        ssd=sd(amount))


mcc = list("mcc_code"=sort(unique(trs$mcc_code))); str(mcc)
days= list("day"=c(1:30)); str(days)

grid= data.frame(days)
grid$mcc_code = mcc$mcc_code[1]

gridtemp = grid

for (i in mcc$mcc_code[2:length(mcc$mcc_code)]) {
    gridtemp$mcc_code = i
    grid = rbind(grid,gridtemp)
}

xday = 366

xyz1 = x0+max(agg.mcc2$day)+1; xyz1=xyz1-xday; xyz1
xyz1x= xyz1-x0; as.numeric(xyz1x)

gridold = subset(agg.mcc2,(day>=xyz1x)&(day<xyz1x+31),select=c('mcc_code','day','ssum'))

gridold$volume = exp(log(500-gridold$ssum))-500
gridold$ssum = NULL

gridold$day = gridold$day+xday; head(gridold)
sort(unique(gridold$day))
sort(unique(grid$day))
tail(sort(unique(agg.mcc2$day)))

grid$day = grid$day+max(agg.mcc2$day)

#gridnew = merge(grid,gridold,by=c("mcc_code","day"),all.x=TRUE)
gridnew = merge(grid,gridold,all.x=TRUE)
gridnew$volume[is.na(gridnew$volume)] = 0

nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task2-",nStep,'.csv',sep='') 
write.csv(gridnew[c(2,1,3)],file=outfile,quote=FALSE,row.names=FALSE)

