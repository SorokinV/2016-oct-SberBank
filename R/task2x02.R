require(plyr)
require(tidyr)

require(forecast)

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

# -----------------------------------------
# build grid for mcc-code and day

df.temp.0 = data.frame("day"=c(min(agg.mcc2$day):max(agg.mcc2$day)))
iCount    = 0
for (i in sort(unique(agg.mcc2$mcc_code))) {
  df.temp.1 = df.temp.0;
  df.temp.1$mcc_code = i;
  if (iCount==0) {
    df = df.temp.1  
  } else {
    df = rbind(df,df.temp.1)
  }
  iCount = iCount + 1;
}
rm(iCount,df.temp.0,df.temp.1)


# build timeSeries from all days min-max

agg.mcc2.ts = merge(df[c(2,1)],agg.mcc2[c(1,2,3)],all.x=TRUE); 
agg.mcc2.ts$ssum[is.na(agg.mcc2.ts$ssum)]=0.0; str(agg.mcc2.ts)

rm(df)

# build stl for all mcc code

df = data.frame()

for (mcc in unique(agg.mcc2.ts$mcc_code)) {
  zz           = ts(agg.mcc2.ts$ssum[agg.mcc2.ts$mcc_code==mcc],start=0,frequency = 30)
  zz.stl       = stlm(log(500-zz),s.window = 20,method="arima")
#  zz.stl       = stlm(log(500-zz),s.window = "periodic",method="arima")
  #zz.stl       = stlm(log(500-zz))
  #  zz.stl       = stlm(zz)
  zz.for       = forecast(zz.stl,h=30)
  dff          = data.frame("volume"=exp(as.numeric(zz.for$mean))-500)
#  dff          = data.frame("volume"=as.numeric(zz.for$mean))
  dff$mcc_code = mcc
  dff$day      = c(1:30)
  df           = rbind(df,dff)
  
}

str(dff)

rm(zz,zz.stl,zz.for,dff)

df$day = df$day+max(agg.mcc2.ts$day)

#---------------------------------------------

nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task2-",nStep,'.csv',sep='') 
write.csv(df[c(2,3,1)],file=outfile,quote=FALSE,row.names=FALSE)

