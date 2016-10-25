require(plyr)
#require(tidyr)
require(forecast)

#
# get minus transaction and build days
#

trsm = subset(trs,trs$amount<0.0)

dd = regexpr("[0-9]+",trs$tr_datetime);
ds = as.numeric(substr(trs$tr_datetime,dd,dd+attr(dd,"match.length")-1)); 
ds.max = max(ds)

#### ds.max = ds.max - 30  ###

dsm  = as.numeric(ds[trs$amount<0.0])
trsm$day = dsm
#dsmw = dsm %% 7

rm(dd,ds,dsm)

hh = regexpr("[0-9]+[0-9]+:[0-9]+[0-9]+:[0-9]+[0-9]+",trsm$tr_datetime)
h  = substr(trsm$tr_datetime,hh+0,hh+8-1)
h  = strptime(h,"%H:%M:%S"); 

trsm$time =(h$hour*60+h$min)*60+(h$sec%%60);

rm(h,hh)

trsm = subset(trsm,time>0) ###

#----------------------------------------------------

agg2.mcc = 
  ddply(trsm,.(mcc_code,day),summarise,
        ssum=sum(amount),
        nn=length(amount),
        mmean=mean(amount),
        ssd=sd(amount))

# -----------------------------------------
# build grid for mcc-code and day

df.temp.0 = data.frame("day"=c(min(agg2.mcc$day):max(agg2.mcc$day)))
iCount    = 0
for (i in sort(unique(agg2.mcc$mcc_code))) {
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

agg2.mcc.ts = merge(df[c(2,1)],agg2.mcc[c(1,2,3)],all.x=TRUE); 
agg2.mcc.ts$ssum[is.na(agg2.mcc.ts$ssum)]=0.0; str(agg2.mcc.ts)

agg2.mcc.ts$wday = (agg2.mcc.ts$day+5)%%7  ###

rm(df)

# build stl for all mcc code

task2 = data.frame()

for (mcc in unique(agg2.mcc.ts$mcc_code)) {
  zz           = agg2.mcc.ts$ssum[agg2.mcc.ts$mcc_code==mcc]
  zz.ts        = ts(log(500-zz),frequency = 7)
  zz.arima     = auto.arima(zz.ts,max.order=15,max.P=5,max.D=5,max.p = 5,max.q = 5,max.Q = 3)
  zz.for       = forecast(zz.arima,h=30)
  dff          = data.frame("volume"=exp(as.numeric(zz.for$mean))-500)
  dff$mcc_code = mcc
  dff$day      = c(1:30)
  task2        = rbind(task2,dff)
  
}

#str(dff); 
str(task2)

rm(zz,zz.stl,zz.for,dff)

task2$day = task2$day+max(agg2.mcc.ts$day)

#---------------------------------------------

nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task2-",nStep,'.csv',sep='') 
write.csv(task2[c(2,3,1)],file=outfile,quote=FALSE,row.names=FALSE)

