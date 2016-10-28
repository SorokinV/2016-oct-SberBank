require(plyr)
#require(tidyr)
require(data.table)

#
# get minus transaction and build days
#

trsm = as.data.table(subset(trs,trs$amount<0.0))
#########trsm = subset(trs,trs$amount<0.0)

trsm[,c('day','time'):=tstrsplit(tr_datetime,' ',fixed=TRUE,type.convert=TRUE)]

ds.max = max(trsm$day)
ds.max = ds.max - 30  ###

rm(dd,ds,dsm)

hhh = tstrsplit(trsm$time,":",type.convert = TRUE)
timeX =(hhh[[1]]*60+hhh[[2]])*60+(hhh[[3]]%%60);
trsm$time = timeX

rm(hhh,timeX)

trsm$tr_datetime = NULL

#trsm = subset(trsm,time==0)

agg2.mcc = 
  ddply(trsm, ###
        .(mcc_code,tr_type,day),summarise,
        ssum=sum(amount),
        nn=length(amount),
        mmean=mean(amount),
        ssd=sd(amount))

agg2.mcc.tr = 
  ddply(trsm, ###
        .(mcc_code,tr_type),summarise,
        nn=length(amount),
        sumAmount=sum(amount),
        avrAmount=mean(amount),
        medAmount=median(amount),
        maxAmount=max(amount),
        lenDays  =length(unique(day)), 
        maxDay   = max(day),
        minDay   = min(day),
        lenCust  =length(unique(customer_id)), 
        lenTerm  =length(unique(term_id))) 


# -----------------------------------------
# build grid for mcc-code and day

df.temp.0 = data.frame("day"=c(min(agg2.mcc$day):max(agg2.mcc$day)))
iCount    = 0
for (i in c(1:nrow(agg2.mcc.tr))) {
  df.temp.1 = subset(df.temp.0,day>=agg2.mcc.tr$minDay[i]);
  df.temp.1$mcc_code = agg2.mcc.tr$mcc_code[i];
  df.temp.1$tr_type  = agg2.mcc.tr$tr_type[i];
  if (iCount==0) {
    df = df.temp.1  
  } else {
    df = rbind(df,df.temp.1)
  }
  iCount = iCount + 1;
}
rm(iCount,df.temp.0,df.temp.1)


# build timeSeries from all days min-max

agg2.mcc.ts = merge(df[c(2,3,1)],agg2.mcc[c(1,2,3,4)],all.x=TRUE); 
agg2.mcc.ts$ssum[is.na(agg2.mcc.ts$ssum)]=0.0; str(agg2.mcc.ts)

agg2.mcc.ts$wday = (agg2.mcc.ts$day+5)%%7  ###

rm(df)

agg2.mcc.ts.full = agg2.mcc.ts

agg2.mcc.ts = subset(agg2.mcc.ts,day<=ds.max)


# build ssa&predict for all mcc code

require(forecast)

task2 = data.frame(); time.begin = Sys.time()

for (ii in c(1:nrow(agg2.mcc.tr))) {
  
  
  #if (ii>100) break;
  
  mcc          = agg2.mcc.tr$mcc_code[ii]; if (mcc!=5681) next;
  days         = agg2.mcc.tr$lenDays[ii]
  
  if (days<=10) next;
  
  tr_type      = agg2.mcc.tr$tr_type[ii]
  nn           = agg2.mcc.tr$nn[ii]

  print(paste(Sys.time(),"mcc ==> (",as.character(ii),")",'(mcc_code,tr_type)=',
              as.character(mcc),as.character(tr_type),
              'days = ',as.character(days),
              'nn=',as.character(nn)))
  
  zz           = agg2.mcc.ts$ssum[agg2.mcc.ts$mcc_code==mcc&agg2.mcc.ts$tr_type==tr_type]
  zz.ts        = ts(log(500-zz), frequency = 7, start = agg2.mcc.tr$minDay%/%7)
  zz.ts        = ts(zz, frequency = 7, start = agg2.mcc.tr$minDay/7)
  zz.ts        = window(zz.ts,start=30)
  #zz.w         = ifelse(length(zz==0)<=150,35,20) 
  #zz.ts = window(zz.ts,start=zz.w) # all all 22 25 30 20 15 20(>5) 20(>6) 20(>7)

  # find frequency list on Pacf utility
  zz.ts.pacf   = Pacf(zz.ts,plot=FALSE,lag.max = length(zz.ts))$acf[,1,1]
  names(zz.ts.pacf) = 1:(length(zz.ts)-1)
  ##zz.ts.pacf   = sort(zz.ts.pacf[zz.ts.pacf>1.96/sqrt(length(zz.ts))],decreasing = TRUE)
  #####zz.ts.pacf   = sort(zz.ts.pacf[zz.ts.pacf>1.96/sqrt(days)],decreasing = TRUE)
  zz.ts.pacf   = sort(zz.ts.pacf[zz.ts.pacf>1.96/sqrt(length(zz.ts[zz.ts<0]))],decreasing = TRUE)
  zz.ts.pacf   = as.numeric(attr(zz.ts.pacf,'names'))
  zz.ts.pacf   = zz.ts.pacf[zz.ts.pacf>=6]
  
  zz.ts.xreg   = (!is.null(zz.ts.pacf))&(length(zz.ts.pacf)>=1)
  
  ##########zz.ts.xreg   = FALSE ########################################
  
  # build fourier arrays  
  
  if (zz.ts.xreg) {
  

    print(paste(Sys.time(),"  frequency ==>",paste(zz.ts.pacf,collapse = " ")))
    
    
    zz.ff  = fourier(ts(zz.ts,frequency = zz.ts.pacf[1]),K=min(20,zz.ts.pacf[1]-1)%/%2)
    zz.fff = fourier(ts(zz.ts,frequency = zz.ts.pacf[1]),K=min(20,zz.ts.pacf[1]-1)%/%2,h=30)
                                    
    if (length(zz.ts.pacf)>1) {
    for (i in zz.ts.pacf[2:length(zz.ts.pacf)]) {
      
      zz.ff  = cbind(zz.ff, fourier(ts(zz.ts,frequency = i),K=min(20,(i-1)%/%2)))
      zz.fff = cbind(zz.fff,fourier(ts(zz.ts,frequency = i),K=min(20,(i-1)%/%2),h=30))
      
    }}

  zz.tslm      = tslm(zz.ts~zz.ff)
  #summary(zz.tslm)
  
  # delete NA values, compute pr(t-value) and select column names
  zz.tslm.coef  = coef(zz.tslm)
  zz.tslm.vcov  = sqrt(diag(vcov(zz.tslm)))
  zz.tslm.names = intersect(names(zz.tslm.coef),names(zz.tslm.vcov))
  zz.tslm.coef  = zz.tslm.coef[zz.tslm.names]
  zz.tslm.vcov  = zz.tslm.vcov[zz.tslm.names]
  
  zz.lm.tstats  = zz.tslm.coef/zz.tslm.vcov
  zz.lm.pt      = 2*pt(abs(zz.lm.tstats),df=df.residual(zz.tslm),lower.tail = FALSE)
  
  zz.tslm.level = 0.05
  
  zz.ts.xreg   = any(zz.lm.pt[2:length(zz.lm.pt)]<zz.tslm.level);
  zz.ts.xreg   = ifelse(is.na(zz.ts.xreg),FALSE,zz.ts.xreg)

  if (zz.ts.xreg) {
  
    #zz.lm.names  = names(zz.lm.pt[zz.lm.pt[2:length(zz.lm.pt)]<zz.tslm.level])
    zz.lm.names  = names(zz.lm.pt[zz.lm.pt<zz.tslm.level])
    zz.lm.names  = tstrsplit(zz.lm.names,'zz.ff')[[2]]
    zz.lm.names  = zz.lm.names[!is.na(zz.lm.names)]
    
    zz.ffx       = zz.ff [,c(zz.lm.names)]
    zz.fff       = zz.fff[,c(zz.lm.names)]
    
    zz.arima     = auto.arima(zz.ts,
                              max.order=15,
                              max.P=5,max.D=5,max.p = 5,max.q = 5,max.Q = 3,
                              #stationary = FALSE,
                              seasonal = FALSE,xreg=zz.ffx)
    
    zz.for       = forecast(zz.arima,h=30,xreg = zz.fff)
    
    rm(zz.fff,zz.ffx)
    rm(zz.lm.names)
  }
  
  rm(zz.tslm.level,zz.lm.pt,zz.lm.tstats,zz.tslm.vcov,zz.tslm.coef,zz.tslm.names,zz.tslm)
  rm(zz.ts.pacf,zz.ff)

  } 
    
  if (!zz.ts.xreg)  {
  
    zz.arima     = auto.arima(zz.ts,
                              max.order=15,
                              max.P=5,max.D=5,max.p = 5,
                              max.q = 5,max.Q = 3)
    
    zz.for       = forecast(zz.arima,h=30)
  
  }
  
  #summary(zz.arima)
  #tsdisplay(residuals(zz.arima),lag.max = 60)
  #Pacf(residuals(zz.arima),lag.max = 60)
  #hist(residuals(zz.arima),breaks = 50)

  dfff         = as.numeric(zz.for$mean); 
  #dfff=ifelse(dfff<=log(500),log(500),dfff)
  dff          = data.frame("volume"=exp(dfff)-500)
  dff$mcc_code = mcc
  dff$tr_type  = tr_type
  dff$day      = c(1:30)
  task2        = rbind(task2,dff)
  
}

print(paste("Work timing (min) :",as.character(Sys.time()-time.begin)))


str(dff); 
str(task2)

##rm(zz,zz.ssa,zz.for,dff)

##task2 = ddply(task2,.(mcc_code,day),summarise,volume=sum(volume))

task2$day = task2$day+max(agg2.mcc.ts$day)
#task2$day = task2$day+max(agg2.mcc.ts$day)

task2.diff = merge(task2,agg2.mcc.ts.full,by=c('mcc_code','tr_type','day'),all.x=TRUE)

str(task2.diff)

task2.diff$RMSE = log(500-task2.diff$ssum)-log(abs(task2.diff$volume)+500)
print(sqrt(sum((task2.diff$RMSE)^2)/length(task2.diff$RMSE)))
plot(sort(task2.diff$RMSE))
hist(task2.diff$RMSE,breaks = 50)

######

xy  = ddply(task2.diff,.(mcc_code),summarise,rm=sum(abs(RMSE)))
xyz = merge(xyz,xy,by=c('mcc_code'))
colSums(xyz)

#############

names(xyz)<-c('mcc_code','rms','rm-r','rm-22','rm-25','rm-30','rm-20','rm-15',
              'rm-20-5','rm-20-6','rm-20-7','rm.20.6c','rm.20.6.0.5','rm.20.4.0.5')

subset(xyz[c(1,2,7,10,12,13,14,15,16,17)],
       mcc_code %in% c(742,1799,3501,4722,5310,5714,6211,6513,
                       6536,7993,7996,8011,8062,8398,8641,5681,
                       xyz$mcc_code[(xyz$`rm-20-6`-15)>xyz$rms]))

subset(xyz[c(1,2,3,4,7,10,12)],
       mcc_code %in% xyz$mcc_code[(xyz$`rm-20-6`-5)>xyz$rms])

xyz$mcc_code[xyz$`rm-20-6`>60]

######task2.diff.new = task2.diff

agg2.mcc.rmse = 
  ddply(task2.diff, ###
        .(mcc_code),summarise,
        error=sum(abs(RMSE)),
        z.len=length(agg2.mcc.ts$ssum[agg2.mcc.ts$mcc_code==mcc_code&agg2.mcc.ts$ssum<(-100)]))

min(abs(agg2.mcc.rmse$error))
agg2.mcc.rmse$mcc_code[which.min(abs(agg2.mcc.rmse$error))]
max(abs(agg2.mcc.rmse$error))
agg2.mcc.rmse$mcc_code[which.max(abs(agg2.mcc.rmse$error))]
plot(agg2.mcc.rmse$error)
plot(agg2.mcc.rmse$error,agg2.mcc.rmse$z.len)

#------ The end -------------------------------

require('plyr')

agg2.mcc.agg = 
  ddply(trsm, ###
        .(mcc_code),summarise,
        nCustomer_id = length(unique(customer_id)),
        nTr_type     = length(unique(tr_type)),
        nTerm_id     = length(unique(term_id)),
        nDays        = length(unique(day)),
        sAmount      = sum(amount),
        minDay       = min(day),
        maxDay       = max(day),
        minAmount    = min(amount),
        maxAmount    = max(amount),
        avrAmount    = mean(amount),
        medAmount    = median(amount),
        nn           = length(amount))
        
agg2.mcc.agg.tr_type = 
  ddply(trsm, ###
        .(mcc_code,tr_type),summarise,
        nCustomer_id = length(unique(customer_id)),
        nTerm_id     = length(unique(term_id)),
        nDays        = length(unique(day)),
        sAmount      = sum(amount),
        minDay       = min(day),
        maxDay       = max(day),
        minAmount    = min(amount),
        maxAmount    = max(amount),
        avrAmount    = mean(amount),
        medAmount    = median(amount),
        nn           = length(amount))




#---------------------------------------------

###nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
###outfile = paste("./Result/task2-",nStep,'.csv',sep='') 
###write.csv(task2[c(2,3,1)],file=outfile,quote=FALSE,row.names=FALSE)

