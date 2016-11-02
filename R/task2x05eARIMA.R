require(plyr)
require(tidyr)
require(data.table)

rm(list=setdiff(ls(),c('trs','trgnd','gnd','x0')))

#
# get minus transaction and build days
#

trsm = as.data.table(subset(trs,trs$amount<0.0))

ds.max = max(trsm$day)
ds.max = ds.max - 30  ###
#########trsm = subset(trs,trs$amount<0.0)

#--------------------------------------------------

agg2.mcc = 
  ddply(trsm, ###
        .(mcc_code,day),summarise,
        ss  = sum(amount),
        nn  = length(amount),
        mm  = mean(amount))

agg2.mcc.type = 
  ddply(trsm,.(mcc_code,day,tr_type),summarise,nn=length(amount))
agg2.mcc.type$tr_type = paste('tr',as.character(agg2.mcc.type$tr_type),sep='_')
#agg2.mcc.type$nn = 1
xreg.type = spread(agg2.mcc.type,tr_type,nn,fill=0)


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
agg2.mcc.ts$ss[is.na(agg2.mcc.ts$ss)]=0.0; str(agg2.mcc.ts)

agg2.mcc.ts$wday = (agg2.mcc.ts$day+5)%%7  ###

rm(df)

# ----------------------------------
# compute month, mday, wday, tmday

months1   = c(31,28,31,30,31,30,31,31,30,31,30,31)
months2   = c(31,29,31,30,31,30,31,31,30,31,30,31)

ax        = agg2.mcc.ts$day+x0
agg2.mcc.ts$mday  = as.numeric(strftime(ax,'%d'))
agg2.mcc.ts$month = as.numeric(strftime(ax,'%m'))
agg2.mcc.ts$tmday = months1[agg2.mcc.ts$month]-agg2.mcc.ts$mday # tail days from month end 

rm(ax,months1,months2)


agg2.mcc.ts.full = agg2.mcc.ts

agg2.mcc.ts = subset(agg2.mcc.ts.full,day<=ds.max)

# build days xreg arrays

xreg.wday  = matrix(0,nrow=nrow(agg2.mcc.ts.full),ncol=7)
xreg.mday  = matrix(0,nrow=nrow(agg2.mcc.ts.full),ncol=max(agg2.mcc.ts.full$mday))
xreg.month = matrix(0,nrow=nrow(agg2.mcc.ts.full),ncol=max(agg2.mcc.ts.full$month))
xreg.tmday = matrix(0,nrow=nrow(agg2.mcc.ts.full),ncol=max(agg2.mcc.ts.full$tmday+1))

for (i in 1:nrow(agg2.mcc.ts.full)) {
  xreg.wday [i,agg2.mcc.ts.full$wday[i]+1]   = 1
  xreg.mday [i,agg2.mcc.ts.full$mday[i]]     = 1
  xreg.month[i,agg2.mcc.ts.full$month[i]]    = 1
  xreg.tmday[i,agg2.mcc.ts.full$tmday[i]+1]  = 1
}

xreg.type  = merge(agg2.mcc.ts.full[,1:2],xreg.type,all.x=TRUE)
xreg.type$mcc_code=NULL
xreg.type$day=NULL
xreg.type[is.na(xreg.type)]=0
xreg.type.1 = xreg.type
for(i in 1:(ncol(xreg.type)-1))    if (xreg.type[1,i]!=-1) {
  for (j in ncol(xreg.type):(i+1)) if (xreg.type[1,j]!=-1) {
    if (all(xreg.type[,i]==xreg.type[,j])) xreg.type[,j]=-1
  }
}
for(i in ncol(xreg.type):1) if (xreg.type[1,i]<0) xreg.type[,i]=NULL

xreg.ts.full = cbind(xreg.wday,xreg.month,xreg.mday,xreg.tmday,xreg.type)

agg2.mcc.ts  = subset(agg2.mcc.ts.full,day<=ds.max)

xreg.ts      = xreg.ts.full[agg2.mcc.ts.full$day<=ds.max,]


# build ssa&predict for all mcc code

require(forecast)

ff.ts  <- function (zz.ts, zz.len=length(zz.ts), ff.min=5, ff.max=60, ff.pr=1.96) {
  
  zz.ts.pacf   = Pacf(zz.ts,plot=FALSE,lag.max = length(zz.ts))$acf[,1,1]
  attr(zz.ts.pacf,'names') = 1:(length(zz.ts)-1)
  zz.ts.pacf   = sort(zz.ts.pacf[abs(zz.ts.pacf)>(ff.pr/sqrt(zz.len))],decreasing = TRUE)
  zz.ts.pacf   = as.numeric(attr(zz.ts.pacf,'names'))
  zz.ts.pacf   = zz.ts.pacf[(zz.ts.pacf>=ff.min)&(zz.ts.pacf<=ff.max)]
  
  return(zz.ts.pacf)
  
}

ff.ts.x <- function(zz.ts) {
  if (length(zz.ts<=1)) return(zz.ts)
  zz.ts.1 = zz.ts; 
  names(zz.ts.1) <- zz.ts.1; zz.ts.1 <- sort(zz.ts.1)
  for (i in 1:(length(zz.ts.1)-1)) if (zz.ts.1[i]>=0){
    for (j in (i+1):length(zz.ts.1)) if (zz.ts.1[j]>=0){
      if ((zz.ts.1[j]%%zz.ts.1[i])==0) zz.ts.1[j] <- -1
    }
  }
  zz.ts.1 = as.numeric(names(zz.ts.1[zz.ts.1<0]))
  return(setdiff(zz.ts,zz.ts.1))
}

ff.xreg <- function (zz.ts,zz.ff,frr=FALSE) {
  
  ff.mat = matrix(0,nrow=length(zz.ts),ncol=1);
  for (ii in zz.ff) {
    if (frr) {
         ff.mat  = cbind(ff.mat,fourier(ts(zz.ts,frequency = ii),K=min(10,ii%/%3)))
    } else {ff.mat  = cbind(ff.mat,seasonaldummy(ts(zz.ts,frequency = ii))) 
         }
  }
  ff.mat=ff.mat[,2:ncol(ff.mat)]
  return(ff.mat)
}

ff.xreg.f <- function (zz.ts,zz.ff,zz.hh,frr=FALSE) {
  
  ff.mat = matrix(0,nrow=zz.hh,ncol=1);
  for (ii in zz.ff) {
    if (frr) {
      ff.mat  = cbind(ff.mat,fourier(ts(zz.ts,frequency = ii),K=min(10,ii%/%3),h=zz.hh)) 
    } else 
      { ff.mat  = cbind(ff.mat,seasonaldummy(ts(zz.ts,frequency = ii),h=zz.hh))
          }
  }
  ff.mat=ff.mat[,2:ncol(ff.mat)]
  return(ff.mat)
}



zz.window       = 30 # weeks 50*7=350 days
zz.forecast.day = 30


task2 = data.frame(); ii = 0; time.begin = Sys.time()

task2.mcc  = unique(agg2.mcc.ts$mcc_code)
task2.rmse = rep(-1,times=length(task2.mcc))
names(task2.rmse) <- task2.mcc

for (mcc in task2.mcc) if (task2.rmse[as.character(mcc)]<0) 
  if (mcc %in% c(3501,5681,5532,7512)) {

  ii  = ii+1
  print(paste(Sys.time(),"mcc ==> (",as.character(ii),")",as.character(mcc)))
  
  zz    = agg2.mcc.ts$ss[agg2.mcc.ts$mcc_code==mcc]
  zz.ts = ts(log(500-zz),frequency = 7)

  zz.ts = window(zz.ts,start=zz.window)

  zz.ff = ff.ts(window(zz.ts,start=zz.window))
  
  zz.arima  <- NULL
  zz.for    <- NULL
  
  zz.stlm   <- stlm(zz.ts); 
  sqrt(sum(zz.stlm$residuals^2)/length(zz.stlm$residuals))

  if (length(zz.ff)==0) {
    zz.arima     = auto.arima(zz.ts,
                              max.order=15,
                              max.P=5,max.D=5,max.p = 5,max.q = 5,max.Q = 3)
    zz.for       = forecast(zz.arima,h=zz.forecast.day)
  }

  if (length(zz.ff)>0) {
    
    zz.ff        = ff.ts.x(zz.ff); 
    zz.ff        = c(zz.ff[1])
    
    zz.xreg      = ff.xreg  (zz.ts,zz.ff,frr=FALSE);
    zz.xreg.f    = ff.xreg.f(zz.ts,zz.ff,zz.forecast.day,frr=FALSE)
    
    for (jj in 1:1) {
    tryCatch({
      
    zz.arima  <- auto.arima(zz.ts,
                              max.order=15,
                              max.P=5,max.D=5,max.p = 5,max.q = 5,max.Q = 3,
                              #stationary = FALSE,
                              seasonal = TRUE,
                              trace = TRUE,
                              xreg=zz.xreg);
    
    zz.for       = forecast(zz.arima,h=zz.forecast.day,
                            xreg = zz.xreg.f)
    }, finally = print("error=====================") );
    
    }
    }
  
  #summary(zz.arima)
  #tsdisplay(residuals(zz.arima),lag.max = 60)
  #Pacf(residuals(zz.arima),lag.max = 60)
  #hist(residuals(zz.arima),breaks = 50)

  if (!is.null(zz.arima)) {
  
  task2.rmse[as.character(mcc)] = sqrt(sum(zz.arima$residuals^2)/zz.arima$nobs)

  dfff         = as.numeric(zz.for$mean); dfff=ifelse(dfff<=log(500),log(500),dfff)
  dff          = data.frame("volume"=exp(dfff)-500)
  dff$mcc_code = mcc
  dff$day      = c(1:30)
  task2        = rbind(task2,dff)
  
  }
  
}

print(paste("Work timing (min) :",as.character(Sys.time()-time.begin)))
task2.rmse


str(dff); 
str(task2)

##rm(zz,zz.ssa,zz.for,dff)

task2$day = task2$day+max(agg2.mcc.ts$day)
#task2$day = task2$day+max(agg2.mcc.ts$day)

task2.diff = merge(task2,agg2.mcc.ts.full,by=c('mcc_code','day'),all.x=TRUE)

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

