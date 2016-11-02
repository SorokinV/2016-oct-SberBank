require(plyr)
require(tidyr)
require(data.table)

rm(list=setdiff(ls(),c('trs','trgnd','gnd','x0')))

#
# get minus transaction and build days
#

trsm = as.data.table(subset(trs,trs$amount<0.0))

ds.max = max(trsm$day)
#####ds.max = ds.max - 30  ###
#########trsm = subset(trs,trs$amount<0.0)

#--------------------------------------------------

agg2.mcc = 
  ddply(trsm, ###
        .(mcc_code,day),summarise,
        ss  = sum(amount),
        nn  = length(amount),
        mm  = mean(amount))


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

agg2.mcc.ts.full = agg2.mcc.ts
agg2.mcc.ts  = subset(agg2.mcc.ts.full,day<=ds.max)


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


ff.stlm <- function(zz.ts,zz.ff,ff.empty=2:24, printOK=FALSE) {
  ff           <- ifelse(length(zz.ff)>0,zz.ff,c(ff.empty))
  ff.rmse.best <- 10000;
  ff.stlm.best <- NULL;
  ff.best      <- -1;
  for (i in ff) {
    ff.stlm = stlm(zz.ts,s.window = i)
    ff.rmse = sqrt(sum(ff.stlm$residuals^2)/length(ff.stlm$residuals))
    if (ff.rmse.best>ff.rmse) {
      ff.stlm.best <- ff.stlm
      ff.rmse.best <- ff.rmse
      ff.best      <- i
    }
  }
  if (printOK) print(paste("ff.stlm",as.character(ff.rmse.best),as.character(ff.best)))
  return(ff.stlm.best)
}


zz.window       = 30 # weeks 50*7=350 days
zz.forecast.day = 30


task2             <- data.frame(); 
task2.stlm        <- task2;
task2.arima       <- task2;

task2.mcc         <- unique(agg2.mcc.ts$mcc_code)

task2.rmse        <- rep(-1,times=length(task2.mcc))
names(task2.rmse) <- task2.mcc
task2.rmse.stlm   <- task2.rmse
task2.rmse.arima  <- task2.rmse

ii = 0; time.begin = Sys.time()

for (mcc in task2.mcc)
  #if (mcc %in% c(3501,5681,5532,7512)) 
    {

  ii  = ii+1
  print(paste(Sys.time(),"mcc ==> (",as.character(ii),")",as.character(mcc)))
  
  zz    = agg2.mcc.ts$ss[agg2.mcc.ts$mcc_code==mcc]
  zz.ts = ts(log(500-zz),frequency = 7)

  zz.ts = window(zz.ts,start=zz.window)

  zz.ff = ff.ts(window(zz.ts,start=zz.window))
  
  zz.stlm      <- ff.stlm(zz.ts,zz.ff,ff.empty=4:24,printOK = TRUE)
  ####zz.stlm      <- stlm(zz.ts); 
  zz.stlm.for  <- forecast(zz.stlm,h=zz.forecast.day)
  plot(zz.stlm$stl)
  plot(zz.stlm.for)
  
  if (length(zz.ff)>=1) {
    zz.ts = ts(zz.ts,frequency = zz.ff[1])
    zz.ff[1] = NA
    zz.ff    = zz.ff[!is.na(zz.ff)]
  }

  if (length(zz.ff)==0) {
    zz.arima     = stlm(zz.ts,method = 'arima',
                              max.order=15,
                              max.P=5,max.D=5,max.p = 5,max.q = 5,max.Q = 3)
    zz.arima.for = forecast(zz.arima,h=zz.forecast.day)
  }

  if (length(zz.ff)>0) {
    
    zz.ff        = ff.ts.x(zz.ff); 
    zz.ff        = c(zz.ff[1])
    
    zz.xreg      = ff.xreg  (zz.ts,zz.ff,frr=FALSE);
    zz.xreg.f    = ff.xreg.f(zz.ts,zz.ff,zz.forecast.day,frr=FALSE)
    
    zz.arima     = stlm (zz.ts,method='arima',
                              max.order=15,
                              max.P=5,max.D=5,max.p = 5,max.q = 5,max.Q = 3,
                              #stationary = FALSE,
                              #seasonal = TRUE,
                              #trace = TRUE,
                              xreg=zz.xreg);
    
    zz.arima.for  = forecast(zz.arima,h=zz.forecast.day,
                            xreg = zz.xreg.f)
    }
  
  task2.rmse.arima[as.character(mcc)] = sqrt(sum(zz.arima$residuals^2)/length(zz.arima$residuals))
  task2.rmse.stlm [as.character(mcc)] = sqrt(sum(zz.stlm$residuals^2)/length(zz.stlm$residuals))
  
  dfff         = as.numeric(zz.arima.for$mean); dfff=ifelse(dfff<=log(500),log(500),dfff)
  dff          = data.frame("volume"=exp(dfff)-500)
  dff$mcc_code = mcc
  dff$day      = c(1:30)
  task2.arima  = rbind(task2.arima,dff)
  if (task2.rmse.arima[as.character(mcc)]<=task2.rmse.stlm[as.character(mcc)])
    task2      = rbind(task2.arima,dff)
  
  dfff         = as.numeric(zz.stlm.for$mean); dfff=ifelse(dfff<=log(500),log(500),dfff)
  dff          = data.frame("volume"=exp(dfff)-500)
  dff$mcc_code = mcc
  dff$day      = c(1:30)
  task2.stlm   = rbind(task2.stlm,dff)
  if (task2.rmse.arima[as.character(mcc)]>task2.rmse.stlm[as.character(mcc)])
    task2      = rbind(task2.stlm,dff)

}

print(paste("Work timing (min) :",as.character(Sys.time()-time.begin)))

task2.rmse   = task2.rmse.arima
task2.rmse[task2.rmse>task2.rmse.stlm]=task2.rmse.stlm[task2.rmse>task2.rmse.stlm]
summary(task2.rmse.arima)
summary(task2.rmse.stlm)
summary(task2.rmse)

task2.rmse[as.character(c(3501,5681,5532,7512))]
task2.rmse.arima[as.character(c(3501,5681,5532,7512))]
task2.rmse.stlm[as.character(c(3501,5681,5532,7512))]

str(dff); 
str(task2)

task2.1 = task2[xa,]
task2.1$day = task2.1$day + ds.max
task2.1$volume[task2.1$volume<0] = 0.0


nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task2-",nStep,'.csv',sep='') 
write.csv(task2.1[c(2,3,1)],file=outfile,quote=FALSE,row.names=FALSE)

