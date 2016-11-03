require(plyr)
require(tidyr)
require(data.table)
require(forecast)

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


ff.stlm <- function(zz.ts,zz.ff,ff.empty=c(2:24), printOK=FALSE) {
  ff           <- ff.empty; if (length(zz.ff)>0) ff <- zz.ff;
  ff           <- c(max(min(ff.empty),min(ff)-1):(max(ff)+1))
  ff           <- c(min(ff.empty):max((max(ff)+1),max(ff.empty)))
  ff.rmse.best <- max(zz.ts);
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
  if (printOK) print(paste("ff.stlm",
                           as.character(ff.rmse.best),
                           as.character(ff.best),
                           "(",as.character(min(ff)),"-",as.character(max(ff)),")"))
  return(ff.stlm.best)
}


zz.window       = 30 # 30 # 45-- # weeks 50*7=350 days (21 week is baddly)
zz.forecast.day = 30


task2             <- data.frame(); 
task2.stlm        <- task2;
task2.arima       <- task2;

task2.mcc         <- unique(agg2.mcc.ts$mcc_code)

task2.rmse        <- rep(-1,times=length(task2.mcc))
names(task2.rmse) <- task2.mcc
task2.rmse.stlm   <- task2.rmse
task2.rmse.arima  <- task2.rmse

task2.bic.arima   <- task2.rmse
task2.bic.stlm    <- task2.rmse
task2.bic         <- task2.rmse


ii = 0; time.begin = Sys.time()

for (mcc in task2.mcc)
  #if (mcc %in% c(3501,5681,5532,7512)) 
    {

  ii  = ii+1
  print(paste(Sys.time(),"mcc ==> (",as.character(ii),")",as.character(mcc)))
  
  zz    = agg2.mcc.ts$ss[agg2.mcc.ts$mcc_code==mcc]
  zz.ts = ts(log(500-zz),frequency = 7)

  zz.ts = window(zz.ts,start=zz.window)

  
  zz.stlm      <- ets(zz.ts,opt.crit = 'mse')
  zz.stlm.for  <- forecast(zz.stlm,h=zz.forecast.day)
  
  #plot(zz.stlm$stl)
  ##summary(zz.stlm$stl)
  #plot(zz.stlm.for)
  #sqrt(sum(zz.stlm$residuals^2)/length(zz.stlm$residuals))

  task2.rmse.stlm [as.character(mcc)] = sqrt(sum(zz.stlm$residuals^2)/length(zz.stlm$residuals))

  dfff         = as.numeric(zz.stlm.for$mean); dfff=ifelse(dfff<=log(500),log(500),dfff)
  dff          = data.frame("volume"=exp(dfff)-500)
  dff$mcc_code = mcc
  dff$day      = c(1:30)
  task2        = rbind(task2,dff)

}

print(paste("Work timing (min) :",as.character(Sys.time()-time.begin)))

task2.rmse   = task2.rmse.stlm
summary(task2.rmse)

xa = as.character(c(3501,5681,5532,7512,8299,8071,8220,5967,5968))
task2.rmse[xa]

str(dff); 
str(task2)

#xa=(c(1:nrow(task2))<=(184*30))
task2.1 = task2 #[xa,]
task2.1$day = task2.1$day + ds.max
task2.1$volume[task2.1$volume<0] = 0.0

if (FALSE) {

task2.last = data.frame(rmse=task2.rmse,
                        rmse.arima=task2.rmse.arima,
                        rmse.stlm=task2.rmse.stlm,
                        mcc_code=task2.mcc)

task2.last.bad = data.frame(rmse=task2.rmse,
                        rmse.arima=task2.rmse.arima,
                        rmse.stlm=task2.rmse.stlm,
                        mcc_code=task2.mcc)

#task2.last.best = task2.last
}

nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task2-",nStep,'.csv',sep='') 
write.csv(task2.1[c(2,3,1)],file=outfile,quote=FALSE,row.names=FALSE)

