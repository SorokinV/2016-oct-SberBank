require(plyr)
#require(tidyr)

rm(list=setdiff(ls(),c('trs','trgnd','gnd','x0')))

#
# get minus transaction and build days
#

trsm = subset(trs,trs$amount<0.0)

ds.max = max(trsm$day)
#ds.max = ds.max - 30  ###

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

agg2.mcc.ts = subset(agg2.mcc.ts,day<=ds.max)


# build ssa&predict for all mcc code

require(Rssa)

estGroup  <- function(zz.for,zz.ts,printOK=FALSE) {
   zz.cur = rowSums(as.data.frame((zz.for)))
#   print(zz.cur)
   
   zz.res.min <- sqrt(sum((zz.cur-zz.ts)^2)/length(zz.ts))
   zz.group   <- c(1:length(zz.for))
   
   while (TRUE) {
     zz.res.i <- zz.res.min; i.res.i = 0;
     if (printOK) print(zz.res.min)
     if (printOK) print(zz.group)
     for (i in zz.group) {
       zz.cur.i = zz.cur-zz.for[[i]]
       zz.res.i = sqrt(sum((zz.cur.i-zz.ts)^2)/length(zz.ts))
       if (zz.res.i<zz.res.min) {
         zz.res.min = zz.res.i; i.res.i = i;
       }
     }
     if (i.res.i==0) break;
     if (length(zz.group)==1) break;
     zz.cur   = zz.cur-zz.for[[i.res.i]]
     zz.group = setdiff(zz.group,c(i.res.i))
   }
   
   return(zz.group)
}


ssa.L     = 150 #60
ssa.neig  = 90
ssa.group = 1:60
ssa.clust = 70 # 10

zz.window       = 5
zz.forecast.day = 30
zz.estimate     = 30
task2 = data.frame()

task2.mcc         <- unique(agg2.mcc.ts$mcc_code)

task2.rmse.ssa    <- rep(-1,times=length(task2.mcc))
names(task2.rmse.ssa) <- task2.mcc

ii = 0; time.begin = Sys.time()

for (mcc in task2.mcc) {
  
  ii  = ii+1
  print(paste(Sys.time(),"mcc ==> (",as.character(ii),")",as.character(mcc)))

  zz           = agg2.mcc.ts$ss[agg2.mcc.ts$mcc_code==mcc]
  zz.ts.1      = ts(log(500-zz),start=0,frequency = 7)
  zz.ts        = window(zz.ts.1,start=zz.window) #15)

  ## estimate groups  
  zz.ts.e      = zz.ts[1:(length(zz.ts)-zz.estimate)]
  zz.ssa       = ssa(zz.ts.e,neig=ssa.neig,L=ssa.L)
  #g1 <- grouping.auto(zz.ssa,grouping.method = "wcor",group=ssa.group,nclust=ssa.clust)
  g1 <- grouping.auto(zz.ssa,grouping.method = "pgram",base='eigen',freq.bins=ssa.clust)
  plot(wcor(zz.ssa,g1))
  r1 <- reconstruct(zz.ssa,groups = g1)
  #plot(r1)
  zz.for       = predict(zz.ssa,len=zz.estimate,groups = g1)
  zz.ts.ee     = zz.ts[(length(zz.ts)-zz.estimate+1):length(zz.ts)]
  
  g1.e         <- estGroup(zz.for,zz.ts.ee,printOK = FALSE)
  
  #r1.e         <- reconstruct(zz.ssa,groups = g1.e)
  #plot(r1)
  #plot(r1.e)
  
  
  #Pacf(zz);
  #Pacf(zz.ts);
  
  zz.ssa       = ssa(zz.ts,neig=ssa.neig,L=ssa.L)
  
  #g1 <- grouping.auto(zz.ssa,grouping.method = "wcor",group=ssa.group,nclust=ssa.clust)
  g1 <- grouping.auto(zz.ssa,grouping.method = "pgram",base='eigen',freq.bins=ssa.clust)
  #plot(wcor(zz.ssa,g1))
  r1 <- reconstruct(zz.ssa,groups = g1.e)
  r1.res <- attr(r1,'residual')
  r1.res <- sqrt(sum(r1.res^2)/length(r1.res))
  r1.res; task2.rmse.ssa.last[as.character(mcc)]

  zz.for       = predict(zz.ssa,len=zz.forecast.day,groups = g1.e)
  zz.for.1     = rowSums(as.data.frame(zz.for))

  task2.rmse.ssa [as.character(mcc)] = r1.res

  dff          = data.frame("volume"=exp(as.numeric(zz.for.1))-500)
  dff$mcc_code = mcc
  dff$day      = c(1:30)
  task2        = rbind(task2,dff)
  
}

print(paste("Work timing (min) :",as.character(Sys.time()-time.begin)))

summary(task2.rmse.ssa)
summary(task2.rmse.ssa.last)

#rm(zz,zz.ssa,zz.for,dff)

####-----------------------------------------------------------------

xa = as.character(c(3501,5681,5532,7512,8299,8071,8220,5967,5968))
task2.rmse.ssa[xa]

str(task2)

#xa=(c(1:nrow(task2))<=(184*30))
task2.1 = task2 #[xa,]
task2.1$day = task2.1$day + ds.max
task2.1$volume[task2.1$volume<0] = 0.0


nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task2-",nStep,'.csv',sep='') 
write.csv(task2.1[c(2,3,1)],file=outfile,quote=FALSE,row.names=FALSE)

task2.rmse.ssa.last = task2.rmse.ssa

