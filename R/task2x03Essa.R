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

zz.forecast.day = 30
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
  zz.ts        = window(zz.ts.1,start=0)
  
  #Pacf(zz);
  #Pacf(zz.ts);
  
  zz.ssa       = ssa(zz.ts,neig=60,L=60) #zz.L)
  
  g1 <- grouping.auto(zz.ssa,grouping.method = "wcor",group=1:60,nclust=8)
  #plot(wcor(zz.ssa,g1))
  r1 <- reconstruct(zz.ssa,groups = g1)
  r1.res <- attr(r1,'residual')
  #sqrt(sum(r1.res^2)/length(r1.res))
  #plot(r1)

  zz.for       = predict(zz.ssa,len=zz.forecast.day,groups = g1)
#  plot(zz.for[[4]])
  zz.for.1     = zz.for[[1]]+zz.for[[8]]; 
  #for(jj in 2:3) #length(zz.for)) 
  #  zz.for.1 = zz.for.1 + zz.for[[jj]];
  #plot(zz.for.1)

  r1.res = 
  task2.rmse.ssa [as.character(mcc)] = sqrt(sum(r1.res^2)/length(r1.res))

  dff          = data.frame("volume"=exp(as.numeric(zz.for.1))-500)
  dff$mcc_code = mcc
  dff$day      = c(1:30)
  task2        = rbind(task2,dff)
  
}

print(paste("Work timing (min) :",as.character(Sys.time()-time.begin)))

summary(task2.rmse.ssa)

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


###----------------------------------------

unlist(lapply(r1,sd))/(unlist(lapply(r1,max))-unlist(lapply(r1,min)))
plot(r1[[1]]+r1[[2]]+r1[[3]]+r1[[4]])

yy = subset(agg2.mcc.ts.full,agg2.mcc.ts.full$day>=427&agg2.mcc.ts.full$mcc_code==mcc)
yy.ts = ts(log(500-yy$ss),frequency = 7,start=61)
plot(yy.ts)
plot(zz.for[[1]]+zz.for[[2]]+zz.for[[3]]+zz.for[[4]]+
     zz.for[[5]]+zz.for[[6]]+zz.for[[7]]+zz.for[[8]])
yy.res = (zz.for[[1]]+zz.for[[2]]+zz.for[[3]]+zz.for[[4]]+
          zz.for[[5]]+zz.for[[6]]+zz.for[[7]]+zz.for[[8]]-yy.ts)#-
          zz.for[[6]]-zz.for[[5]]-zz.for[[4]]-zz.for[[3]]-
          zz.for[[2]]-zz.for[[7]]#-zz.for[[8]]
yy.res = (zz.for[[1]]-yy.ts)+zz.for[[8]]+zz.for[[7]]+zz.for[[6]]
yy.res = (zz.for[[1]]-yy.ts)+
#  zz.for[[2]]+
#  zz.for[[3]]+
#  zz.for[[4]]+
#  zz.for[[5]]+
#  zz.for[[6]]+
#  zz.for[[7]]#+
  zz.for[[8]]
##yy.res
sqrt(sum(yy.res^2)/length(yy.res))
plot(r1)
plot(yy.res)
