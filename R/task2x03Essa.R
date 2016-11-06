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

estGroup  <- function(zz.for,zz.ts) {
   zz.cur = rowSums(as.data.frame((zz.for)))
#   print(zz.cur)
   
   zz.res.min <- sqrt(sum((zz.cur-zz.ts)^2)/length(zz.ts))
   zz.group   <- c(1:length(zz.for))
   
   while (TRUE) {
     zz.res.i <- zz.res.min; i.res.i = 0;
#     print(zz.res.min)
#     print(zz.group)
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


ssa.L     = 60
ssa.neig  = 60
ssa.group = 1:60
ssa.clust = 25 # 10

zz.window       = 35
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
  zz.ts        = window(zz.ts.1,start=15)

  ## estimate groups  
  zz.ts.e      = zz.ts[1:(length(zz.ts)-zz.estimate)]
  zz.ssa       = ssa(zz.ts.e,neig=ssa.neig,L=ssa.L)
  g1 <- grouping.auto(zz.ssa,grouping.method = "wcor",group=ssa.group,nclust=ssa.clust)
  #plot(wcor(zz.ssa,g1))
  r1 <- reconstruct(zz.ssa,groups = g1)
  #plot(r1)
  zz.for       = predict(zz.ssa,len=zz.estimate,groups = g1)
  zz.ts.ee     = zz.ts[(length(zz.ts)-zz.estimate+1):length(zz.ts)]
  
  g1.e         <- estGroup(zz.for,zz.ts.ee)
  #r1.e         <- reconstruct(zz.ssa,groups = g1.e)
  #plot(r1)
  #plot(r1.e)
  
  
  #Pacf(zz);
  #Pacf(zz.ts);
  
  zz.ssa       = ssa(zz.ts,neig=ssa.neig,L=ssa.L)
  
  g1 <- grouping.auto(zz.ssa,grouping.method = "wcor",group=ssa.group,nclust=ssa.clust)
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
