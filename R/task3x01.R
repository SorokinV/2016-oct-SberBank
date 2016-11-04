require(plyr)
#require(tidyr)
#require(caTools)
require(forecast)

# Clear variables

rm(list=setdiff(ls(),c('trs','trgnd','gnd','x0')))

# Build absent customer_id in gender data

customer.all = sort(unique(trs$customer_id))
customer.gnd = sort(unique(gnd$customer_id))
customer.abs = setdiff(customer.all,customer.gnd)

df = data.frame("customer_id" = customer.abs)

rm(customer.all,customer.gnd)

# build grid for decart(customer_id*mcc-code)

df.temp.0 = data.frame("mcc_code"=sort(unique(trs$mcc_code)))
iCount    = 0
for (i in customer.abs) {
  df.temp.1 = df.temp.0;
  df.temp.1$customer_id = i;
  if (iCount==0) {
    df.grid = df.temp.1  
  } else {
    df.grid = rbind(df.grid,df.temp.1)
  }
  iCount = iCount + 1;
}

rm(iCount,df.temp.0,df.temp.1)

# trs3 only absent gender customer_id and minus transaction

trs3 = merge(df,trs,by='customer_id')
trs3 = subset(trs3,amount<0)

rm(df)

#--------------------------


trs3$month = floor((trs3$day+29-(max(trs3$day)%%30))/30)

trs3$tr_datetime = NULL
trs3$tr_type     = NULL
trs3$term_id = NULL


str(trs3)

# build aggregation on customer_id, mcc_code, month sum on amount

agg3.ccm = 
  ddply(trs3,.(customer_id,mcc_code,month),summarise,
        amount=sum(amount))

agg3.cm = 
  ddply(trs3,.(customer_id,month),summarise,
        amount=sum(amount))

agg3.for = 
  ddply(agg3.ccm,.(customer_id,mcc_code),summarise,
        max.month = max(month),
        cnt.month = length(month))

#------------------------------------------------------------

# (best) df.for = subset(agg3.for,(max.month>=10)&(cnt.month>=3))
# rmse = 1.33 df.for = agg3.for
# rmse = 1.33 df.for = subset(agg3.for,(max.month>=7)) #&(cnt.month>=3))
df.for = subset(agg3.for,(max.month>=12)&(cnt.month>=3))
df   = data.frame()
df.m = data.frame(month=0:15)

print(paste(Sys.time(),"beg"))

for (ii in 1:nrow(df.for)) {
  if ((ii%%1000)==0) print(paste(Sys.time(),"-->",ii));
  zz.df           = subset(agg3.ccm,(agg3.ccm$mcc_code==df.for$mcc_code[ii])&(agg3.ccm$customer_id==df.for$customer_id[ii]))   
  zz.df           = merge(df.m,zz.df,all.x=TRUE); zz.df$amount[is.na(zz.df$amount)] = 0.0 
  zz              = ts(log(1-zz.df$amount), frequency = 2)
  #print(zz)
  
  ## rmse = 1.36 (don't best) 04.11.2016 (opt.crit=likehood)
  ## zz.stl          = stlm(zz)#,s.window=NULL)#,method = 'arima')
  
  #  rmse = 1.3168 (best with mse 04:00 04/11/2016)
  #zz.stl          = ets (zz,opt.crit = 'mse')
  
  #  mae  result = 1.396713 
  ##zz.stl          = ets (zz,opt.crit = 'mae')#,s.window=NULL)#,method = 'arima')
  
  #  sigma  result = 1.316878
  #zz.stl          = ets (zz,opt.crit = 'sigma')
  
  #  all datas and nmse = 10 rmse = 1.33 (baddly)
  #zz.stl          = ets (zz,opt.crit = 'mse',nmse=10)
  
  #  all datas rmse =
  zz.stl          = ets (zz,opt.crit = 'mse')
  
  #plot(zz.stl$stl)
  #sqrt(sum(zz.stl$residuals^2)/length(zz.stl$residuals))
  zz.for          = forecast(zz.stl,h=1)
  dff             = data.frame("volume"=exp(as.numeric(zz.for$mean))-1)
  dff$customer_id = df.for$customer_id[ii]
  dff$mcc_code    = df.for$mcc_code[ii]
  df           = rbind(df,dff)
  
  #if (ii>=1000) break();
  
}

print(paste(Sys.time(),"end",ii))

str(df)

rm(zz,zz.stl,zz.for,dff)

# build result for task3

task3 = merge(df.grid,df,all.x = TRUE)
task3$volume[is.na(task3$volume)]=0.0
str(task3)

nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task3-",nStep,'.csv',sep='') 
write.csv(task3[c(2,1,3)],file=outfile,quote=FALSE,row.names=FALSE)


