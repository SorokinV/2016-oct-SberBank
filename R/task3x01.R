require(plyr)
#require(tidyr)
#require(caTools)
require(forecast)


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

# build days and month

#--------------------------------------------------------

# regexpr("[0-9]+","00 00:00:00")

dd = regexpr("[0-9]+",trs$tr_datetime); 
ds = as.numeric(substr(trs$tr_datetime,dd,dd+attr(dd,"match.length")-1)); 
ds.max = max(ds)

dd = regexpr("[0-9]+",trs3$tr_datetime)
ds = as.numeric(substr(trs3$tr_datetime,dd,dd+attr(dd,"match.length")-1)); 
# last day is number = 489, next day is 480 and month = 16
ds = ds + 29 - (ds.max %% 30) 
head(ds)

md = ds %% 30
mm =  floor(ds / 30)
head(md); head(mm); 
trs3$day=ds
trs3$month = mm
trs3$m.day = md
trs3$tr_datetime = NULL
trs3$tr_type     = NULL
trs3$term_id = NULL


rm(dd,ds,md,mm)

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

df.for = subset(agg3.for,(max.month>=10)&(cnt.month>=3))
df.i   = 0
df   = data.frame()
df.m = data.frame(month=0:15)

print(paste(Sys.time(),"beg"))

for (ii in 1:nrow(df.for)) {
  if ((ii%%1000)==0) print(paste(Sys.time(),"-->",ii));
  zz.df           = subset(agg3.ccm,(agg3.ccm$mcc_code==df.for$mcc_code[ii])&(agg3.ccm$customer_id==df.for$customer_id[ii]))   
  zz.df           = merge(df.m,zz.df,all.x=TRUE); zz.df$amount[is.na(zz.df$amount)] = 0.0 
  zz              = ts(log(1-zz.df$amount), frequency = 2)
  #print(zz)
  zz.stl          = stlm(zz,s.window="per")
  #plot(zz.stl$stl)
  zz.for          = forecast(zz.stl,h=1)
  dff             = data.frame("volume"=exp(as.numeric(zz.for$mean))-1)
  dff$customer_id = df.for$customer_id[ii]
  dff$mcc_code    = df.for$mcc_code[ii]
  df           = rbind(df,dff)
  
  df.i = df.i + 1
  #if (df.i>=1000) break();
  
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
write.csv(task3[c(1,2,3)],file=outfile,quote=FALSE,row.names=FALSE)


