require(plyr)
#require(tidyr)
require(caTools)

# Build absent customer_id in gender data

customer.all = sort(unique(trs$customer_id))
customer.gnd = sort(unique(gnd$customer_id))
customer.abs = setdiff(customer.all,customer.gnd)

df = data.frame("customer_id" = customer.abs)

rm(customer.all,customer.gnd,customer.abs)

# trs3 only absent gender customer_id and minus transaction

trs3 = merge(df,trs,by='customer_id')
trs3 = subset(trs3,amount<0)

rm(df)

# build days and month

#--------------------------------------------------------

# regexpr("[0-9]+","00 00:00:00")
dd = regexpr("[0-9]+",trs3$tr_datetime)
ds = as.numeric(substr(trs3$tr_datetime,dd,dd+attr(dd,"match.length")-1)); head(ds)
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


