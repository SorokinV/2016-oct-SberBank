require(data.table)
require(plyr)
require(tidyr)
require(caTools)

### build main aggregate table

agg1.cust = 
  ddply(trs, ###
        .(customer_id),summarise,
        nn        = length(amount),
        sumAmount = sum(amount),
        avrAmount = mean(amount),
        medAmount = median(amount),
        minAmount = min(amount),
        maxAmount = max(amount),
        lenDays   = length(unique(day)), 
        minDay    = min(day),
        maxDay    = max(day),
        durDays   = max(day)-min(day)+1,
        minTime   = min(time),
        maxTime   = max(time),
        durTimes  = max(time)-min(time)+1,
        lenMcc    = length(unique(mcc_code)), 
        lenType   = length(unique(tr_type)), 
        lenTerm   = length(unique(term_id))) 

agg1.cust = merge(agg1.cust,gnd,all.x = TRUE)

richm = trs[amount<0,sum(amount),by='customer_id']
richp = trs[amount>0,sum(amount),by='customer_id']
names(richm) <- c('customer_id','richm')
names(richp) <- c('customer_id','richp')

agg1.cust = merge(agg1.cust,richm,all.x = TRUE)
agg1.cust = merge(agg1.cust,richp,all.x = TRUE)

agg1.cust$richm[is.na(agg1.cust$richm)] = 0
agg1.cust$richp[is.na(agg1.cust$richp)] = 0
agg1.cust$richm = agg1.cust$richm/agg1.cust$lenDays
agg1.cust$richp = agg1.cust$richp/agg1.cust$lenDays
agg1.cust$rich  = agg1.cust$sumAmount/agg1.cust$lenDays

### build mcc_code+tr_type by customer_id

require(plyr)
require(tidyr)

agg1.code = 
  ddply(trs,.(customer_id,mcc_code,tr_type),summarise,
        nn = length(mcc_code),
        ss = sum(amount),
        mm = mean(amount))

agg1.code$col = paste("mt","m",as.character(agg1.code$mcc_code),as.character(agg1.code$tr_type),sep="_");
agg1.tmp      = spread(agg1.code[,c(1,6,7)],col,mm,fill=0)
agg1.tmp.id   = agg1.tmp$customer_id
agg1.tmp$customer_id = NULL

#tmp = rowSums(agg1.tmp)
#agg1.tmp      = agg1.tmp/tmp
agg1.tmp$customer_id = agg1.tmp.id
agg1.code.col = agg1.tmp


agg1.code$col = paste("mt","s",as.character(agg1.code$mcc_code),as.character(agg1.code$tr_type),sep="_");
agg1.tmp      = spread(agg1.code[,c(1,5,7)],col,ss,fill=0)
agg1.tmp      = merge(agg1.tmp,subset(agg1.cust,select=c("customer_id","nn","durDays",'lenDays')))

agg1.tmp.id   = agg1.tmp$customer_id
agg1.tmp$customer_id = NULL
agg1.tmp = agg1.tmp/agg1.tmp$lenDays
agg1.tmp$nn          = NULL
agg1.tmp$durDays     = NULL
agg1.tmp$lenDays     = NULL

#tmp = rowSums(agg1.tmp)
#agg1.tmp      = agg1.tmp/tmp
agg1.tmp$customer_id = agg1.tmp.id

agg1.code.col = merge(agg1.code.col,agg1.tmp,by='customer_id')

x=colSums(agg1.code.col[,-c(1)])

rm(agg1.tmp)


### build wday by customer_id

agg1.wday = 
  ddply(trs,.(customer_id,wday),summarise,
        nn = length(wday),
        ss = sum(amount),
        mm = mean(amount))

agg1.wday$col = paste("wd","m",as.character(agg1.wday$wday),sep="_");
agg1.wday.col = spread(agg1.wday[,c(1,5,6)],col,mm,fill=0)

agg1.wday$col = paste("wd","n",as.character(agg1.wday$wday),sep="_");
agg1.tmp      = spread(agg1.wday[,c(1,3,6)],col,nn,fill=0)
agg1.tmp      = merge(agg1.tmp,subset(agg1.cust,select=c("customer_id","nn","durDays",'lenDays')))

agg1.tmp.id   = agg1.tmp$customer_id
agg1.tmp = agg1.tmp/agg1.tmp$durDays
agg1.tmp$customer_id = agg1.tmp.id
agg1.tmp$nn      = NULL
agg1.tmp$durDays = NULL
agg1.tmp$lenDays = NULL

agg1.wday.col = merge(agg1.wday.col,agg1.tmp,by='customer_id')

x=colSums(agg1.wday.col[,-c(1)])

rm(agg1.tmp)


### build month by customer_id

require(plyr)
require(tidyr)

agg1.month = 
  ddply(trs,.(customer_id,month),summarise,
        nn = length(month),
        ss = sum(amount),
        mm = mean(amount))

agg1.month$col = paste("mo","m",as.character(agg1.month$month),sep="_");
agg1.month.col = spread(agg1.month[,c(1,5,6)],col,mm,fill=0)

agg1.month$col = paste("mo","n",as.character(agg1.month$month),sep="_");
agg1.tmp      = spread(agg1.month[,c(1,3,6)],col,nn,fill=0)
agg1.tmp      = merge(agg1.tmp,subset(agg1.cust,select=c("customer_id","nn","durDays",'lenDays')))

agg1.tmp.id   = agg1.tmp$customer_id
agg1.tmp = agg1.tmp/agg1.tmp$durDays
agg1.tmp$customer_id = agg1.tmp.id
agg1.tmp$nn      = NULL
agg1.tmp$durDays = NULL
agg1.tmp$lenDays = NULL

agg1.month.col = merge(agg1.month.col,agg1.tmp,by='customer_id')

agg1.month$col = paste("mo","s",as.character(agg1.month$month),sep="_");
agg1.tmp      = spread(agg1.month[,c(1,4,6)],col,ss,fill=0)
agg1.tmp      = merge(agg1.tmp,subset(agg1.cust,select=c("customer_id","nn","durDays",'lenDays')))

agg1.tmp.id   = agg1.tmp$customer_id
agg1.tmp = agg1.tmp/agg1.tmp$durDays
agg1.tmp$customer_id = agg1.tmp.id
agg1.tmp$nn      = NULL
agg1.tmp$durDays = NULL
agg1.tmp$lenDays = NULL

agg1.month.col = merge(agg1.month.col,agg1.tmp,by='customer_id')

x=colSums(agg1.month.col[,-c(1)])

rm(agg1.tmp)

### build mday by customer_id

require(plyr)
require(tidyr)

agg1.mday = 
  ddply(trs,.(customer_id,mday),summarise,
        nn = length(mday),
        ss = sum(amount),
        mm = mean(amount))

agg1.mday$col = paste("md","m",as.character(agg1.mday$mday),sep="_");
agg1.mday.col = spread(agg1.mday[,c(1,5,6)],col,mm,fill=0)

if (TRUE) {
  
agg1.mday$col = paste("md","n",as.character(agg1.mday$mday),sep="_");
agg1.tmp      = spread(agg1.mday[,c(1,3,6)],col,nn,fill=0)
agg1.tmp      = merge(agg1.tmp,subset(agg1.cust,select=c("customer_id","nn","durDays",'lenDays')))

agg1.tmp.id   = agg1.tmp$customer_id
agg1.tmp = agg1.tmp/agg1.tmp$durDays
agg1.tmp$customer_id = agg1.tmp.id
agg1.tmp$nn      = NULL
agg1.tmp$durDays = NULL
agg1.tmp$lenDays = NULL

agg1.mday.col = merge(agg1.mday.col,agg1.tmp,by='customer_id')

}

agg1.mday$col = paste("md","s",as.character(agg1.mday$mday),sep="_");
agg1.tmp      = spread(agg1.mday[,c(1,4,6)],col,ss,fill=0)
agg1.tmp      = merge(agg1.tmp,subset(agg1.cust,select=c("customer_id","nn","durDays",'lenDays')))

agg1.tmp.id   = agg1.tmp$customer_id
agg1.tmp = agg1.tmp/agg1.tmp$durDays
agg1.tmp$customer_id = agg1.tmp.id
agg1.tmp$nn      = NULL
agg1.tmp$durDays = NULL
agg1.tmp$lenDays = NULL

agg1.mday.col = merge(agg1.mday.col,agg1.tmp,by='customer_id')

x=colSums(agg1.mday.col[,-c(1)])

rm(agg1.tmp)

### build term_id by customer_id (440339 unique term_id ?)

if (FALSE) {
  
require(plyr)
require(tidyr)
  
  
trs.sex = merge(trs,gnd,by='customer_id',all.x=TRUE)

agg1.tmp  =
  ddply(trs.sex,.(customer_id,term_id),summarise,
        pr    = mean(gender,na.rm = TRUE),
        pr.na = mean(gender))

agg1.tmp.term = table(agg1.tmp$term_id[is.na(agg1.tmp$pr.na)])
agg1.tmp.term = names(agg1.tmp.term[agg1.tmp.term>1])
agg1.tmp.term = data.frame(term_id=agg1.tmp.term,stringsAsFactors = FALSE)

agg1.term = 
  ddply(trs.sex,.(customer_id,term_id),summarise,
        nn    = length(amount),
        ss    = sum(amount),
        mm    = mean(amount))

agg1.term.1 = merge(agg1.tmp.term,agg1.term,by='term_id')
agg1.term.1$term_id[agg1.term.1$term_id==''] = 'boba1234'
agg1.term   = agg1.term.1

rm(trs.sex,agg1.term.1,agg1.tmp,agg1.tmp.term)

agg1.term$col = paste("trt","n",as.character(agg1.term$term_id),sep="_");
agg1.term.col = spread(agg1.term[,c(2,3,6)],col,nn,fill=0)

agg1.term$col = paste("trt","s",as.character(agg1.term$term_id),sep="_");
agg1.tmp      = spread(agg1.term[,c(1,4,6)],col,ss,fill=0)
agg1.term.col = merge(agg1.term.col,agg1.tmp,by='customer_id')

agg1.term$col = paste("trt","m",as.character(agg1.term$term_id),sep="_");
agg1.tmp      = spread(agg1.term[,c(1,5,6)],col,mm,fill=0)
agg1.term.col = merge(agg1.term.col,agg1.tmp,by='customer_id')

x=colSums(agg1.term.col[,-c(1)])

rm(agg1.tmp)

}

###----------------------------------------------------------

agg1.tmp1 = merge(agg1.code.col,agg1.wday.col,by='customer_id')
agg1.tmp1 = merge(agg1.tmp1,agg1.month.col,by='customer_id')
agg1.tmp1 = merge(agg1.tmp1,agg1.mday.col, by='customer_id')
agg1.tmp  = merge(agg1.cust,agg1.tmp1,by='customer_id')
#agg1.tmp = merge(agg1.tmp, agg1.type.col,by='customer_id')

#x=grep("(_n_)|(_m_)",names(agg1.tmp))
#agg1.cor  = (cor(subset(agg1.tmp,!is.na(gender))))['gender',]
#agg1.cor  = agg1.cor[!is.na(agg1.cor)]
#agg1.cor.mt  = agg1.cor[grep("mt_",names(agg1.cor))]
#agg1.cor.not = agg1.cor[grep("mt_",names(agg1.cor),invert = TRUE)]
agg1.cor   = colSums(agg1.tmp)
#xy = c(agg1.cor.not,agg1.cor.mt[abs(agg1.cor.mt)>0.0005])
xy = agg1.cor
agg1.tmp1 = agg1.tmp[,unique(c('customer_id','gender',names(xy)))]
#agg1.tmp1$durTimes = NULL

### ---------------------------------------------------------
### using xgb boosting
###
require(xgboost)

param <- list( objective = "binary:logistic", 
               booster = "gbtree",
               colsample_bytree = 0.1, #0.5, # 0.5, #0.5, # 0.4, 
               #               subsample = 0.9,
               #               eval_metric = evalAuc,
               eval_metric = "auc",
               #tree_method = "exact",
               gamma = 7, #1.5, #0.0022, #2.25, #2.25, # 1, # 2.25, #0.05,
               #min_child_weight = 8, #6, #7, #10, #8, #5, 
               #subsample = 0.6, #0.8,
               silent    = 0)  

xy          <- grep("customer_id|gender",names(agg1.tmp1))
agg1.tmp2   <- agg1.tmp1[!is.na(agg1.tmp1$gender),-xy]

dYtrain     <- as.matrix(agg1.tmp2)
dYlabel     <- agg1.tmp1$gender[!is.na(agg1.tmp1$gender)]

agg1.tmp2   <- agg1.tmp1[is.na(agg1.tmp1$gender),-xy]
agg1.tmp3   <- agg1.tmp1$customer_id[is.na(agg1.tmp1$gender)]

dYtest      <- as.matrix(agg1.tmp2)


tmp.matrix  <- xgb.DMatrix(dYtrain,label = dYlabel);

eta <- 0.07 # 0.02 # 0.2 #0.05 # 0.02 #0.1 #0.05

set.seed(12341234)
history = xgb.cv(tmp.matrix, 
                 nfold = 10, #10, #5, #8, #25, # 10, 
                 #folds = folds,
                 eta=eta, 
                 #max_depth=max_depth, 
                 params =param, 
                 #nrounds  =  1000,
                 nrounds =  ifelse(eta<0.035,4000,1000), 
                 #                 metrics = "auc", 
                 maximize = TRUE, #maxima, 
                 stratified = TRUE,
                 prediction=TRUE,
                 early.stop.round = ifelse(eta<0.035,250, 50), 
                 print.every.n = 25);

#     ------------ look results and select best result in history

if (!is.null(history$pred)) {
  history.pred = history$pred
  history      = history$dt
}

max(history$test.auc.mean);
h_max=which.max(history$test.auc.mean);
print(c(h_max,"-->",history$test.auc.mean[h_max],history$test.auc.std[h_max],history$train.auc.mean[h_max],history$train.auc.std[h_max]));
plot(history$test.auc.mean)
plot(history$test.auc.mean[history$test.auc.mean>0.875])
#plot(history$test.auc.std[history$test.auc.std<0.015])

#---------------------------------------------------------

##?set.seed(1234)
bst = xgb.train (     
  params =param, 
  tmp.matrix,
  eta=eta, 
  #max_depth=max_depth, 
  nrounds = h_max+400, # 1500, # ifelse(eta<0.035,3000,800), 
  verbose=1, 
  print.every.n = 25,
  #watchlist=list(eval = dtest, train = tmp.matrix),
  watchlist=list(eval = tmp.matrix),
  #watchlist=list(train = tmp.matrix),
  metrics = "auc", 
  stratified = TRUE,
  early.stop.round = 50,
  maximize = TRUE)


pre.train        = predict(bst,tmp.matrix);
pre.test         = predict(bst,dYtest);

#pre.test         = agg.customer.2$pr.term

#---------------------------------------------------------
#agg.customer.glm=glm(gender~.-customer_id,data=agg.customer.1,family='binomial')
#summary(agg.customer.glm)
#pre.train        = predict(agg.customer.glm,type='response')

hist(pre.train,breaks = 100)
colAUC(pre.train,agg1.tmp1$gender[!is.na(agg1.tmp1$gender)])

#pre.test         = predict(agg.customer.glm,type='response',newdata = agg.customer.2)
hist(pre.test,breaks = 100)

#--------------------------------------------------------

xygg = agg1.tmp3
xypp = pre.test

plot(sort(xypp))

xy1res  = list('customer_id'=xygg,
               "gender"=sprintf("%12.10f",xypp))
nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task1-",nStep,'.csv',sep='') 
write.csv(xy1res,file=outfile,quote=FALSE,row.names=FALSE)


#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
