require(plyr)
require(tidyr)
require(caTools)
require(xgboost)

#--------------------------------------------------------

trs = read.csv("./Data/transactions.csv",stringsAsFactors = FALSE)
str(trs)

#--------------------------------------------------------

regexpr("[0-9]+","00 00:00:00")
dd = regexpr("[0-9]+",trs$tr_datetime)
ds = substr(trs$tr_datetime,dd,dd+attr(dd,"match.length")-1); head(ds)
tds= table(ds)

rm(dd)

#---------------------------------------------------------

xx <- as.Date("2004-01-01","%Y-%m-%d"); xx
x0 <- xx-153

#---------------------------------------------------------

agg.code = 
  ddply(trs,.(customer_id,mcc_code),summarise,
        N=length(mcc_code),
        ssum=sum(amount),
        mmean=mean(amount),
        ssd=sd(amount))

agg.w.code   = spread(agg.code[,c(1,2,3)],mcc_code,N,fill=0)
mcc.names    = names(agg.w.code)
mcc.names    = c("customer_id",paste('a',mcc.names[2:length(mcc.names)],sep=""))
names(agg.w.code) = mcc.names


x=rowSums(agg.w.code[,-c(1)]); head(x)
y=agg.w.code[,-c(1)]/x; head(y)
xy  = y; xy$customer_id=agg.w.code$customer_id; head(xy)

#--------------------------------------------------------

agg.w.code.2 = spread(agg.code[,c(1,2,5)],mcc_code,mmean,fill=0)
mcc.names    = names(agg.w.code.2)
mcc.names    = c("customer_id",paste('b',mcc.names[2:length(mcc.names)],sep=""))
names(agg.w.code.2) = mcc.names
agg.w.code.2$customer_id = NULL

xy = cbind(xy,agg.w.code.2)

agg.w.code.2 = spread(agg.code[,c(1,2,4)],mcc_code,mmean,fill=0)
mcc.names    = names(agg.w.code.2)
mcc.names    = c("customer_id",paste('c',mcc.names[2:length(mcc.names)],sep=""))
names(agg.w.code.2) = mcc.names
agg.w.code.2$customer_id = NULL

xy = cbind(xy,agg.w.code.2)


xys = merge(xy,gnd,by='customer_id'); head(xys)

#--------------------------------------------------------

param <- list( objective = "binary:logistic", 
               booster = "gbtree",
               #colsample_bytree = 0.5, # 0.5, #0.5, # 0.4, 
               #               subsample = 0.9,
               #               eval_metric = evalAuc,
               eval_metric = "auc",
               #tree_method = "exact",
               #gamma = 0.0022, #2.25, #2.25, # 1, # 2.25, #0.05,
               #min_child_weight = 5, 
               #subsample = 0.6, #0.8,
               silent    = 0)  


dYtrain     <- as.matrix(xys[,-grep("customer_id|gender",names(xys))])
dYlabel     <- xys$gender

tmp.matrix  <- xgb.DMatrix(dYtrain,label = dYlabel);

eta <- 0.05

#agg.w.code.sex.glm = glm(gender~.-customer_id,xys,family='binomial')
#xyp = predict(agg.w.code.sex.glm,type='response')

history = xgb.cv(tmp.matrix, 
                 nfold = 8, #25, # 10, 
                 #folds = folds,
                 eta=eta, 
                 #max_depth=max_depth, 
                 params =param, 
                 nrounds  =  2400,
                 #nrounds =  ifelse(eta<0.035,3000,600), 
                 #                 metrics = "auc", 
                 maximize = TRUE, #maxima, 
                 stratified = TRUE,
                 prediction=TRUE,
                 early.stop.round = 50, 
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
plot(history$test.auc.mean[history$test.auc.mean>0.84])
#plot(history$test.auc.std[history$test.auc.std<0.015])

#--------------------------------------------------------

xyss= merge(xy,gnd,by='customer_id',all.x = TRUE);
#xypp= predict(agg.w.code.sex.glm,newdata=xyss, type='response')

dYtest     <- as.matrix(xyss[,-c(1,length(xyss))])

bst = xgb.train (     
  params =param, 
  tmp.matrix,
  eta=eta, 
  #max_depth=max_depth, 
  nrounds = h_max, # 1500, # ifelse(eta<0.035,3000,800), 
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

AUC = colAUC(pre.train,dYlabel);
print (sprintf("%s work ended:-----> AUC=%10.8f",format(Sys.time(),"%Y-%m-%d %H:%M:%S"),AUC));

xypp = pre.test

#--------------------------------------------------------

xypp[!is.na(xyss$gender)]=xyss$gender[!is.na(xyss$gender)]

xygg = xy$customer_id[is.na(xyss$gender)]
xypp = xypp[is.na(xyss$gender)]

xy1res  = list('customer_id'=xygg,
               "gender"=sprintf("%12.10f",xypp))
nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task1-",nStep,'.csv',sep='') 
write.csv(xy1res,file=outfile,quote=FALSE,row.names=FALSE)


#--------------------------------------------------------

rm(x,y,xy,xys)
rm(xypp,xyss)
