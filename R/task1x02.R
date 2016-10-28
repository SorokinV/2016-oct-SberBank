require(plyr)
#require(tidyr)
require(caTools)
require(xgboost)


#-------------------------------------------------

str(trs)

trs.sex = merge(trs,gnd,by='customer_id',all.x=TRUE)

agg1.code.sex = 
  ddply(trs.sex,.(mcc_code,tr_type),summarise,
        pr   =mean(gender,na.rm = TRUE),
        pr.na=mean(gender))

agg1.code.sex = subset(agg1.code.sex,is.na(agg1.code.sex$pr.na))
agg1.code.sex$pr.na = NULL

if (FALSE) {
agg1.type.sex = 
  ddply(trs.sex,.(tr_type),summarise,
        pr   =mean(gender,na.rm = TRUE),
        pr.na=mean(gender))

agg1.type.sex = subset(agg1.type.sex,is.na(agg1.type.sex$pr.na))
agg1.type.sex$pr.na = NULL
}

agg1.term.sex = 
  ddply(trs.sex,.(term_id),summarise,
        pr   =mean(gender,na.rm = TRUE),
        pr.na=mean(gender))

agg1.term.sex = subset(agg1.term.sex,is.na(agg1.term.sex$pr.na))
agg1.term.sex$pr.na = NULL

trs.sex = merge(trs.sex,agg1.code.sex,by=c('mcc_code','tr_type'),all.x=TRUE)
#trs.sex = merge(trs.sex,agg1.type.sex,by='tr_type',all.x=TRUE)
trs.sex = merge(trs.sex,agg1.term.sex,by='term_id',all.x=TRUE)
#trs.sex = merge(trs.sex,gnd,by='customer_id',all.x=TRUE)

agg1.customer = 
  ddply(trs.sex,.(customer_id),summarise,
        pr.code = mean(pr.x,na.rm = TRUE),
#        pr.type = mean(pr.y,na.rm = TRUE),
        pr.term = mean(pr.y,na.rm = TRUE))

agg1.customer.sex = merge(agg1.customer,gnd,by='customer_id',all.x=TRUE)

agg1.customer.1   = subset(agg1.customer.sex,!is.na(agg1.customer.sex$gender))

agg1.customer.2   = subset(agg1.customer.sex, is.na(agg1.customer.sex$gender))
summary(agg1.customer.2)
agg1.customer.1$pr.term[is.na(agg1.customer.1$pr.term)]=0.5
agg1.customer.2$pr.term[is.na(agg1.customer.2$pr.term)]=0.5


#--------------------------------------------------------

param <- list( objective = "binary:logistic", 
               booster = "gbtree",
               #colsample_bytree = 0.5, # 0.5, #0.5, # 0.4, 
               #               subsample = 0.9,
               #               eval_metric = evalAuc,
               eval_metric = "auc",
               #tree_method = "exact",
               #gamma = 1.5, #0.0022, #2.25, #2.25, # 1, # 2.25, #0.05,
               #min_child_weight = 8, #6, #7, #10, #8, #5, 
               #subsample = 0.6, #0.8,
               silent    = 0)  


dYtrain     <- as.matrix(agg1.customer.1[,-grep("customer_id|gender|pr.type",names(agg1.customer.1))])
dYlabel     <- agg1.customer.1$gender
dYtest      <- as.matrix(agg1.customer.2[,-grep("customer_id|gender|pr.type",names(agg1.customer.2))])


tmp.matrix  <- xgb.DMatrix(dYtrain,label = dYlabel);

eta <- 0.02 #0.1 #0.05

#agg1.w.code.sex.glm = glm(gender~.-customer_id,xys,family='binomial')
#xyp = predict(agg1.w.code.sex.glm,type='response')

history = xgb.cv(tmp.matrix, 
                 nfold = 10, #5, #8, #25, # 10, 
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
plot(history$test.auc.mean[history$test.auc.mean>0.875])
#plot(history$test.auc.std[history$test.auc.std<0.015])

#---------------------------------------------------------

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

#pre.test         = agg1.customer.2$pr.term

#---------------------------------------------------------
#agg1.customer.glm=glm(gender~.-customer_id,data=agg1.customer.1,family='binomial')
#summary(agg1.customer.glm)
#pre.train        = predict(agg1.customer.glm,type='response')

hist(pre.train,breaks = 40)
colAUC(pre.train,agg1.customer.1$gender)
colAUC(agg1.customer.1$pr.term,agg1.customer.1$gender)

#pre.test         = predict(agg1.customer.glm,type='response',newdata = agg1.customer.2)
hist(pre.test,breaks = 40)

task1res= list('customer_id'=agg1.customer.2$customer_id,
               "gender"=sprintf("%12.10f",pre.test))
nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task1-",nStep,'.csv',sep='') 
write.csv(task1res,file=outfile,quote=FALSE,row.names=FALSE)

