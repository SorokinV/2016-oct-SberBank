x=rowSums(agg.w.code[,-c(1)]); head(x)
y=agg.w.code[,-c(1)]/x; head(y)
xy  = y; xy$customer_id=agg.w.code$customer_id; head(xy)
xys = merge(xy,gnd,by='customer_id'); head(xys)
agg.w.code.sex.glm = glm(gender~.-customer_id,xys,family='binomial')
xyp = predict(agg.w.code.sex.glm,type='response')

xyss= merge(xy,gnd,by='customer_id',all.x = TRUE);
xypp= predict(agg.w.code.sex.glm,newdata=xyss, type='response')


xypp[!is.na(xyss$gender)]=xyss$gender[!is.na(xyss$gender)]

xygg = xy$customer_id[is.na(xyss$gender)]
xypp = xypp[is.na(xyss$gender)]

xy1res  = list('customer_id'=xygg,
               "gender"=sprintf("%12.10f",xypp))
nStep   = strftime(Sys.time(),"%Y%m%d-%H%M%S")
outfile = paste("./Result/task1-",nStep,'.csv',sep='') 
write.csv(xy1res,file=outfile,quote=FALSE,row.names=FALSE)


/* don't calculate

x=rowSums(agg.w.type[,-c(1)]); head(x)
y=agg.w.type[,-c(1)]/x; head(y)
xy  = y; xy$customer_id=agg.w.type$customer_id; head(xy)
xys = merge(xy,gnd,by='customer_id'); head(xys)
agg.w.type.sex.glm = glm(gender~.-customer_id,xys,family='binomial')

*/



  
rm(x,y,xy,xys)
rm(xypp,xyss)
