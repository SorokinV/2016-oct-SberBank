# build grid for mcc-code and day

df.temp.0 = data.frame("day"=c(min(agg.mcc2$day):max(agg.mcc2$day)))
iCount    = 0
for (i in sort(unique(agg.mcc2$mcc_code))) {
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

agg.mcc2.ts = merge(df[c(2,1)],agg.mcc2[c(1,2,3)],all.x=TRUE); 
agg.mcc2.ts$ssum[is.na(agg.mcc2.ts$ssum)]=0.0; str(agg.mcc2.ts)

rm(df)
