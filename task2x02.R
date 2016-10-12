require(dplyr)
require(tidyr)
require(caTools)
require(xgboost)

#
# get minus transaction
#

trsm = subset(trs,trs$amount<0.0)
dsm  = as.numeric(ds[trs$amount<0.0])
trsm$day = dsm
dsmw = dsm %% 7
