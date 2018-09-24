library(xlsx)
library(tidyr)
library(Metrics)
library(smooth)
library(TTR)
library(tseries)

setwd('D:\\KE Notes\\Developing intelligent systems for BA\\Day3\\2a')
data <- read.csv("FinancialData.csv")
# creating a new dataframe using week, sg->d, p->d only
new_data <- data[c('Week','SG.D','P.D')]
# creating sg_ts and pound_ts using the exchange rates
sg_ts <- ts(new_data$SG.D, start = c(2006,1), frequency = 52) 
pound_ts <- ts(new_data$P.D, start = c(2006,1), frequency = 52) 
# plotting 
ts.plot(sg_ts,pound_ts,col=c("blue","red"))

# creating the initial train test , leaving 104 observations for test
sg_train <- window(sg_ts,start=c(2006,1),end=c(2015,34))
pound_train <- window(pound_ts,start=c(2006,1),end=c(2015,34))
pound_test <- window(pound_ts,start=c(2015,35))
ts.plot(sg_train,pound_train,col=c("blue","red"))

# triple exponential smoothing
es_tr_model <- HoltWinters(pound_train)
es_tr_pred <- predict(es_tr_model, 104, prediction.interval = FALSE)
ts.plot(pound_ts,es_tr_pred,col=c("red","blue"))

pred_sg_xch <- data.frame()
pred_pound_xch <- data.frame()
for (i in c(0:103)){
  end_m = (34 + i)%%52
  end_y = 2015 + (34 + i)%/%52
  if (end_m == 0){
    end_m = 52
    end_y = end_y-1
  }
  loop_sg_train <- window(sg_ts,start=c(2006,1),end=c(end_y,end_m))
  loop_pound_train <- window(pound_ts,start=c(2006,1),end=c(end_y,end_m))
  estr_sg_model <- HoltWinters(loop_sg_train)
  estr_pound_model <- HoltWinters(loop_pound_train)
  estr_sg_pred <- predict(estr_sg_model, 104-i)
  estr_pound_pred <- predict(estr_pound_model, 104-i)
  #print(paste(502+i,502+1+i,es_tr_pred[1]))
  pred_sg_xch[(1+i):104,paste('wk',toString(503+i))] <- estr_sg_pred
  pred_pound_xch[(1+i):104,paste('wk',toString(503+i))] <- estr_pound_pred
}


write.csv(x =pred_sg_xch,file = 'pred_sg_xch.csv',row.names = FALSE )
write.csv(x =pred_pound_xch,file = 'pred_pound_xch.csv',row.names = FALSE )

pred_sg_ts <- ts(pred_xch$SG.D, start = c(2015,35), frequency = 52)
pred_pound_ts <- ts(pred_xch$P.D, start = c(2015,35), frequency = 52)

ts.plot(sg_ts,pound_ts,pred_sg_ts,pred_pound_ts,col=c("red","blue","green","black"))


mape(data[503:606,'P.D'],estr_pound_pred)

