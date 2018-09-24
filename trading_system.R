library("TTR")
library("forecast")
library(useful)
library(readr)
library(prophet)
library(zoo)
library(lubridate)
library(zoo)

FinancialData <- read_csv("data/FinancialData.csv")
financial_series<-ts(FinancialData,frequency =365.25/7,start = c(2006,1))
data<-data.frame(date_decimal(time(financial_series)[1:606]),FinancialData$`SG/D`)
colnames(data)<-c("ds","y")

### prophet   ####

data_train<-data[1:502,]
data_test<-data[503:606,]
m<-prophet(data_train)
future <- make_future_dataframe(m,periods = 104,freq = 'week')
forecast <- predict(m, future)
prophet_plot_components(m, forecast)


###
preds_sg_d <- data.frame()
look_ahead=5
for (i in c(0:104)){
  data_train<-data[1:(502+i),]
  m<-prophet(data_train)
  future <- make_future_dataframe(m,periods = look_ahead,freq = 'week')  
  forecast <- predict(m, future)
  preds<-forecast$yhat[(503+i):(503+i+look_ahead-1)]
  preds_sg_d[1:5,paste('wk',toString(503+i))] <- preds
}
pred_sg_d=c(1:104)
for(i in c(1:length(preds_sg_d))){
  pred_sg_d[i]=preds_sg_d[[i]][1]
}
sum(abs(pred_sg_d[1:104]-data$y[503:606]))/104*100


pred_sg_d_time_series<-ts(pred_sg_d,frequency =365.25/7,start = c(2015,34))
sg_d_time_series<-ts(financial_series[1:606,9],frequency = 365.25/7,start = c(2006,1))
ts.plot(sg_d_time_series,pred_sg_d_time_series,col=c("red","blue"))




#### vicky  ######
look_ahead=10
preds_sg_d <- read_csv("vicky/pred_sg_xch.csv")
preds_pound_d <- read_csv("vicky/pred_pound_xch.csv")
pred_sg_d=data.frame()
for(i in c(1:length(preds_sg_d))){
  pred_sg_d[1:look_ahead,paste('wk',toString(503+i-1))]=preds_sg_d[[i]][i:(i+look_ahead-1)]
}
pred_pound_d=data.frame()
for(i in c(1:length(preds_pound_d))){
  pred_pound_d[1:look_ahead,paste('wk',toString(503+i-1))]=preds_pound_d[[i]][i:(i+look_ahead-1)]
}
sum(abs(pred_sg_d[1,1:104]-data$y[503:606]))/104*100
sum(abs(pred_pound_d[1,1:104]-FinancialData$`P/D`[503:606]))/104*100

pred_pound_d[is.na(pred_pound_d)] <- 1
pred_sg_d[is.na(pred_sg_d)] <- 1


write.csv(x =pred_pound_d,file = './data/p_d.csv',row.names = FALSE )
write.csv(x =pred_sg_d,file = './data/sg_d.csv',row.names = FALSE )

write.csv(x =t(pred_pound_d[1,1:104]),file = 'p_d.csv',row.names = FALSE )
write.csv(x =t(pred_sg_d[1,1:104]),file = 'sg_d.csv',row.names = FALSE )


