usadata = read.csv(file = 'F:/UNIMELB/Graduate/DSProject/Data-scence-project/data/usa_unemployment_nsa.csv')
labour = read.csv(file = 'F:/UNIMELB/Graduate/DSProject/Data-scence-project/data/labour_backup.csv')
percentage = read.csv(file = 'F:/UNIMELB/Graduate/DSProject/Data-scence-project/data/rate.csv')
library(forecast)

#
#here should be forecast funcation, it should give an output matrix with 50col(51 if national is included later)
#and 12(or whatever time period want to forecat)
#
#benchmark using,current using arima.auto, planning to add argument of method or more
forecast_fun = function(train_ts){
  temp = c(train_ts[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  #state_data=state_raw[1:(length(state_raw)-39)]
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
  ts_state_data
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(ts_state_data,seasonal = TRUE)
  prediction = forecast(autoresult, h=12)
  prediction = t(t(as.vector(prediction$mean[1:length(prediction$mean)])))
  for(i in 2:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    #state_data=state_raw[1:(length(state_raw)-3)]
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
    
    ts_state_data_log=log(ts_state_data) #log transformation
    #above generate ts of 1976-2018 for each state
    #autoresult=auto.arima(ts_state_data,seasonal = TRUE)
    autoresult=auto.arima(ts_state_data_log,seasonal = TRUE)
    fore_res = forecast(autoresult, h=12)
    fore_res = t(t(as.vector(fore_res$mean[1:length(fore_res$mean)])))
    fore_res = exp(fore_res) #exp transformation
    prediction = cbind(prediction, fore_res)
    
  }
  
  return(prediction)
}

forecast_res = forecast_fun(usadata)

compare=function(end_year,end_month,forecast){
  xx=as.matrix(forecast)
  yy=matrix(percentage[,(2+(end_year-1976)*12+end_month)])
  result=as.matrix(xx%*%yy)
  data=as.numeric(usadata[52,c((2+(end_year-1976)*12+end_month):(2+(end_year-1976)*12+11+end_month))])
  diff=result-matrix(data)
  return(diff) 
}

final_diff=compare(2018,12,forecast_res)
plot(final_diff, type="l",main="(With log transformation)difference between prediction and real, in 12 month",xlab="month", ylab="difference in percentage")
#pred�?4.589013 real�?4.5 time:2018 Jan
#pred�?3.533 real�?3.7 time:2018 dec
#pred�?4.373029 real�?4.4 time:2019 jan
#pred�?4.323978 real�?4.1 time:2019 feb
#pred�?3.87493 real�?3.9 time:2019 march
recon_s = c(percentage[,556])#use the newest for now and with only 2 level, it is very simple
#national_forecast = forecast_res%*%recon_s

#doing the same bussiness using another function, theoritically they shall be the same.


#to avoid using covid data, whatever period want ot test, please ensuretest dataset ends before 12/2019
#for both of two forecast function below, 
#train_ts: is data needed to train and test(assume to be usada here)
#end_year, end_month: year and month when training set ends(yyyy/mm)
#period:how many periods you want to forecast, 1 period is 1 month
#begin_month:how many month are there between end of the trainning set(defined by end_year and end_month), and end of dataset(03/2022), shoud be 27+period
forecast_fun = function(train_ts, end_year, end_month, period, begin_month){
  temp = c(train_ts[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  state_data=state_raw[1:(length(state_raw)-begin_month)]
  ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(ts_state_data,seasonal = TRUE)
  prediction = forecast(autoresult, h=period)
  prediction = t(t(as.vector(prediction$mean[1:length(prediction$mean)])))
  for(i in 2:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    state_data=state_raw[1:(length(state_raw)-begin_month)]
    ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
    #above generate ts of 1976-2018 for each state
    autoresult=auto.arima(ts_state_data,seasonal = TRUE)
    fore_res = forecast(autoresult, h=period)
    fore_res = t(t(as.vector(fore_res$mean[1:length(fore_res$mean)])))
    prediction = cbind(prediction, fore_res)
  }
  return(prediction)
}

forecast_fun_log = function(train_ts, end_year, end_month, period, begin_month){
  temp = c(train_ts[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  state_data=state_raw[1:(length(state_raw)-begin_month)]
  ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(log(ts_state_data),seasonal = TRUE)
  prediction = forecast(autoresult, h=period)
  prediction = t(t(as.vector(exp(prediction$mean[1:length(prediction$mean)]))))
  for(i in 2:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    state_data=state_raw[1:(length(state_raw)-begin_month)]
    ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
    #above generate ts of 1976-2018 for each state
    autoresult=auto.arima(log(ts_state_data),seasonal = TRUE)
    fore_res = forecast(autoresult, h=period)
    fore_res = t(t(as.vector(exp(fore_res$mean[1:length(fore_res$mean)]))))
    prediction = cbind(prediction, fore_res)
  }
  return(prediction)
}

#this function assume using usada as real value
#end_year and end_month: end timepoint of the TRAINNING set
#forecast: the forecast generate, the period of this should fits the next argument
#period:how many period(s), in month your forecast are in
compare=function(end_year,end_month,forecast, period){
  xx=as.matrix(forecast)
  yy=matrix(percentage[,(2+(end_year-1976)*12+end_month)])
  result=as.matrix(xx%*%yy)
  data=as.numeric(usadata[52,c((2+(end_year-1976)*12+end_month):(2+(end_year-1976)*12+11+period+end_month))])
  diff=result-matrix(data)
  return(diff) 
}

forecast_res = forecast_fun(usadata, 2017,12,24, 51)
forecast_res_log = forecast_fun_log(usadata, 2017,12,24, 51)

final_diff = compare(2017,12,forecast_res)
final_diff_log=compare(2017,12,forecast_res_log)
plot(final_diff, type="l",main="difference between prediction and real",xlab="month", ylab="difference in percentage",col="red", ylim = c(-0.3,1))
lines(final_diff_log,col = "blue")
abline(h=0)
legend(0.5, legend=c("original", "log transfered"),col=c("red", "blue"), lty=1:2, cex=0.8)

#check variance of difference/error
var(final_diff)
var(final_diff_log)