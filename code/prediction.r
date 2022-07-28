#please first enable library here, like forecast

usadata = read.csv(file = 'F:/UNIMELB/Graduate/DSProject/Data-scence-project/data/usa_unemployment_nsa.csv')
labour = read.csv(file = 'F:/UNIMELB/Graduate/DSProject/Data-scence-project/data/labour_backup.csv')
percentage = read.csv(file = 'F:/UNIMELB/Graduate/DSProject/Data-scence-project/data/rate.csv')


#
#here should be forecast function, it should give an output matrix with 50col(51 if national is included later)
#and 12(or whatever time period want to forecast)
#

#benchmark using,current using arima.auto, planning to add argument of method or more
forecast_fun = function(train_ts){
  temp = c(train_ts[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  state_data=state_raw[1:(length(state_raw)-39)]
  ts_state_data=ts(state_data,start=c(1976, 1), end=c(2018, 12), frequency=12)
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(ts_state_data,seasonal = TRUE)
  prediction = forecast(autoresult, h=12)
  prediction = t(t(as.vector(prediction$mean[1:length(prediction$mean)])))
  for(i in 2:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    state_data=state_raw[1:(length(state_raw)-39)]
    ts_state_data=ts(state_data,start=c(1976, 1), end=c(2018, 12), frequency=12)
    #above generate ts of 1976-2018 for each state
    autoresult=auto.arima(ts_state_data,seasonal = TRUE)
    fore_res = forecast(autoresult, h=12)
    fore_res = t(t(as.vector(fore_res$mean[1:length(fore_res$mean)])))
    prediction = cbind(prediction, fore_res)
  }
  return(prediction)
}

forecast_res = forecast(xxxx)
recon_s = c(percentage[,556])#use the newest for now and with only 2 level, it is very simple
national_forecast = forecast_res%*%recon_s
