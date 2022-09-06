#same function as the forecast_fun, but reform the structure to invlude the national forecast too, as the first line.
#same change can be used in the log version too.

forecast_fun_withnational = function(train_ts, end_year, end_month, period, begin_month){
  temp = c(train_ts[52,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  state_data=state_raw[1:(length(state_raw)-begin_month)]
  ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(ts_state_data,seasonal = TRUE)
  prediction = forecast(autoresult, h=period)
  prediction = t(t(as.vector(prediction$mean[1:length(prediction$mean)])))
  for(i in 1:51){
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


#generate S matrix needed by reconciliation

average_weight = t(as.matrix(rowMeans(percentage[-c(1)]), ncol=1))
S = matrix(0,52,51)
S[1,] = average_weight
for (i in 2:52){
  S[i, i - 1] = average_weight[i-1] 
}