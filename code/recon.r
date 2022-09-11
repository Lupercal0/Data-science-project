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


#generate S matrix needed by reconciliation, which include national as well
average_weight = t(as.matrix(rowMeans(percentage[-c(1)]), ncol=1))
filler = matrix(1,1,51)
#generate summation matrix, to fit the relation. this matrix only represent relation no weight
S = matrix(0,52,51)
S[1,]=filler
for (i in 2:52){
  S[i,i-1] = 1
}
#generate G matrix that represent the weight, and GS = I
for(i in 1:51){
  temp = matrix(-average_weight[i], 1,52)
  G[i,] = temp
  G[i,1] = average_weight[i]
  G[i,i+1] = 1-average_weight[i]
}

prediction = forecast_fun_withnational(usadata, 2018, 12, 12, 39)

recon = S%*%G%*%t(prediction)


#print the plot of error , comparing three methods(just weighted sum ,recon and regression result)
data=as.numeric(usadata[52,c((2+(2018-1976)*12+12):(2+(2018-1976)*12+11+12))])
diff=recon[1]-matrix(data)

original = forecast_fun(usadata, 12)
original_weight = compare_real(2018,12,original)
plot(diff, type="l",main="difference between prediction and real, by reconcilation",xlab="month", ylab="difference in percentage",col="red", ylim = c(0,1.4))
lines(original_weight, col = "green")
lines(final_diff, col = "blue")
legend("topleft", c("with recon", "with easy weight", "with regression"),lty = c(1,1),col = c("red", "green", "blue"))