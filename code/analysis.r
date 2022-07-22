
usadata = read.csv(file = 'path')
labour = read.csv(file = 'path')
percentage = read.csv(file = 'path')
library(forecast)




temp = c(usadata[1,])
temp = temp[2:length(temp)]

#statewise model analysis,just put whatever state index you want in the for loop
for(i in 1:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    #state_data=state_raw[1:(length(state_raw)-3)]
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
    #above generate ts of 1976-2018 for each state
    autoresult = auto.arima(ts_state_data,seasonal = TRUE)
    summary(autoresult)
  }