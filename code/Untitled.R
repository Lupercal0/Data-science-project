usadata = read.csv(file = '/Users/caoyuqi/Documents/unimelb/MAST90106/Data-scence-project-main/data/usa_unemployment_nsa.csv')
labour = read.csv(file = '/Users/caoyuqi/Documents/unimelb/MAST90106/Data-scence-project-main/data/labour_backup.csv')
percentage = read.csv(file = '/Users/caoyuqi/Documents/unimelb/MAST90106/Data-scence-project-main/data/rate.csv')
library(forecast)
library(tseries)
library(Metrics)

#Alaska
temp = c(usadata[2,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)


acf(ts_state_data)
pacf(ts_state_data)

adf.test(ts_state_data)
#d=1

d = diff(ts_state_data, differences=1)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

#try orders
t=arima(ts_state_data,order=c(0,1,3),seasonal=c(0,1,2))
checkresiduals(t)
#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t,h=24)

temp = c(usadata[2,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse(tnew, tnewnew)












