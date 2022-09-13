usadata = read.csv(file = '/Users/caoyuqi/Documents/unimelb/MAST90106/Data-scence-project-main/data/usa_unemployment_nsa.csv')
labour = read.csv(file = '/Users/caoyuqi/Documents/unimelb/MAST90106/Data-scence-project-main/data/labour_backup.csv')
percentage = read.csv(file = '/Users/caoyuqi/Documents/unimelb/MAST90106/Data-scence-project-main/data/rate.csv')
library(forecast)
library(tseries)
library(Metrics)

#Alabama
temp = c(usadata[1,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

#Alaska: Arima(ts_state_data,order=c(2,0,2),seasonal=c(2,1,2))


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
t=Arima(ts_state_data,order=c(0,1,3),seasonal=c(0,1,2))
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

#Alaska: Arima(ts_state_data,order=c(0,1,3),seasonal=c(0,1,2))

#Arizona


temp = c(usadata[3,])
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
t1=Arima(ts_state_data,order=c(4,1,0),seasonal=c(3,1,0))
checkresiduals(t1)

t2=Arima(ts_state_data,order=c(5,1,0),seasonal=c(1,1,1))
checkresiduals(t2)
#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t2,h=24)


temp = c(usadata[3,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Arizona: Arima(ts_state_data,order=c(5,1,0),seasonal=c(1,1,1))


#Arkansas


temp = c(usadata[4,])
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
t1=Arima(ts_state_data,order=c(2,1,1),seasonal=c(0,1,1))
t1$aicc
checkresiduals(t1)

t2=Arima(ts_state_data,order=c(1,1,1),seasonal=c(0,1,1))
t2$aicc
checkresiduals(t2)

t3=Arima(ts_state_data,order=c(0,1,1),seasonal=c(0,1,1))
t3$aicc
checkresiduals(t3)

t4=Arima(ts_state_data,order=c(3,1,1),seasonal=c(0,1,1))
t4$aicc
checkresiduals(t4)

t5=Arima(ts_state_data,order=c(1,1,1),seasonal=c(1,1,1))
t5$aicc
checkresiduals(t5)
#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t2,h=24)


temp = c(usadata[4,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Arkasas: Arima(ts_state_data,order=c(1,1,0),seasonal=c(0,1,1))

#California


temp = c(usadata[5,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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
#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(autoresult,h=24)


temp = c(usadata[5,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#California: Arima(ts_state_data,order=c(2,0,2),seasonal=c(2,1,1))

#Colorado

temp = c(usadata[6,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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

t1=Arima(ts_state_data,order=c(1,1,2),seasonal=c(0,1,1))
t1$aicc
checkresiduals(t1)

t2=Arima(ts_state_data,order=c(1,1,2),seasonal=c(1,1,1))
t2$aicc
checkresiduals(t2)

#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t2,h=24)


temp = c(usadata[6,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Colorado: Arima(ts_state_data,order=c(1,1,2),seasonal=c(0,1,1))

#Connecticut

temp = c(usadata[7,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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

t1=Arima(ts_state_data,order=c(2,0,2),seasonal=c(0,1,1))
t1$aicc
checkresiduals(t1)


#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t1,h=24)


temp = c(usadata[7,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Connecticut: Arima(ts_state_data,order=c(2,0,2),seasonal=c(0,1,1))

#Delaware

temp = c(usadata[8,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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

t1=Arima(ts_state_data,order=c(0,1,5),seasonal=c(0,1,1))
t1$aicc
checkresiduals(t1)

t2=Arima(ts_state_data,order=c(0,1,3),seasonal=c(0,1,1))
t2$aicc
checkresiduals(t2)

t3=Arima(ts_state_data,order=c(0,1,4),seasonal=c(0,1,1))
t3$aicc
checkresiduals(t3)

t4=Arima(ts_state_data,order=c(1,1,2),seasonal=c(1,1,1))
t4$aicc
checkresiduals(t4)

#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t4,h=24)


temp = c(usadata[8,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Delaware: Arima(ts_state_data,order=c(1,1,2),seasonal=c(1,1,1))

#District of Columbia


temp = c(usadata[9,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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

t1=Arima(ts_state_data,order=c(2,0,2),seasonal=c(1,1,1))
t1$aicc
checkresiduals(t1)

#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t1,h=24)


temp = c(usadata[9,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#District of Columbia: Arima(ts_state_data,order=c(3,0,2),seasonal=c(2,1,1))

#Florida

temp = c(usadata[10,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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

t1=Arima(ts_state_data,order=c(2,0,3),seasonal=c(0,1,1))
t1$aicc
checkresiduals(t1)

#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t1,h=24)


temp = c(usadata[10,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Florida: Arima(ts_state_data,order=c(2,0,3),seasonal=c(0,1,1))

#Georgia

temp = c(usadata[11,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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

t1=Arima(ts_state_data,order=c(2,0,3),seasonal=c(0,1,1))
t1$aicc
checkresiduals(t1)

#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(autoresult,h=24)


temp = c(usadata[11,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Georgia: Arima(ts_state_data,order=c(2,0,2),seasonal=c(0,1,1))

#Hawaii

temp = c(usadata[12,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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

t1=Arima(ts_state_data,order=c(2,0,3),seasonal=c(0,1,1))
t1$aicc
checkresiduals(t1)

#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(autoresult,h=24)


temp = c(usadata[12,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Hawaii: Arima(ts_state_data,order=c(2,0,2),seasonal=c(2,1,1))

#Idaho

temp = c(usadata[13,])
val = temp[2:length(temp)]
state_name = unlist(temp[1])
state_raw = as.vector(unlist(val))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
autoresult$aicc
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

t1=Arima(ts_state_data,order=c(2,1,1),seasonal=c(2,1,1))
t1$aicc
checkresiduals(t1)

t2=Arima(ts_state_data,order=c(2,1,2),seasonal=c(2,1,1))
t2$aicc
checkresiduals(t2)

#p-value > 0.05 means significant 

#check AICc
#check RMSE

fore = forecast(t2,h=24)


temp = c(usadata[13,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))

rmse1=rmse(tnew, tnewnew)

#Idaho: Arima(ts_state_data,order=c(2,1,1),seasonal=c(2,1,1))





