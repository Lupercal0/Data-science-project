# get packages
library(forecast)
library(tseries)
library(Metrics)

# read data
usadata = read.csv(file = 'usa_unemployment_nsa.csv')
labour = read.csv(file = 'labour_backup.csv')
percentage = read.csv(file = 'rate.csv')

# function for delete outliers
del_outlier=function(ts){
  ind=tsoutliers(ts)$index
  replece=tsoutliers(ts)$replacements
  if (length(ind)!=0){
    for (i in 1:length(ind)){
      ts[ind[i]]=replece[i]
    }
  }
  return(ts)
}


# state27 Montana 
usadata[27,1]
state27 = c(usadata[27,])
state27 = state27[2:length(state27)]
state_raw = as.vector(unlist(state27))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data=del_outlier(ts_state_data)

plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)


# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value >0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
adf.test(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

#try orders
t=Arima(ts_state_data,order=c(4,1,1),seasonal=c(3,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#check AICc
t

#check RMSE
fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)

# state28 Nebraska
usadata[28,1]
state28 = c(usadata[28,])
state28 = state28[2:length(state28)]
state_raw = as.vector(unlist(state28))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data=del_outlier(ts_state_data)

plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value >0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(0,1,4),seasonal=c(1,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#p-value > 0.05 means significant 

#check AICc
t

#check RMSE
fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state29 Nevada
usadata[29,1]
state29 = c(usadata[29,])
state29 = state29[2:length(state29)]
state_raw = as.vector(unlist(state29))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data=del_outlier(ts_state_data)

plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value >0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(2,0,2),seasonal=c(0,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#check AICc
t

#check RMSE
fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)

# state30 New Hampshire
usadata[30,1]
state30 = c(usadata[30,])
state30 = state30[2:length(state30)]
state_raw = as.vector(unlist(state30))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data=del_outlier(ts_state_data)

plot(ts_state_data)

# p-value = 0.005187 < 0.05
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value >0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(5,1,1),seasonal=c(2,1,2))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#p-value > 0.05 means significant 

#check AICc
t

#check RMSE
fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state31 New Jersey
usadata[31,1]
state31 = c(usadata[31,])
state31 = state31[2:length(state31)]
state_raw = as.vector(unlist(state31))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data=del_outlier(ts_state_data)
plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value >0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(1,1,2),seasonal=c(0,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#check AICc
t

#check RMSE

fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state32 New Mexico
usadata[32,1]
state32 = c(usadata[32,])
state32 = state32[2:length(state32)]
state_raw = as.vector(unlist(state32))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data = del_outlier(ts_state_data)

plot.ts(ts_state_data)
plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

autoresult %>% residuals() %>% ggtsdisplay()


# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value < 0.05 stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(2,1,2),seasonal=c(0,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)


#check AICc
t

#check RMSE
fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state33 New York
usadata[33,1]
state33 = c(usadata[33,])
state33 = state33[2:length(state33)]
state_raw = as.vector(unlist(state33))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data = del_outlier(ts_state_data)
plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

autoresult %>% residuals() %>% ggtsdisplay()


# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value > 0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(2,0,2),seasonal=c(2,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#p-value > 0.05 means significant 

#check AICc
t

#check RMSE

fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state34 North Carolina
usadata[34,1]
state34 = c(usadata[34,])
state34 = state34[2:length(state34)]
state_raw = as.vector(unlist(state34))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data = del_outlier(ts_state_data)
plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

autoresult %>% residuals() %>% ggtsdisplay()


# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value > 0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(0,1,2),seasonal=c(0,1,3))
t %>% residuals() %>% ggtsdisplay() 
checkresiduals(t,plot = FALSE)

#p-value > 0.05 means significant 

#check AICc
t

#check RMSE

fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state35 North Dakota
usadata[35,1]
state35 = c(usadata[35,])
state35 = state35[2:length(state35)]
state_raw = as.vector(unlist(state35))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data = del_outlier(ts_state_data)
plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

autoresult %>% residuals() %>% ggtsdisplay()


# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value > 0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(2,0,2),seasonal=c(2,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#p-value > 0.05 means significant 

#check AICc
t

#check RMSE

fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state36 Ohio
usadata[36,1]
state36 = c(usadata[36,])
state36 = state36[2:length(state36)]
state_raw = as.vector(unlist(state36))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data = del_outlier(ts_state_data)
plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

autoresult %>% residuals() %>% ggtsdisplay()


# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value > 0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(0,1,2),seasonal=c(0,1,2))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#check AICc
t

#check RMSE

fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state37 Oklahoma
usadata[37,1]
state37 = c(usadata[37,])
state37 = state37[2:length(state37)]
state_raw = as.vector(unlist(state37))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data = del_outlier(ts_state_data)
plot(ts_state_data)


# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

autoresult %>% residuals() %>% ggtsdisplay()


# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value < 0.05 stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(0,1,4),seasonal=c(0,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#check AICc
t

#check RMSE

fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)


# state38 Oregon
usadata[38,1]
state38 = c(usadata[38,])
state38 = state38[2:length(state38)]
state_raw = as.vector(unlist(state38))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data = del_outlier(ts_state_data)
plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)


# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value > 0.05 non - stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(0,1,3),seasonal=c(0,1,1))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#p-value > 0.05 means significant 

#check AICc
t

#check RMSE
fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)

# state39 Pennsylvania
usadata[39,1]
state39 = c(usadata[39,])
state39 = state39[2:length(state39)]
state_raw = as.vector(unlist(state39))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)

ts_state_data = del_outlier(ts_state_data)
plot(ts_state_data)

# auto Arima
autoresult = auto.arima(ts_state_data,seasonal = TRUE)
checkresiduals(autoresult)

# check acf and pacf plot
acf(ts_state_data)
pacf(ts_state_data)
# p-value > 0.05 non-stationary
adf.test(ts_state_data)

# difference ts , d = 1
d = diff(ts_state_data, differences=1)
plot.ts(d)

#get p and q from plots
acf(d)
pacf(d)
ts_state_data %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
adf.test(d)

#try orders
t=Arima(ts_state_data,order=c(2,0,1),seasonal=c(0,1,2))
t %>% residuals() %>% ggtsdisplay()
checkresiduals(t,plot = FALSE)

#p-value > 0.05 means significant 

#check AICc
t

#check RMSE
fore = forecast(t,h=24)
tnew=state_raw[(length(state_raw)-51):(length(state_raw)-28)]
tnewnew = t(t(as.vector(fore$mean[1:length(fore$mean)])))
rmse(tnew, tnewnew)