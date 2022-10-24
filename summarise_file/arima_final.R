library(forecast)
library(ggplot2)
usadata = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/usa_unemployment_nsa.csv')
labour = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/labour_backup.csv')
percentage = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/rate.csv')
para=read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/para.csv')

year=2018
month=12
#function used to remove outlier
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
#function used to clcluate error
compare=function(end_year,end_month,forecast){
  xx=as.matrix(forecast)
  yy=matrix(percentage[,(2+(end_year-1976)*12+end_month)])
  result=as.matrix(xx%*%yy)
  data=as.numeric(usadata[52,c((2+(end_year-1976)*12+end_month):(2+(end_year-1976)*12+11+end_month))])
  diff=result-matrix(data)
  return(diff) 
}

temp = c(usadata[52,])
temp = temp[2:length(temp)]
state_raw = as.vector(unlist(temp))
ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
plot(ts_state_data,type="l")

#the basic auto version of forecast function, as benchmark p.s. extreme slow
auto_arima=function(train_ts){
  temp = c(usadata[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
  ts_state_data=del_outlier(ts_state_data)
  
  fit <- auto.arima(ts_state_data,seasonal = TRUE)
  ro_pred=forecast(fit,h=12) 
  ro_res= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
  for(i in 2:51){
    temp = c(usadata[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
    ts_state_data=del_outlier(ts_state_data)
    fit <- auto.arima(ts_state_data,seasonal = TRUE)
    ro_pred<-forecast(fit,h=12) 
    ro_pred= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
    ro_res=cbind(ro_res,ro_pred)
  }
  return(ro_res)
}


#with stl model
STL_forecast= function(train_ts){
  temp = c(train_ts[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
  ts_state_data=del_outlier(ts_state_data)
  
  fit <- stl(ts_state_data,t.window=5, s.window = "periodic")
  stl_pred=forecast(fit,method="arima",h=12) 
  stl_res= t(t(as.vector(stl_pred$mean[1:length(stl_pred$mean)])))
  

  for(i in 2:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
    ts_state_data=del_outlier(ts_state_data)
    
    fit <- stl(ts_state_data,t.window=13, s.window = "periodic")
    stl_pred<-forecast(fit,method="arima",h=12) 
    stl_pred= t(t(as.vector(stl_pred$mean[1:length(stl_pred$mean)])))
    stl_res=cbind(stl_res,stl_pred)
    
    

  }
  return(stl_res)
}


#forecast with log transformation
forecast_fun_log = function(train_ts){
  temp = c(train_ts[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
  ts_state_data=del_outlier(ts_state_data)
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(log(ts_state_data),seasonal = TRUE)
  prediction = forecast(autoresult, h=12)
  prediction = t(t(as.vector(exp(prediction$mean[1:length(prediction$mean)]))))
  for(i in 2:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
    ts_state_data=del_outlier(ts_state_data)
    #above generate ts of 1976-2018 for each state
    autoresult=auto.arima(log(ts_state_data),seasonal = TRUE)
    fore_res = forecast(autoresult, h=12)
    fore_res = t(t(as.vector(exp(fore_res$mean[1:length(fore_res$mean)]))))
    prediction = cbind(prediction, fore_res)
  }
  return(prediction)
}



#with national also forecast, used for reconciliation
forecast_fun_log_national = function(train_ts){
  sigma = c()
  temp = c(train_ts[52,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
  ts_state_data=del_outlier(ts_state_data)
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(log(ts_state_data),seasonal = TRUE)
  sigma=append(sigma, autoresult$sigma2)
  prediction = forecast(autoresult, h=12)
  prediction = t(t(as.vector(exp(prediction$mean[1:length(prediction$mean)]))))
  for(i in 1:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
    ts_state_data=del_outlier(ts_state_data)
    #above generate ts of 1976-2018 for each state
    autoresult=auto.arima(log(ts_state_data),seasonal = TRUE)
    sigma=append(sigma, autoresult$sigma2)
    fore_res = forecast(autoresult, h=12)
    fore_res = t(t(as.vector(exp(fore_res$mean[1:length(fore_res$mean)]))))
    prediction = cbind(prediction, fore_res)
  }
  res <- list("pred" =prediction, "sigma" =sigma)
  return(res)
}



#forecast with national data and manual parameter, used for reconciliation
forecast_with_national= function(train_ts){
  sigma = c()
  param =matrix(0,52,3)
  temp = c(usadata[52,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
  ts_state_data=del_outlier(ts_state_data)
  
  fit <- auto.arima(ts_state_data,seasonal = TRUE)
  param[1,] = arimaorder(fit)[1:3]
  sigma=append(sigma, fit$sigma2)
  ro_pred=forecast(fit,h=12) 
  ro_res= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
  
  #  stlf_pred = stlf(ts_state_data,method='arima',h=24)
  #  stlf_res = t(t(as.vector(stlf_pred$mean[1:length(stlf_pred$mean)])))
  for(i in 1:51){
    temp = c(usadata[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
    ts_state_data=del_outlier(ts_state_data)
    fit <- Arima(ts_state_data,order=as.numeric(para[i,1:3]),seasonal=as.numeric(para[i,4:6]))
    param[i+1, ] = as.numeric(para[i,1:3])
    sigma=append(sigma, fit$sigma2)
    ro_pred<-forecast(fit,h=12) 
    ro_pred= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
    ro_res=cbind(ro_res,ro_pred)
  }
  res <- list("pred" =ro_res, "sigma" =sigma, 'param' = param)
  return(res)
}
forcast_manual_arima=forecast_with_national(usadata)
forecast_log_national=forecast_fun_log_national(usadata)

psi_vec  =matrix(0,52,13)

#calcluate the standard error of forecast error, used for reconciliation
for(i in 1:52){
  psi_vec[i,] =c(1, ARMAtoMA(ar = forcast_manual_arima$param[i,1], ma = forcast_manual_arima$param[i,3], 12))
}


#calcluate the variance of national forecast
national_variance=c()
temp = c(usadata[52,])
temp = temp[2:length(temp)]
national = as.vector(unlist(temp))
national_nc=national[1:(length(national)-39)]
national_variance[1]= var(national_nc)
for(i in 1:51){
  temp = c(usadata[i,])
  temp = temp[2:length(temp)]
  national = as.vector(unlist(temp))
  national_nc=national[1:(length(national)-39)]
  national_variance[i+1]= var(national_nc)
}



#calclute the variance of A, the direct forecast
sigp2 = matrix(0,12,52)
for(i in 1:12){#also loop on period
  temp = c()
  for(j in 1:52){
    temp[j] = national_variance[j]*sum(psi_vec[j,1:i])
  }
  sigp2[i,] = temp
}


average_weight = t(as.matrix(rowMeans(percentage[-c(1)]), ncol=1))

wei2 = c(1, average_weight^2)




#calcluating the variance of T-B, the indirect forecast, with variance addition role, ignore covariance as paper suggested
#with formula in paper chapter 3.2.3
gaa = matrix(0,12,51)
for(i in 1:12){#number of period here
  temp = c()
  sigtb = c()
  for(j in 1:51){
    loc = sigp2[i][-1]
    sigtb = append(sigtb, (sigp2[1,1] + sum(wei2[-j]*loc[-j])))
  }
  for(j in 1:51){
    temp[j] = 1-(sigp2[i,j]/(sigp2[i,j] + sigmatb[j]))
  }
  gaa[i,] = temp
}


# form up the G and S matrix and produce the forecast as chapter 3.2.3 discussed
filler = matrix(1,1,51)
S = matrix(0,52,51)
S[1,]=filler
for (i in 2:52){
  S[i,i-1] = 1
}
forecast = c()
for(i in 1:12){
  G = matrix(0, 51, 52)
  for(j in 1:51){
    temp = matrix(-1*(1-gaa[i,j]), 1,52)
    G[j,] = temp
    G[j,1] = 1-gaa[i,j]
    G[j,j+1] = gaa[i,j]
  }
  recon = S%*%G%*%forcast_manual_arima$pred[i,]
  forecast[i] = recon[1]
}

# visualise part
forecast_decomp=STL_forecast(usadata)
forecast_log=forecast_fun_log(usadata)
forecast_auto=auto_arima(usadata)

data=as.numeric(usadata[52,c((2+(2018-1976)*12+12):(2+(2018-1976)*12+11+12))])
diff_recon=-recon[1,]/1000-matrix(data)
diff_manual_arima=compare(year,month,forcast_manual_arima$pred[,2:52])
diff_log=compare(year,month,forecast_log)
diff_log1=compare(year,month,forecast_log_national$pred[,2:52])

diff_decomp=compare(year,month,forecast_decomp)
diff_auto=compare(year,month,forecast_auto)
diff_direct=forcast_manual_arima$pred[,1]-matrix(data)
diffffff=forecast/sum((S%*%G)[1,])-as.numeric(usadata[52,c((2+(2018-1976)*12+12):(2+(2018-1976)*12+11+12))])

plot(diff_auto, type="l",main="Forcast error with different aggregation method",xlab="month", ylab="difference",ylim =c(-0.2,0.75),col="green")
lines(diff_decomp,col="red")
lines(diff_log,col="blue")
lines(diff_manual_arima,col="orange")
lines(diffffff,col="purple")
lines(diff_direct,col="dark green")

abline(h=0, v=0)
legend("topleft", c("with decomposition", "with log","auto arima","manual arima","with reconciliation","direct forcast"),lty = c(1,1),
       col = c("red", "blue", "green","orange","purple", "dark green"),cex=0.7)







