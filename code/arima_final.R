library(forecast)
library(ggplot2)
usadata = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/usa_unemployment_nsa.csv')
labour = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/labour_backup.csv')
percentage = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/rate.csv')
para=read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/para.csv')

year=2018
month=12
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



remove_outliers_forecast= function(train_ts){
  temp = c(usadata[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
  ts_state_data=del_outlier(ts_state_data)
  
  fit <- Arima(ts_state_data,order=as.numeric(para[1,1:3]),seasonal=as.numeric(para[1,4:6]))
  ro_pred=forecast(fit,h=12) 
  ro_res= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
  
  #  stlf_pred = stlf(ts_state_data,method='arima',h=24)
  #  stlf_res = t(t(as.vector(stlf_pred$mean[1:length(stlf_pred$mean)])))
  for(i in 2:51){
    temp = c(usadata[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
    ts_state_data=del_outlier(ts_state_data)
    fit <- Arima(ts_state_data,order=as.numeric(para[i,1:3]),seasonal=as.numeric(para[i,4:6]))
    ro_pred<-forecast(fit,h=12) 
    ro_pred= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
    ro_res=cbind(ro_res,ro_pred)
  }
  return(ro_res)
}
forecast_ro=remove_outliers_forecast(usadata)
diff1=compare(year,month,forecast_ro)
plot(diff1, type="l",main="perdicted difference",xlab="month", ylab="difference")