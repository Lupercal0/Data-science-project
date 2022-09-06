library(forecast)
library(ggplot2)
usadata = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/usa_unemployment_nsa.csv')
labour = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/labour_backup.csv')
percentage = read.csv(file = 'C:/Users/Cwz/Desktop/study/S3/capstone1/Data-scence-project-main/data/rate.csv')


STL_forecast= function(train_ts){
  temp = c(train_ts[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2017, 12), frequency=12)
  
  fit <- stl(ts_state_data,t.window=5, s.window = "periodic")
  stl_pred=forecast(fit,method="arima",h=24) 
  stl_res= t(t(as.vector(stl_pred$mean[1:length(stl_pred$mean)])))
  
#  stlf_pred = stlf(ts_state_data,method='arima',h=24)
#  stlf_res = t(t(as.vector(stlf_pred$mean[1:length(stlf_pred$mean)])))
  for(i in 2:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2017, 12), frequency=12)
    
    fit <- stl(ts_state_data,t.window=13, s.window = "periodic")
    stl_pred<-forecast(fit,method="arima",h=24) 
    stl_pred= t(t(as.vector(stl_pred$mean[1:length(stl_pred$mean)])))
    stl_res=cbind(stl_res,stl_pred)
    
    
  #  stlf_pred <- stlf(ts_state_data,method='arima',h=24)
  #  stlf_pred = t(t(as.vector(stlf_pred$mean[1:length(stlf_pred$mean)])))
  #  stlf_res=cbind(stlf_res,stlf_pred)
  }
  return(stl_res)
}

forecast_stl=STL_forecast(usadata)

  


compare=function(end_year,end_month,forecast){
  xx=as.matrix(forecast)
  yy=matrix(percentage[,(2+(end_year-1976)*12+end_month)])
  result=as.matrix(xx%*%yy)
  data=as.numeric(usadata[52,c((2+(end_year-1976)*12+end_month):(2+(end_year-1976)*12+23+end_month))])
  diff=result-matrix(data)
  return(diff) 
}
diff1=compare(2017,12,forecast_stl)
plot(diff1, type="l",ylim = c(0,0.7))
