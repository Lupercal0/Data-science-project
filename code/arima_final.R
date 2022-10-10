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

forecast_with_national= function(train_ts){
  sigma = c()
  temp = c(usadata[52,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
  ts_state_data=del_outlier(ts_state_data)
  
  fit <- auto.arima(ts_state_data,seasonal = TRUE)
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
    sigma=append(sigma, fit$sigma2)
    ro_pred<-forecast(fit,h=12) 
    ro_pred= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
    ro_res=cbind(ro_res,ro_pred)
  }
  res <- list("pred" =ro_res, "sigma" =sigma)
  return(res)
}
forecast_ro=forecast_with_national(usadata)
diff1=compare(year,month,forecast_ro$pred[,2:52])
plot(diff1, type="l",main="perdicted difference",xlab="month", ylab="difference")

average_weight = t(as.matrix(rowMeans(percentage[-c(1)]), ncol=1))
wei2 = c(1,average_weight^2)
#this list has no national data
sigmatb = c()

for(i in 1:51){
  sigmatb = append(sigmatb, sum(wei2[-i]*forecast_ro$sigma[-i]))
}
g = c()
for(i in 1:51){
  g = append(g, 1-(forecast_ro$sigma[i]/(forecast_ro$sigma[i] + sigmatb[i])))
}



filler = matrix(1,1,51)
S = matrix(0,52,51)
S[1,]=filler
for (i in 2:52){
  S[i,i-1] = 1
}

G = matrix(0, 51, 52)
#G[,1] = t(average_weight)
for(i in 1:51){
  temp = matrix(-g[i], 1,52)
  G[i,] = temp
  G[i,1] = g[i]
  G[i,i+1] = 1-g[i]
}
recon = S%*%G%*%t(forecast_ro$pred)


data=as.numeric(usadata[52,c((2+(2018-1976)*12+12):(2+(2018-1976)*12+11+12))])
diff=-recon[1,]/1000-matrix(data)
diff2=forecast_ro$pred[,1]-matrix(data)
plot(diff2, type="l",main="perdicted difference",xlab="month", ylab="difference",ylim =c(-0.1,0.75),col="green")
lines(diff,col="red")
lines(diff1,col="blue")
abline(h=0, v=0)
legend("topleft", c("with recon", "mannual-arima","direct national"),lty = c(1,1),col = c("red", "blue", "green"),cex=0.7)