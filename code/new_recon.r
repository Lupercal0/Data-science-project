average_weight = t(as.matrix(rowMeans(percentage[-c(1)]), ncol=1))
sigma = []

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
    sigma.append(sigma2, fit$sigma2)
    ro_pred<-forecast(fit,h=12) 
    ro_pred= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
    ro_res=cbind(ro_res,ro_pred)
  }
  return(ro_res)
}



#calcluate sigma (tnota)
wei2 = average_weight^2
#this list has no national data
sigmatb = []
for(i in 1:51){
  sigmatb = aoppend(sigmatb, wei2[-i]*sigma[-i])
}
g = []
for(i in 1:51){
  g = append(g, (sigma[i]/(sigma[i] + sigmatb[i])))
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

prediction = forecast_fun_withnational(usadata, 2018, 12, 12, 39)

recon = S%*%G%*%t(prediction)