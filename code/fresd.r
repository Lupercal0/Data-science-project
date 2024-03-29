#below are caculating
forecast_with_national= function(train_ts){
  sigma = c()
  param = c()
  temp = c(usadata[52,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  ts_state_data=ts(state_raw,start=c(1976, 1), end=c(year, month), frequency=12)
  ts_state_data=del_outlier(ts_state_data)
  
  fit <- auto.arima(ts_state_data,seasonal = TRUE)
  param[1] = arimaorder(fit)
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
    param[i+1] = arimaorder(fit)
    sigma=append(sigma, fit$sigma2)
    ro_pred<-forecast(fit,h=12) 
    ro_pred= t(t(as.vector(ro_pred$mean[1:length(ro_pred$mean)])))
    ro_res=cbind(ro_res,ro_pred)
  }
  res <- list("pred" =ro_res, "sigma" =sigma, 'param' = param)
  return(res)
}


forecast_ro = forecast_with_national(usadata)


psi_vec  = c()

for(i in 1:52){
    psi_vec[i] = ARMAtoMA(ar = forecast_ro$param[i][1], ma = forecast_ro$param[i][2], 12)
}
#below are caculating

#psi_vec = c(1, ARMAtoMA(ar = para[1], ma = para[2], 12))



#should be 2 dimension. with row as period and col as state
sigp2 = c()

for(i in 1:12){#also loop on period
  temp = []
  for(j in 1:52){
    temp[j] = national_variance*sum(psi_vec[j][1:i])
  }
  sigp2[i] = temp
}




wei2 = c(1, average_weight^2)




#should be each row as each period and col with parameter for each g
gaa = c()
for(i in 1:12){#number of period here
  temp = c()
  sigtb = c()
  for(j in 1:51){
    loc = sigp2[i][-1]
    sigtb = append(sigtb, (sigp2[1] + sum(wei2[-j]*loc[-j])))
  }
  for(j in 1:51){
    temp[j] = 1-(sigp2[i][j]/(sigp2[i][j] + sigmatb[j]))
  }
  gaa[i] = temp
}



filler = matrix(1,1,51)
S = matrix(0,52,51)
S[1,]=filler
for (i in 2:52){
  S[i,i-1] = 1
}
#forecast will be a vector of sigle number which should be the forecast
forecast = c()
for(i in 1:12){
    G = matrix(0, 51, 52)
    for(j in 1:51){
      temp = matrix(-1*(1-gaa[i][j]), 1,52)
      G[j,] = temp
      G[j,1] = 1-gaa[i][j]
      G[j,j+1] = gaa[i][j]
    }
    recon = S%*%G%*%t(forecast_ro$pred[i])
    forecast[i] = recon[1]
}

