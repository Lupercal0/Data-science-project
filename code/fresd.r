#below are caculating
autoresult_test=auto.arima(ts_national_nc,seasonal = TRUE)
para = arimaorder(autoresult_test)
para = append(1, para)

psi_vec = ARMAtoMA(ar = para[1], ma = para[2], 12)



#should be 2 dimension. with row as period and col as state
sigp2 = c()
for(i in 1:12){#also loop on period
  temp = []
  for(j in 1:52){
    temp[j] = national_variance*sum(para[j][1:i])
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
    sigtb = append(sigtb, sum(wei2[-j]*sigp2[i][-j]))
  }
  for(j in 1:51){
    temp[j] = 1-(sigp2[i][j]/(sigp2[i][j] + sigmatb[j]))
  }
  gaa[i] = temp
}