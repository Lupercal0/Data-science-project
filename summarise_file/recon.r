#same function as the forecast_fun, but reform the structure to invlude the national forecast too, as the first line.
#same change can be used in the log version too.
library(forecast)
path = #your path to usa_unemployment_nsa.csv file
usadata = read.csv(file = path)
labour = read.csv(file = path)
percentage = read.csv(file = path)


forecast_fun_withnational = function(train_ts, end_year, end_month, period, begin_month){
  temp = c(train_ts[52,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  state_data=state_raw[1:(length(state_raw)-begin_month)]
  ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(ts_state_data,seasonal = TRUE)
  prediction = forecast(autoresult, h=period)
  prediction = t(t(as.vector(prediction$mean[1:length(prediction$mean)])))
  for(i in 1:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    state_data=state_raw[1:(length(state_raw)-begin_month)]
    ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
    #above generate ts of 1976-2018 for each state
    autoresult=auto.arima(ts_state_data,seasonal = TRUE)
    fore_res = forecast(autoresult, h=period)
    fore_res = t(t(as.vector(fore_res$mean[1:length(fore_res$mean)])))
    prediction = cbind(prediction, fore_res)
  }
  return(prediction)
}
#to avoid using covid data, whatever period want ot test, please ensuretest dataset ends before 12/2019
#for both of two forecast function below, 
#train_ts: is data needed to train and test(assume to be usada here)
#end_year, end_month: year and month when training set ends(yyyy/mm)
#period:how many periods you want to forecast, 1 period is 1 month
#begin_month:how many month are there between end of the trainning set(defined by end_year and end_month), and end of dataset(03/2022), shoud be 27+period
forecast_fun = function(train_ts, end_year, end_month, period, begin_month){
  temp = c(train_ts[1,])
  temp = temp[2:length(temp)]
  state_raw = as.vector(unlist(temp))
  state_data=state_raw[1:(length(state_raw)-begin_month)]
  ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
  #above generate ts of 1976-2018 for each state
  autoresult=auto.arima(ts_state_data,seasonal = TRUE)
  prediction = forecast(autoresult, h=period)
  prediction = t(t(as.vector(prediction$mean[1:length(prediction$mean)])))
  for(i in 2:51){
    temp = c(train_ts[i,])
    temp = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    state_data=state_raw[1:(length(state_raw)-begin_month)]
    ts_state_data=ts(state_data,start=c(1976, 1), end=c(end_year, end_month), frequency=12)
    #above generate ts of 1976-2018 for each state
    autoresult=auto.arima(ts_state_data,seasonal = TRUE)
    fore_res = forecast(autoresult, h=period)
    fore_res = t(t(as.vector(fore_res$mean[1:length(fore_res$mean)])))
    prediction = cbind(prediction, fore_res)
  }
  return(prediction)
}

#test corelation between states, and try to find alternative ways
#(mathmetical way rather than theoratical) to find weight.

#usadata is generated in the visualise.r file

#generate the matrix for process
y = t(as.matrix(usadata[52,][2:527]))
X = head(usadata, 51)
X= X[2:527]
X= matrix(unlist(X),ncol = 51,nrow = 526, byrow = TRUE)

#X be a matrix with states as colnums(parameters) and month's records as rows
#y is national result with time as row

#basic linear regression
linear_model = lm(y~X)

#this function assume using usada as real value
#end_year and end_month: end timepoint of the TRAINNING set
#forecast: the forecast generate, the period of this should fits the next argument
#period:how many period(s), in month your forecast are in
compare=function(end_year,end_month,forecast, period){
  xx=as.matrix(forecast)
  yy=matrix(percentage[,(2+(end_year-1976)*12+end_month)])
  result=as.matrix(xx%*%yy)
  data=as.numeric(usadata[52,c((2+(end_year-1976)*12+end_month):(2+(end_year-1976)*12+11+period+end_month))])
  diff=result-matrix(data)
  return(diff) 
}

#weighted aggregration based on the regression result
compare_regre=function(end_year,end_month,forecast, model){
  xx=as.matrix(forecast)
  yy=matrix(model$coefficients[2:length(model$coefficients)])
  intercept = matrix(model$coefficients[1], ncol=1, nrow = 12)
  result=as.matrix(xx%*%yy)+intercept
  data=as.numeric(usadata[52,c((2+(end_year-1976)*12+end_month):(2+(end_year-1976)*12+11+end_month))])
  diff=result-matrix(data)
  return(diff) 
}


#generate S matrix needed by reconciliation, which include national as well
average_weight = t(as.matrix(rowMeans(percentage[-c(1)]), ncol=1))
filler = matrix(1,1,51)
#generate summation matrix, to fit the relation. this matrix only represent relation no weight
S = matrix(0,52,51)
S[1,]=filler
for (i in 2:52){
  S[i,i-1] = 1
}
#generate G matrix that represent the weight, and GS = I
for(i in 1:51){
  temp = matrix(-average_weight[i], 1,52)
  G[i,] = temp
  G[i,1] = average_weight[i]
  G[i,i+1] = 1-average_weight[i]
}

prediction = forecast_fun_withnational(usadata, 2018, 12, 12, 39)

recon = S%*%G%*%t(prediction)
final_diff = forecast_fun_withnational(usadata, 2018, 12, 12, 39)

# another comparing with directly forecast base on national data directly
autoresult=auto.arima(ts_national_data,seasonal = TRUE)
prediction = forecast(autoresult, h=12)
prediction_na = t(t(as.vector(prediction$mean[1:length(prediction$mean)])))
real = c(usadata[52,])
real = real[(length(real)-38):(length(real)-27)]
real = as.vector(unlist(real))
diff_real = prediction_na - real

#print the plot of error , comparing three methods(just weighted sum ,recon and regression result)
data=as.numeric(usadata[52,c((2+(2018-1976)*12+12):(2+(2018-1976)*12+11+12))])
diff=recon[1]-matrix(data)

original = forecast_fun(usadata, 12)
original_weight = compare_real(2018,12,original)
plot(diff, type="l",main="difference between prediction and real, by reconcilation",xlab="month", ylab="difference in percentage",col="red", ylim = c(0,1.4))
lines(original_weight, col = "green")
lines(final_diff, col = "blue")
lines(diff_real, col = "black")
legend("topleft", c("with recon", "with easy weight", "with regression","direct national"),lty = c(1,1),col = c("red", "green", "blue", "black"),cex=0.7)
