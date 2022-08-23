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


#weighted aggregration based on the regression result
compare=function(end_year,end_month,forecast, model){
  xx=as.matrix(forecast)
  yy=matrix(model$coefficients[2:length(model$coefficients)])
  intercept = matrix(model$coefficients[1], ncol=1, nrow = 12)
  result=as.matrix(xx%*%yy)+intercept
  data=as.numeric(usadata[52,c((2+(end_year-1976)*12+end_month):(2+(end_year-1976)*12+11+end_month))])
  diff=result-matrix(data)
  return(diff) 
}

#weighted aggrgration based on labor force weighted
compare_real=function(end_year,end_month,forecast){
  xx=as.matrix(forecast)
  yy=matrix(percentage[,(2+(end_year-1976)*12+end_month)])
  result=as.matrix(xx%*%yy)
  data=as.numeric(usadata[52,c((2+(end_year-1976)*12+end_month):(2+(end_year-1976)*12+11+end_month))])
  diff=result-matrix(data)
  return(diff) 
}

final_diff=compare(2018,12,forecast_res, linear_model)
plot(final_diff, type="l",main="difference between prediction and real, in 12 month",xlab="month", ylab="difference in percentage", col="blue")

final_diff_real=compare_real(2018,12,forecast_res)
#plot(final_diff_real, type="l",main="difference between prediction and real, in 12 month, with labor force rate",xlab="month", ylab="difference in percentage")
lines(final_diff_real, col="red")
legend(0.5, legend=c("with labour weight", "with regression weight"),col=c("red", "blue"), lty=1:2, cex=0.8)