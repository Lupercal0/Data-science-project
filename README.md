# Data-scence-projectv  
#place for data science project code and dataV  
library needed for this project(except default R package):  
forecast      (by Rob Hyndman)  



there are 3 folders  
&nbsp;The file in summarise folder here is mainly about the summarise of work, which include several part:  
&nbsp;&nbsp;Forecast part include two method:ARIMA and LSTM  
&nbsp;&nbsp;&nbsp;LSTM method is in the LSTM.ipynb file  
&nbsp;&nbsp;&nbsp;ARIMA method is in the forecast.r file  
&nbsp;&nbsp;&nbsp;#should be a file conclude manual-checking of arima parameter here#  
&nbsp;&nbsp;Aggrefrate part include:  
&nbsp;&nbsp;&nbsp;reconcilation method is in the recon.r(include special forecas function needed for reconciliation)  
&nbsp;&nbsp;&nbsp;naive weighted bottom to up method are included in the forecast.r  
&nbsp;&nbsp;&nbsp;regression method include in recon.r  
&nbsp;The dada folder:  
&nbsp;&nbsp;include version of:  
&nbsp;&nbsp;&nbsp;usa unemployment rate, including national data, 50 states and capital teritory data, as usa_unemployment_nsa.csvv
&nbsp;&nbsp;&nbsp;processed usa labor force data, as weight, in rate.csv  
&nbsp;the code folder:  
&nbsp;&nbsp;include expriment and preprocess data, most has been summarised in to summarise folder  
&nbsp;&nbsp;&nbsp;adjusting parameterxx-xx.rmd/.r file used to manualy adjust arima parameter  
&nbsp;&nbsp;&nbsp;analysis.r: use to find the statinoary data  
&nbsp;&nbsp;&nbsp;docomp.r and decomposition+arima.rmd:used to help find the parameter of arima  
&nbsp;&nbsp;&nbsp;predict_with_log_transformation.r: use to test the method of log transformation  
&nbsp;&nbsp;&nbsp;predict.r and predict2.r:use to generate first set of forecast  
&nbsp;&nbsp;&nbsp;processing.py:used to process labor force data to get weight needed for aggregration   
&nbsp;&nbsp;&nbsp;regression.r:first test place for the regression method to generate difference.  
&nbsp;&nbsp;&nbsp;stateCheck.r:file use to check property of timeseise, take alaska as example  
&nbsp;&nbsp;&nbsp;visualise.r: used to visualise the timeserise itself.   