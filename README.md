# Data-scence-projectv  
#place for data science project code and dataV  
library needed for this project(except default R package):  
forecast      (by Rob Hyndman)  



there are 3 folders  
&emsp;The file in summarise folder here is mainly about the summarise of work, which include several part:  
&emsp;&emsp;Forecast part include two method:ARIMA and LSTM  
&emsp;&emsp;&emsp;LSTM method is in the LSTM.ipynb file  
&emsp;&emsp;&emsp;ARIMA method is in the forecast.r file  
&emsp;&emsp;&emsp;#should be a file conclude manual-checking of arima parameter here#  
&emsp;&emsp;Aggrefrate part include:  
&emsp;&emsp;&emsp;reconcilation method is in the recon.r(include special forecas function needed for reconciliation)  
&emsp;&emsp;&emsp;naive weighted bottom to up method are included in the forecast.r  
&emsp;&emsp;&emsp;regression method include in recon.r  
&emsp;The dada folder:  
&emsp;&emsp;include version of:  
&emsp;&emsp;&emsp;usa unemployment rate, including national data, 50 states and capital teritory data, as usa_unemployment_nsa.csvv
&emsp;&emsp;&emsp;processed usa labor force data, as weight, in rate.csv  
&emsp;the code folder:  
&emsp;&emsp;include expriment and preprocess data, most has been summarised in to summarise folder  
&emsp;&emsp;&emsp;adjusting parameterxx-xx.rmd/.r file used to manualy adjust arima parameter  
&emsp;&emsp;&emsp;analysis.r: use to find the statinoary data  
&emsp;&emsp;&emsp;docomp.r and decomposition+arima.rmd:used to help find the parameter of arima  
&emsp;&emsp;&emsp;predict_with_log_transformation.r: use to test the method of log transformation  
&emsp;&emsp;&emsp;predict.r and predict2.r:use to generate first set of forecast  
&emsp;&emsp;&emsp;processing.py:used to process labor force data to get weight needed for aggregration   
&emsp;&emsp;&emsp;regression.r:first test place for the regression method to generate difference.  
&emsp;&emsp;&emsp;stateCheck.r:file use to check property of timeseise, take alaska as example  
&emsp;&emsp;&emsp;visualise.r: used to visualise the timeserise itself.   