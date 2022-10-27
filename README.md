# Data-scence-projectv  
#place for data science project code and dataV  
library needed for this project(except default R package):  
forecast      (by Rob Hyndman)  
Data science project group 34:
Zhiyuan Gao; Weize Cai; Yuqi Cao; Rui Wang; Zoe Wu


there are 4 folders 
&emsp;The file in meeting minutes folder are meetingminutes through the project
&emsp;The file in summarise folder here is mainly about the summarise of work, which include several part:  
&emsp;&emsp;LSTM method is in the LSTM.ipynb file  
&emsp;&emsp;ARIMA method and reconciliation is in the arima_final.r file  
&emsp;The dada folder:  
&emsp;&emsp;include version of:  
&emsp;&emsp;&emsp;usa unemployment rate, including national data, 50 states and capital teritory data  
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