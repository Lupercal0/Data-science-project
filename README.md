# Data-scence-projectv  
#place for data science project code and dataV  
library needed for this project(except default R package):  
forecast      (by Rob Hyndman)  



there are 3 folders  
    The file in summarise folder here is mainly about the summarise of work, which include several part:  
        Forecast part include two method:ARIMA and LSTM  
            LSTM method is in the LSTM.ipynb file  
            ARIMA method is in the forecast.r file  
            #should be a file conclude manual-checking of arima parameter here#  
        Aggrefrate part include:  
            reconcilation method is in the recon.r(include special forecas function needed for reconciliation)  
            naive weighted bottom to up method are included in the forecast.r  
            regression method include in recon.r  
    The dada folder:  
        include version of:  
            usa unemployment rate, including national data, 50 states and capital teritory data, as usa_unemployment_nsa.csvv
            processed usa labor force data, as weight, in rate.csv  
    the code folder:  
        include expriment and preprocess data, most has been summarised in to summarise folder  
            adjusting parameterxx-xx.rmd/.r file used to manualy adjust arima parameter  
            analysis.r: use to find the statinoary data  
            docomp.r and decomposition+arima.rmd:used to help find the parameter of arima  
            predict_with_log_transformation.r: use to test the method of log transformation  
            predict.r and predict2.r:use to generate first set of forecast  
            processing.py:used to process labor force data to get weight needed for aggregration   
            regression.r:first test place for the regression method to generate difference.  
            stateCheck.r:file use to check property of timeseise, take alaska as example  
            visualise.r: used to visualise the timeserise itself.   