usadata = read.csv(file = 'C:/Users/a1360/OneDrive/桌面/assignment/dsp/usa_unemployment_nsa.csv')

#national:
temp = c(usadata[52,])
temp = temp[2:length(temp)]
national = as.vector(unlist(temp))
ts_national=ts(national,start=c(1976, 1), end=c(2022, 3), frequency=12)
plot(ts_national,main='Plot of us unemployment rate, include Covid era')
#remove pandemic
national_nc=national[1:(length(national)-27)]
ts_national_nc=ts(national_nc,start=c(1976, 1), end=c(2019, 12), frequency=12)
plot(ts_national_nc, main='Plot of us unemployment rate, exclude Covid era')
#slicing
ts_76_79 = c(national_nc)[1:48]
ts_80_84= c(national_nc)[49:108]
ts_85_89= c(national_nc)[109:168]
ts_90_94= c(national_nc)[169:228]
ts_95_99= c(national_nc)[229:288]
ts_00_04= c(national_nc)[289:348]
ts_05_09= c(national_nc)[349:408]
ts_10_14= c(national_nc)[409:468]
ts_15_18= c(national_nc)[469:528]
ts_76_79 = ts(ts_80_84,start=c(1976, 1), end=c(1979, 12), frequency=12)
ts_80_84= ts(ts_80_84,start=c(1980, 1), end=c(1984, 12), frequency=12)
ts_85_89= ts(ts_80_84,start=c(1985, 1), end=c(1989, 12), frequency=12)
ts_90_94= ts(ts_80_84,start=c(1990, 1), end=c(1994, 12), frequency=12)
ts_95_99= ts(ts_80_84,start=c(1995, 1), end=c(1999, 12), frequency=12)
ts_00_04= ts(ts_80_84,start=c(2000, 1), end=c(2004, 12), frequency=12)
ts_05_09= ts(ts_80_84,start=c(2005, 1), end=c(2009, 12), frequency=12)
ts_10_14= ts(ts_80_84,start=c(2010, 1), end=c(2014, 12), frequency=12)
ts_15_19= ts(ts_80_84,start=c(2015, 1), end=c(2019, 12), frequency=12)
plot(ts_76_79, main ='slicing every 5 years')
plot(ts_80_84, main ='slicing every 5 years')
plot(ts_85_89, main ='slicing every 5 years')
plot(ts_90_94, main ='slicing every 5 years')
plot(ts_95_99, main ='slicing every 5 years')
plot(ts_00_04, main ='slicing every 5 years')
plot(ts_05_09, main ='slicing every 5 years')
plot(ts_10_14, main ='slicing every 5 years')
plot(ts_15_19, main ='slicing every 5 years')

Visual = function(path, row_num){
  data = read.csv(file = path)
  temp = c(usadata[row_num,])
  temp = temp[2:length(temp)]
  national = as.vector(unlist(temp))
  ts_national=ts(national,start=c(1976, 1), end=c(2022, 3), frequency=12)
  plot(ts_national,main='Plot of us unemployment rate, include Covid era')
  #remove pandemic
  national_nc=national[1:(length(national)-27)]
  ts_national_nc=ts(national_nc,start=c(1976, 1), end=c(2019, 12), frequency=12)
  plot(ts_national_nc, main='Plot of us unemployment rate, exclude Covid era')
  ts_76_79 = c(national_nc)[1:48]
  ts_80_84= c(national_nc)[49:108]
  ts_85_89= c(national_nc)[109:168]
  ts_90_94= c(national_nc)[169:228]
  ts_95_99= c(national_nc)[229:288]
  ts_00_04= c(national_nc)[289:348]
  ts_05_09= c(national_nc)[349:408]
  ts_10_14= c(national_nc)[409:468]
  ts_15_18= c(national_nc)[469:528]
  ts_76_79 = ts(ts_80_84,start=c(1976, 1), end=c(1979, 12), frequency=12)
  ts_80_84= ts(ts_80_84,start=c(1980, 1), end=c(1984, 12), frequency=12)
  ts_85_89= ts(ts_80_84,start=c(1985, 1), end=c(1989, 12), frequency=12)
  ts_90_94= ts(ts_80_84,start=c(1990, 1), end=c(1994, 12), frequency=12)
  ts_95_99= ts(ts_80_84,start=c(1995, 1), end=c(1999, 12), frequency=12)
  ts_00_04= ts(ts_80_84,start=c(2000, 1), end=c(2004, 12), frequency=12)
  ts_05_09= ts(ts_80_84,start=c(2005, 1), end=c(2009, 12), frequency=12)
  ts_10_14= ts(ts_80_84,start=c(2010, 1), end=c(2014, 12), frequency=12)
  ts_15_19= ts(ts_80_84,start=c(2015, 1), end=c(2019, 12), frequency=12)
  plot(ts_76_79, main ='slicing every 5 years')
  plot(ts_80_84, main ='slicing every 5 years')
  plot(ts_85_89, main ='slicing every 5 years')
  plot(ts_90_94, main ='slicing every 5 years')
  plot(ts_95_99, main ='slicing every 5 years')
  plot(ts_00_04, main ='slicing every 5 years')
  plot(ts_05_09, main ='slicing every 5 years')
  plot(ts_10_14, main ='slicing every 5 years')
  plot(ts_15_19, main ='slicing every 5 years')
}
