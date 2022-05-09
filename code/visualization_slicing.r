usadata = read.csv(file = 'path')

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
