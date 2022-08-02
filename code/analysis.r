
usadata = read.csv(file = 'path')
labour = read.csv(file = 'path')
percentage = read.csv(file = 'path')
library(forecast)




temp = c(usadata[1,])
temp = temp[2:length(temp)]


state_names = c()
state_adf_pvalues = c()
state_stationary_tatus = c()
num =c()
all_ts = list()
nonstationary_ts = list()
stationary_ts = list()

#statewise model analysis,just put whatever state index you want in the for loop
for(i in 1:51){
    temp = c(train_ts[i,])
    state_name = unlist(temp[1])
    val = temp[2:length(temp)]
    state_raw = as.vector(unlist(temp))
    #state_data=state_raw[1:(length(state_raw)-3)]
    ts_state_data=ts(state_raw,start=c(1976, 1), end=c(2018, 12), frequency=12)
    #above generate ts of 1976-2018 for each state
    autoresult = auto.arima(ts_state_data,seasonal = TRUE)
    summary(autoresult)
    state_names[i] = state_name
    state_adf_pvalues[i] = adf.test(ts_state_data)$p.value
    if (adf.test(ts_state_data)$p.value > 0.05) {
        state_stationary_tatus[i] = "non-stationary"
        num = append(num,i)
        nonstationary_ts[[state_name]] = ts_state_data
    } else {
        state_stationary_tatus[i] = "stationary"
        stationary_ts[[state_name]] = ts_state_data
    }
  }

for (name in names(nonstationary_ts)) {
  stationary_ts[[name]]=diff(nonstationary_ts[[name]], differences = 1)
}
