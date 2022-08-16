#usadata = read.csv(file = path)

#functon for checking unusual data for each state, exclude the covid era.

state_check = function(states_index){

  temp = c(usadata[states_index,])
  name = toString(temp[1])
  temp = temp[2:length(temp)]
  temp = as.vector(unlist(temp))
  states=temp[1:(length(national)-27)]
  ts=ts(states,start=c(1976, 1), end=c(2019, 12), frequency=12)
  plot(ts, main=name, xlab="year", ylab="perentage")
}

#replace the n with data index, see csv file  or use usadata[n,][1]to check
state_check(n)
