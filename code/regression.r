#test corelation between states, and try to find alternative ways
#(mathmetical way rather than theoratical) to find weight.

#usadata is generated in the visualise.r file

#generate the matrix for process
y = as.matrix(usadata[52,][2:527])
y = t(y)
X = head(usadata, 51)
X= X[2:527]
X= matrix(unlist(X),ncol = 51,nrow = 526, byrow = TRUE)
#X be a matrix with states as colnums(parameters) and month's records as rows
#y is national result with time as row

#basic linear regression
linear_model = lm(y~X)

# add whatever other regression method you want here