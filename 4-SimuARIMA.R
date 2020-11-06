


### Simulate a MA time series

rm(list=ls())
dd <- arima.sim(model = list(ma = 1), n = 1000, sd = 0.1) 
acf(dd)
pacf(dd)
sd(dd)
dd2 <- arima.sim(model = list(ma = c(1,2)), n = 1000, sd = 0.1) 
acf(dd2)


### Simulate an ARMA time series

rm(list=ls())
dd <- arima.sim(model = list(ar = 0.9, ma = 0.9), n = 1000, sd = 0.1) #n number of simulation
acf(dd,10) #10 number of lag
sd(dd)





### Simulate a random walk

rm(list=ls())
library('tseries')
dd <- arima.sim( model = list(order=c(0, 1, 0)), n = 100000, sd = 0.1)   #model = list(order=c(0, 1, 0)) ar and ma trun off, only random walk turn on 
plot(dd)
acf(dd)
adf.test(dd, alternative='stationary')      ### When the p-value is larger than 5%, the series is considered non-stationary
dd2 <- arima.sim( model = list(order=c(0, 1, 0)), n = 1000, sd = 0.1)   #if n=100000 to n=1000 observation small; downward bais
acf(dd2,100)


### Simulate an ARIMA time series

rm(list=ls())
library('tseries')
dd <- arima.sim( model = list(ar = 0.9, order=c(1, 1, 0)), n = 100000, sd = 0.1)#order=c(1, 1, 0) ar=on radom=on ma=off ;ar(1)=0.9
plot(dd)
adf.test(dd, alternative='stationary')      ### When the p-value is larger than 5%, the series is considered non-stationary
acf(dd,10)




