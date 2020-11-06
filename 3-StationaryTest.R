




rm(list=ls())                                  ### Clean the R workspace
#install.packages('tseries')                    ### Install packages
library('tseries')

shiller <- read.csv('3-Shillerdata.csv', header=TRUE)
plot(shiller$Price, type='l', main='SP500 Price: Jan 1881 to Sep 2017')
plot(shiller$PE, type='l', main='PE Ratio for SP500: Jan 1881 to Sep 2017')

adf.test(shiller$Price, alternative='stationary')   ### When the p-value is larger than 5%, the series is considered non-stationary
adf.test(shiller$PE, alternative='stationary')      ### When the p-value is larger than 5%, the series is considered non-stationary

acf(shiller$PE)

pacf(shiller$PE)  #determine how many lag does PE have

acf(shiller$Price) #ramdom walk



rm(list=ls())                                  ### Clean the R workspace
library('tseries')

sp500 <- read.csv('1-sp500.csv', header=T)       ### Load the daily return data of SP500 index
price <- sp500$Close                           ### Extract the price information

T     <- length(price)
ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 
ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)
ret   <- ret[ind]


adf.test(price, alternative='stationary')      ### When the p-value is larger than 5%, the series is considered non-stationary
adf.test(ret, alternative='stationary')        ### When the p-value is larger than 5%, the series is considered non-stationary




