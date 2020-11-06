


rm(list=ls())                                  ### Clean the R workspace

getwd()

sp500 <- read.csv('E:/uconn/S2/1-model2-5321/1-SP500.csv', header=T)       ### Load the daily return data of SP500 index
#sp500 <- read.csv('1-SP500.csv', header=T)      ##???????????????????????????????????????
# sp500 <- read.csv('E:/uconn/S2/sp500.csv', header=F)
#class(sp500)

price <- sp500$Close                           ### Extract the price information

#head(sp500)  #check the top datas
#tail(sp500)  #check the last datas
#class(price)


T     <- length(price)

ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 
#plot(ret)


ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)

ret   <- ret[ind]

## Figure 1.1
autocorr <- acf(ret, 100)$acf[2:101]           ### Calculate the autocorrelation for lags(order) from 1 to 100  #??????????????????1
plot(autocorr, type='l', col='blue')
abline(0, 0, col='red')            ## make a horizontal line
?acf

## Figure 1.3
retsquare <- ret^2
autocorr <- acf(retsquare, 100)$acf[2:101]  
plot(autocorr, type='l', col='black')
abline(0, 0, col='red')
library('tseries')
adf.test(retsquare, alternative='stationary') 

mean(ret)*100     ## average daily returns in percentages
sd(ret)*100       ## standard deviation in percentages

acf(price)
acf(ret)
acf(retsquare)


pacf(price)
pacf(ret)
pacf(retsquare)
