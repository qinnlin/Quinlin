
### Clean the R workspace
rm(list=ls())                                  

## file route
getwd()

## install and use packages 
install.packages('quantmod')
library(quantmod)  #download stock price from yahoo finance
library('tseries') #time series

install.packages("installr")
library(installr)
updateR()

install.packages("derivmkts") #bondpv; duration convexity
library("derivmkts")

##upload file
sp500 <- read.csv('E:/uconn/S2/1-model2-5321/1-SP500.csv', header=T)      
sp500 <- read.csv('1-SP500.csv', header=T)      ##same file can omit upper resourses
sp500 <- read.csv('E:/uconn/S2/sp500.csv', header=F) #no header

## class
class(sp500)

## build a t spaces variable
A  <- numeric(T)  #t spaces

##check datas
head(sp500)  #check the top datas
tail(sp500)  #check the last datas

##speify data
price <- sp500$Close 
price <- AAPL[,6] # the 6th column

##length
T     <- length(price)

##loop
ret   <- log(price[2:T]) - log(price[1:(T-1)]) 
ret   <- diff(log(price))  #difference 2-1 3-2...
for (i in 2:T){A[i] <- 0.94*A[i-1] + 0.06*ret[i-1]^2}
ret   <- ret[-1]    #-1 get rid of  the first abservation

##plot
plot(ret)
par(mfrow=c(2,2)) #c(2[row],2[column]) group of graph
plot(autocorr, type='l', col='blue',ylim=c(0,0.14),pch=22, xlab='x', ylab='y', main='SP500 Price') # y axies between 0-0.14 #main 'title'#pch=22 point square fangkuai
points(B, col='blue', type='l',lwd=4) #points making another plot on the top of the first plot #lwd The line width, a positive number, defaulting to 1. 
abline(0, 0, col='red')            ## make a horizontal line



##delet null
ind   <- which(ret!=0)          ### Only keep those returns that are not zero (i.e. not on holidays)
ret   <- ret[ind]
ret   <- ret[-1]    #-1 get rid of  the first abservation


##statistic language
mean(ret)*100   
sd(ret)*100  
cor(data31$x1, data31$y1) #correlation coefficient
cov(data31$x1, data31$y1)# covariance

retsquare <- ret^2
var<- -qnorm(0.01, mean=0, sd=sqrt(sigma2)) ##1% VaR  Normal Distribution
for (i in 251:T){var<- -quantile(ret[(i-250):(i-1)], probs=0.01)}  #ret[] choosing windows 1-250 2-251 ....#historic data to estimate


##other functions
autocorr <- acf(ret, 100)$acf[2:101]      ### Calculate the autocorrelation for lags(order) from 1 to 100   #the first one=1 because self-autocorrelation
adf.test(shiller$Price, alternative='stationary')   ###ADF test; When the p-value is larger than 5%, the series is considered non-stationary
apply(data31, 2, mean) #apply repeat; 2 means for each Column, who:data31; where:2-each column; do:mean(function)
reg1 <- lm( y1 ~ x1, data = data31) #regression
summary(reg1)
dd <- arima.sim(model = list(ar =c(0.5,0.4)), n = 10000, sd = 0.1) #ar=phy1=0.9r(t-1)  or ar=0.5r(t-1)+0.4r(t-2)


## obtain stock data from yahoo
library(quantmod) 
getSymbols('AAPL', from ="2005-01-01", to = "2020-02-20")
head(AAPL)

##calculate fix income(bond)
install.packages("derivmkts")
library("derivmkts")

coupon <- 8; mat <- 10; freq <- 1; principal <- 100; yield <- 0.104;
price <- bondpv(coupon, mat, yield, principal, freq) 
bondPrice <- price
MacDuration <- duration(price, coupon, mat, principal, freq, modified=FALSE)
ModifiedDuraiton <- duration(price, coupon, mat, principal, freq, modified=TRUE)
Convexity <- convexity(price, coupon, mat, principal, freq) 