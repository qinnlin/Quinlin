



### Download stock prices from Yahoo Finance
rm(list=ls())
#install.packages('quantmod')
library(quantmod) 

# download data
getSymbols('AAPL', from ="2005-01-01", to = "2020-02-20")
head(AAPL)
tail(AAPL) # the end of the data at present time

price <- AAPL[,6]          # the column with adjusted closing price
ret   <- diff(log(price))  #difference 2-1 3-2...
ret   <- ret[-1]    #-1 get rid of  the first abservation
plot(price)
plot(ret)





### Download stock prices from Yahoo Finance
rm(list=ls())
library(quantmod)

# download data
getSymbols('^GSPC', from ="2000-12-01", to = "2019-02-20")
head(GSPC)
tail(GSPC)

price <- GSPC[,6]
ret   <- diff(log(price))
ret   <- ret[-1]
plot(price)
plot(ret)

