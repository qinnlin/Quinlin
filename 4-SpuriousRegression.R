

### An example of supurious regression

rm(list=ls())
dd1 <- arima.sim( model = list(order=c(0, 1, 0)), n = 1000, sd = 0.1)
dd2 <- arima.sim( model = list(order=c(0, 1, 0)), n = 1000, sd = 0.1)

reg1 <- lm(dd1 ~ dd2)
summary(reg1)
acf(reg1$residuals) #always check residual 

#fix the problem
dd1_diff <- diff(dd1)
dd2_diff <- diff(dd2)

reg2 <- lm(dd1_diff ~ dd2_diff)
summary(reg2)
acf(reg2$residuals)



