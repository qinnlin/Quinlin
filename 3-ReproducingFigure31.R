

#### Reproducing Table 3.1 and Figure 3.1

rm(list=ls())

data31 <- read.csv('3-RegressionData.csv', header=TRUE)
apply(data31, 2, mean)
apply(data31, 2, sd)
cor(data31$x1, data31$y1)
cor(data31$x2, data31$y2)
cor(data31$x3, data31$y3)
cor(data31$x4, data31$y4)
?cor

reg1 <- lm( y1 ~ x1, data = data31)
summary(reg1)

reg2 <- lm( y2 ~ x2, data = data31)
summary(reg2)

reg3 <- lm( y3 ~ x3, data = data31)
summary(reg3)

reg4 <- lm( y4 ~ x4, data = data31)
summary(reg4)

par(mfrow=c(2,2))
plot(data31$x1, data31$y1, xlim=c(0, 20), ylim=c(0, 15), pch=22, xlab='x', ylab='y')
points(data31$x1, reg1$fitted.values, type='l', col='red', lwd=4)

plot(data31$x2, data31$y2, xlim=c(0, 20), ylim=c(0, 15), pch=22, xlab='x', ylab='y')
points(data31$x2, reg2$fitted.values, type='l', col='red', lwd=4)

plot(data31$x3, data31$y3, xlim=c(0, 20), ylim=c(0, 15), pch=22, xlab='x', ylab='y')#pch point square fangkuai
points(data31$x3, reg3$fitted.values, type='l', col='red', lwd=4)# lwd The line width, a positive number, defaulting to 1. 

plot(data31$x4, data31$y4, xlim=c(0, 20), ylim=c(0, 15), pch=22, xlab='x', ylab='y')
points(data31$x4, reg4$fitted.values, type='l', col='red', lwd=4)

#?par   info about lwd pch etc
