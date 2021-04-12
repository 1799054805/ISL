#8.
library(MASS)
library(ISLR)
library(car)
attach(Auto)
summary(Auto)
lm.fit=lm(mpg ~ horsepower)
summary(lm.fit)
predict(lm.fit,data.frame(horsepower=c(98)),interval="confidence")
predict(lm.fit,data.frame(horsepower=c(98)),interval="prediction")
plot(horsepower,mpg)
abline(lm.fit,lwd=3,col="red")
par(mfrow=c(2,2))
plot(lm.fit)
#(a)
#1.原假设为horsepower与mpg不相关,因为p值远小于0.05,拒绝原假设,认为horsepower和mpg有关系。
#2. R-squared为0.6059,60.5948%的mpg可以被horsepower解释。
#3.线性回归系数小于零,mpg与horsepower负相关。
#4.mpg的预测值是24.46708,95%置信区间为(23.97308,24.96108),95%预测区间为(14.8094,34.12476)。
#(c)mpg与horsepower非线性相关。

#9.
pairs(Auto)
cor(subset(Auto,select=-name))
lm.fit2=lm(mpg~.-name,data=Auto)
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
plot(predict(lm.fit2), rstudent(lm.fit2))
lm.fit3=lm(mpg~displacement*weight+year*origin)
summary(lm.fit3)
lm.fit4=lm(mpg~log(horsepower)+sqrt(horsepower)+horsepower+I(horsepower^2))
summary(lm.fit4)
par(mfrow=c(2,2))
plot(lm.fit4)

#10.
attach(Carseats)
lm.fit5=lm(Sales~Price+Urban+US)
summary(lm.fit5)
lm.fit6=lm(Sales~Price+US)
summary(lm.fit6)
confint(lm.fit6)
plot(predict(lm.fit6),rstudent(lm.fit6))
par(mfrow=c(2,2))
plot(lm.fit6)
