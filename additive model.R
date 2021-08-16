require(splines)
require(ISLR)
attach(Wage)
x = Wage
agelims = range(x$age)
age.grid = seq(from = agelims[1], to = agelims[2])
age.grid


fit <- lm(x$wage ~ bs(x$age, knots = c(25,40,60)),data = x)
summary(fit)

plot(x$age,x$wage)
points(x$age, predict(fit, newdata = list(age = age.grid)),col = "darkgreen", lwd = 2, type = "l")

abline(v =c(25, 40, 60), lty = 2, col = "darkgreen")
fit1 <- smooth.spline(age,wage,df = 5)
plot(age,wage,col='grey')
points()
lines(fit1,col="red")

fit2 <- smooth.spline(age,wage,cv=TRUE)
fit2
lines(fit2,col="blue")
