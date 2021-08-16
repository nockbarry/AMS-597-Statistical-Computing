#AMS 597 HW 3
#Nicholas Barrett
#Q1 
#a
l <- "http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt"
x <- read.table(l,header = TRUE)
x
fit1 <- lm(x$Pfizer ~ x$Exxon + x$Citigroup , data = x)
summary(fit1)
#intercept is .004, exxon is .288, citigroup is .186, both exxon and citi significant at .05

#B
plot(x$Pfizer,x$Citigroup)
pp <-predict(fit1, int = "c")
matlines(sort(x$Citigroup), pp[order(x$Citigroup), ], lty =c(1,2, 2), col =c("black", "red", "red"))

plot(x$Pfizer,x$Exxon)
pp <-predict(fit1, int = "c")
matlines(sort(x$Exxon), pp[order(x$Exxon), ], lty =c(1,2, 2), col =c("black", "red", "red"))

#C
anova(fit1)
#signiifcant at .05, exxon is more signficant 

#D
fit2 <- lm(x$Pfizer ~ 0 + x$Exxon + x$Citigroup , data = x)
summary(fit2)
#Exxon is .25, and Citi is .18812, pv = .04
plot(x$Pfizer, x$Citigroup)
pp <-predict(fit2, int = "c")
matlines(sort(x$Citigroup), pp[order(x$Citigroup), ], lty =c(1,2, 2), col =c("black", "red", "red"))

anova(fit2)
anova(fit1,fit2)
#E
cor(x$Pfizer,x$Exxon)
#not 0

#Q2


#Q3
chicken.data <- data.frame(weight = ChickWeight$weight,Time = ChickWeight$Time, Diet = ChickWeight$Diet)
fit3 = (lm(weight ~ Time + Diet, data = chicken.data))
summary(fit3)
anova(fit3)

time2 = chicken.data[chicken.data$Time == 2,]
subsetmodel = lm(time2$weight~time2$Diet, data = time2)
summary(subsetmodel)
anova(subsetmodel)

#Q4
pos.pf = length(x$Pfizer[x$Pfizer>0])
neg = x$Pfizer[x$Pfizer<0]
prop.test(pos.pf,length(x$Pfizer),.55)

pos.in = length(x$Intel[x$Intel>0])
prop.test(pos.in,length(x$Intel),.55,alternative = "greater")

prop.test(c(pos.pf,pos.in),c(64,64))


#Q5
library(MASS)
attach(mcycle)
plot(times,accel)
bump.fit = lm(accel ~ poly(times,3,raw=TRUE))
vif(bump.fit)
summary(bump.fit)
confint(bump.fit, level=.95)
predicted.intervals <- predict(bump.fit,data.frame(times),interval='confidence',
                               level=0.95)
plot(times,accel)
lines(times,predicted.intervals[,1],col='green',lwd=3)
lines(times,predicted.intervals[,2],col='black',lwd=1)
lines(times,predicted.intervals[,3],col='black',lwd=1)
# a polynomial of 3 seems to work the best, although many higher order polynomials have significant p values
#

#Q6
link <- "http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/HW3Qn6Data.txt"
x <- read.table(link,header = TRUE)

fit1 = lm(x$y ~ x$x3)
vif(fit1)
BIC(fit1)
summary(fit1)
plot(x$x3,x$y)
abline(fit1)
means = apply(x,2,mean)

#The smallest BIC i could get with any model was just y ~ x3