#AMs 597 Quiz 3 Nicholas Barrett

logret<-read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/StockLogRet.txt",header=T)
attach(logret)
t.test(logret$MSFT,logret$IBM,alternative = "greater")
#H0 = MSFT>IBM p = .2004  cannot reject H0

fit1 <- lm(logret$IBM ~ 1 + logret$DELL) #A
fit2 <- lm(logret$IBM ~ 0 + logret$DELL) #B
summary(fit1) 
summary(fit2)
#fit 2 seems better
new.data = data.frame(D=.03) 

predict.lm(fit2, newdata=new.data, interval = 'prediction',level = .99)
