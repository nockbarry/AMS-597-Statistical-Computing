dat <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/BloodAge.txt",header=T) 
fit = lm(dat$Age~dat$BP)
summary(fit)
anova(fit)
plot(dat$BP,dat$Age)
library(car)
vif(fit)
#age differs by bp group high

c4 = (dat$Age>70)
c4 = dat$Age[c4]
c4
c3 = (dat$Age<=70)
c3
c4
c3 = dat$Age[c3]
c3
c2 = (c3<=50)
c2 = c3[c2]
c2
c1 = (c2<=30)
c1 = c2[c1]
c1
#i think im supposed to build a contingency table and do a chi squared test

