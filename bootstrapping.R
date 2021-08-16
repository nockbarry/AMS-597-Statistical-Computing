
#summary stats
library(bootstrap)

theta.hat = cor(law$LSAT,law$GPA)

B = 1000
n = dim(law)[1]
theta.hat.b = rep(NA,B)

for(i in 1:B){
  id <- sample(1:n,n,replace=TRUE)
  bootlaw = law[id,]
  theta.hat.b[i] = cor(bootlaw$LSAT,bootlaw$GPA)
}

# same as sqrt(sum((theta.hat.b-mean(thheta.hat.b))^2)/(B-1))
se.theta.hat = sd(theta.hat.b)
se.theta.hat
bias.B = mean(theta.hat.b)-theta.hat
bias.B
#standard normal CI
c(theta.hat-qnorm(.025,lower.tail=F)*se.theta.hat,theta.hat+qnorm(.025,lower.tail=F)*se.theta.hat)
#basic CI

b <- c(quantile(theta.hat.b,.975),quantile(theta.hat.b,.025))
c(2*theta.hat-b[1],2*theta.hat-b[2])

#same as above
library(boot)
r.boot <- function(x,id){
  cor(x[id,1],x[id,2])
}

boot.stat = boot(data=law,statistic=r.boot,R=1000)

#HW 7 Q1 long
library(bootstrap)

med.hat = median(x)

B = 1000
n = 50
med.hat.b = se.mb = mb <- rep(NA,B)
R = 200

for(i in 1:B){
  id <- sample(1:n,n,replace=TRUE)
  bootx = x[id]
  med.hat.b[i] = median(bootx)
  med.hat.R = rep(NA,R)
  for(r in 1:R){
    id2 <- sample(1:n,n,replace=TRUE)
    bootx2 = bootx[id2]
    med.hat.R[r] = median(bootx2)
  }
  se.mb[i] <- sd(med.hat.R)
  mb[i] = (med.hat.b[i]-med.hat)/se.mb[i]
}

#sqrt(sum((med.hat.b-mean(med.hat.b))^2)/(B-1))
se.med.hat = sd(med.hat.b)
se.med.hat
bias.B = mean(med.hat.b)-med.hat
bias.B
#standard normal bootstrap CI
c(med.hat-qnorm(.025,lower.tail=F)*se.med.hat,med.hat+qnorm(.025,lower.tail=F)*se.med.hat)
#Basic bootstrap Ci
b <- c(quantile(med.hat.b,.975),quantile(med.hat.b,.025))
c(2*med.hat-b[1],2*med.hat-b[2])

#bootstrap t interval
alpha = .05
t1 = quantile(mb,1-alpha/2)
t2 = quantile(mb,alpha/2)
c(med.hat-t1*se.med.hat,med.hat-t2*se.med.hat)


