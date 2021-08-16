#AMS 597 HW7
#Nicholas Barrett

#Q1

r.boot.1 <- function(x,id){
  r <- median(x[id])
}

r.boot.2 <- function(x,id){
 r <-  median(x[id])
 v <- var(boot(data=x[id],statistic = r.boot.1,R=200)$t)
 return(c(r,v))

}

set.seed(123)
x <- rnorm(50)
boot(data=x,statistic = r.boot.1,R=1000)
my.boot.out = boot(data=x,statistic=r.boot.2,R=1000)
boot.ci(my.boot.out,type='all')

#Q2

#Q3
set.seed(123)
x <- rnorm(50)
y = .2*x*rnorm(50)

cor.test(x,y)

#Q4
fx = function(x){
  exp(2*x) - x - 6
}
fprimex = function(x){
  2*exp(2*x) - 1
}
x = seq(-1,1.5,length=100)
plot(x,fx(x))

x = c(0,1)
t <- 2
eps <- 10e-6
while(abs(x[t]-x[t-1])>eps&t<=1000){
  curx <- x[t]-fx(x[t])/fprimex(x[t])
  x <- c(x,curx)
  t <- t+1
}
x
abline(h=0)
abline(v=x[length(x)],col='red')
#value of x is
x[length(x)]

uniroot(fx,c(-1,1.5))
#root is approximately the same

#Q5

set.seed(123)
lambda.true <- 10
x <- rpois(1000,lambda.true)
mean(x)
res = optimize(fx,lower=-1,upper=20,maximum=TRUE)



