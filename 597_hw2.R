#AMS 597 HW 2
#Nicholas Barrett 110355429
#Q1
sample(1:4, size = 10, prob = c(.1,.2,.4,.3),replace = T)
runif(10,0,1)


#Q2
x <- rexp(100,rate = 2)
ecdf(x)
plot(x)
plot.ecdf(x)


#Q3
l <- "http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt"
x <- read.table(l,header = TRUE)
x
t.test(x$AmerExp)
#True mean is probably 0
wilcox.test(x$AmerExp)
#true return is probably 0
t.test(x$AmerExp,x$Pfizer)
#difference in mean could be 0 at .05
var.test(x$AmerExp,x$Pfizer)
#variance not equal at .05
wilcox.test(x$AmerExp,x$Pfizer)
#log returns could be the same


#Q4

my.t.test <- function(x,y=NULL,alternative="two.sided",mu0=0){
  if(alternative=="two.sided"){alt = FALSE}
  if(is.null(y)){
    x <- na.omit(x)
    n <- length(x)
    t0 <- sqrt(n)*(mean(x)-mu0)/sd(x)
    pv1 <- 2*pt(abs(t0),df=n-1,lower.tail=alt)
    return(list(stastic=t0,pvalue=pv1,df=n-1))
  } else {
    p = var.test(x,y)
    x <- na.omit(x)
    y <- na.omit(y)
    nx <- length(x)
    ny <- length(y)
    if(p$p.value <= .05){
      #unquel var t test
      t0 <- (mean(x)-mean(y))/sqrt((var(x)/nx)+(var(y)/ny))
      df <- (((var(x)/nx)+(var(y)/ny))**2)/(((var(x)**2)/(nx*nx*(nx-1))) + ((var(y)**2)/(ny*ny*(ny-1))))
      pv1 <- 2*pt(abs(t0),df,lower.tail=alt)
      return(list(stastic=t0,pvalue=pv1,df=df))
    } else {
      #equal var t test
      df <- nx + ny - 2 
      sp <- sqrt(((nx-1)*var(x) + (ny-1)*var(y))/df) 
      t0 <- (mean(x)-mean(y)-mu0)/(sp*sqrt((1/nx)+(1/ny)))
      pv1 <- 2*pt(abs(t0),df,lower.tail=alt)
      return(list(stastic=t0,pvalue=pv1,df=df))
  }
  }
}
#Test
t.test(x$Pfizer)
my.t.test(x$Pfizer)
t.test(x$Pfizer,x$Intel)
my.t.test(x$Pfizer,x$Intel)


#Q5

my.wilcox.test <- function(x)



x <- rnorm(10)
mu <- 0
my.wilcox.1samp <- function(x,mu=0){
  x <- x-mu
  n <- length(x)
  xr <- rank(abs(x))
  Splus <- sum(xr[x>0])
  
  Splus0 <- 0
  for(i in 1:n){
    tmp0 <- combn(1:n,i)
    tmp1 <- apply(tmp0,2,sum)
    Splus0 <- c(Splus0,tmp1)
  }
  
  pv1 <- length(which(Splus0>=Splus))/length(Splus0)
  pv2 <- length(which(Splus0<=Splus))/length(Splus0)
  pv <- 2*min(pv1,pv2)
  
  return(list(Splus=Splus,pvalue=pv))
}

#Q6

set.seed(123)
x <- rnorm(50)
y <- 2*x+rnorm(50)
#lest square estimate
bhat <- sum(x*y)/sum(x*x)
plot(x,y)
abline(a=0,b=bhat)
lm(y ~ 0 + x)
bhat

N <- length(x)
ccdc <- 0
for(i in 1:N){
  for(j in 1:50){
    cc = x[i]-x[j]
    dc = y[i]-y[j]
    if(cc*dc < 0){
      ccdc = ccdc + 1
    }
  }
}
tau = (ccdc)/choose(N,2)
tau

cor.test(x,y,method = "kendall")
