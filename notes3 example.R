gen.abc <- function(n,p){
  a = n*p
  x=sample(LETTERS,a,replace=TRUE)
  m=matrix(x,nrow = n, ncol = p)
  return(m)
}

sub.fun <- function(x) {
  y <- table(x)
  res1 <- names(y)[which(y==max(y))]
  return(res1)
}

my.freq <- function(abcmat){
  res1 <- apply(abcmat,1,sub.fun)
  return(res1)
}
m
gen


one.sample.t <-function(x,mu0=0){
  x <- na.omit(x)
  n <- length(x)
  t0 <- sqrt(n)*(mean(x)-mu0)/sd(x)
  pv1 <- 2*pt(abs(t0),df=n-1,lower.tail=FALSE)
  return(list(stastic=t0,pvalue=pv1))
}


#wilcoxn signed rank test

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


