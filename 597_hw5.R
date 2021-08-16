#AMS 597 HW 5
#Nicholas Barrett

#Q1
require(MASS)

my.chi <- function(n,df){
  n=n/2
  u1 = runif((df*n))
  u2 = runif((df*n))
  x1 = sqrt(-2*log(u1))*cos(2*pi*u2)
  x2 = sqrt(-2*log(u1))*sin(2*pi*u2)
  x = c(x1,x2)
  z = matrix(data =x , ncol = df)
  w = z**2
  w = apply(w,1,sum)
  
  return(w)
}

my.f <- function(n,a,b){
  x = my.chi(n,a)
  y = my.chi(n,b)
  f = (x/a)/(y/b)
}
my.f(1000,5,10)

#Q2

my.polar = function(n){
  #generates norms
  k=0
  x = rep(NA,n/2)
  y = rep(NA,n/2)
  j=0
  
  while(k<(n/2)){
    u1 = runif(1)
    u2 = runif(1)
    v1 = 2*u1 -1
    v2 = 2*u2 -1
    S = v1**2 + v2**2
    j=j+1
    if(S<1){
      k=k+1
      x[k]=sqrt(-2*log(S)/S)*v1
      y[k]=sqrt(-2*log(S)/S)*v2
    }
  }
  #print(length(x))
  return(c(x,y))
}
my.polar(10000)

#Q3
my.t <- function(n,df){
  z = my.polar(n)
  w = my.chi(n,df)
  t = rep(NA,n)
  for(i in 1:n){#i was getting a weird error without this, idk why, it should 
    #work without being looped
    t[i]=z[i]/sqrt(w[i]/df)
  }
  #t = z/sqrt(w/df)
  return(t)
}
halfmy.t <- function(n,df){ #this is just for comparison
  z = rnorm(n)
  w = rchisq(n,df)
  t = z/sqrt(w/df)
  return(t)
}
#both seem to work pretty well, it looks like the one 
#entirely made from runif is a little worse

my.mix = function(n){
  k = 0
  x = rep(NA,n)
  Y = sample(1:3,n,prob=c(.3,.35,.35),replace=TRUE)
  id1 = which(Y==1)
  id2 = which(Y==2)
  id3 = which(Y==3)
  
  x[id1]<-my.t(length(id1),3)
  x[id2]<-my.t(length(id2),5)
  x[id3]<-my.t(length(id3),7)
  x
}
my.mix(100)

#Q4
my.multinorm = function(n,mu,sigma){ #this was not specified to be a matrix
  #function in the question
  z = my.polar(n)
  x = sigma*sigma*z + mu
  x
}

#Q5
x = ChickWeight
y = x$weight
x1 = x$Time
x2 = x$Diet

fit =  lm(y~x1+x2)

my.ls = function(y,x1,x2){
  dum = matrix(data = 0,nrow=length(y),ncol = length(levels(x2)))
  
  for(i in 2:length(levels(x2))){
    dum[,i]= as.numeric(x2 == i)
  }
  X = cbind(1,x1,dum[,2:length(levels(x2))])
  #X = model.matrix(y~x1+x2,data=x) #this also works
  beta.est = ginv(t(X) %*% X) %*% t(X) %*% y
  beta.est
  
}

my.ls(y,x1,x2)
fit$coefficients

#Q6
require(splines)
require(ISLR)

x = mcycle$times
y = mcycle$accel
plot(x,y)

fit1 = smooth.spline(x,y,df=15)
lines(fit1)

yhat = predict(fit1,x)

#from hw3 solution poly of 12 or 13 was best fit
bump.fit = lm(y ~ poly(x,12,raw=TRUE))

yhat2 = predict(bump.fit,data.frame(x))
lines(x,yhat2)

MSE = (1/length(y))*sum((y-yhat$y)**2)
MSE
MSE2 = (1/length(y))*sum((y-yhat2)**2)
MSE2
#452 vs 463, slightly better as a spline
