#notes 8 examples for generating RV's

c = 6
n = 10000
k = 0 # counter for acceptance
j = 0 ##number of iterations

x <- rep(NA,n)

while(k<n){
  y = runif(1)
  j = j+1
  u = runif(1)
  if(u<=4*y*(1-y)){
    k = k+1
    x[k] = y
  }
}
hist(x)
j

#Transform method
lambda = 1
n = 10000
a = 2
b = 2
x = rgamma(n,a,lambda)
y = rgamma(n,b,lambda)
w = x/(x+y) ## w~beta(a,b)
hist(w)

#Box Muller Method
u1 = runif(1500)
u2 = runif(1500)
x1 = sqrt(-2*log(u1))*cos(2*pi*u2)
x2 = sqrt(-2*log(u1))*sin(2*pi*u2)
x = c(x1,x2)
hist(c(x1,x2))
qqnorm(c(x1,x2))


df = 3
n = df*1000
z = matrix(data =x , ncol = df)
w = z**2
w = apply(w,1,sum)
hist(w)
#compare to r chi square

w1 = rchisq(1000,df)
qqplot(w1,w)
abline(0,1)
