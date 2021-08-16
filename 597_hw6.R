#AMS 597 HW 6
#Nicholas Barrett

#Q1
x = runif(10000,min=0,max=pi/3)
g = mean(sin(x))
w.hat = pi/3 *g
w.hat
w = -cos(pi/3) + cos(0)
w
w.hat-w


#Q2

n=1000
x = runif(n,min=0,max=.5)
g = mean(exp(-x))
w.hat = .5*g
w.hat
var.g = sum((exp(-x)-w.hat)**2)/(n-1)
var.g

g2 = rexp(n)
g.mean = mean(g2)
w.hat2 = .5*g.mean
w.hat2
var.g2 = sum((g2-w.hat)**2)/(n-1)
var.g2
#var.g is much smaller, runif did better

#Q3
my.pbeta1 = function(a,b,x){
  t = runif(n,0,x)
  g = (t**(a-1))*((1-t)**(b-1))
  B.x = (x-0)*(mean(g))
  B.x
}

my.pbeta2 = function(a,b,x){
  u = rgamma(n,a)
  v = rgamma(n,b)
  y = u/(u+v) #not sure what to do with this part
  b = (x**(a-1))*((1-x)**(b-1))/mean(y) # this dosent work but idk
  return(b)
}

n=1000
a = 3
b = 3
x = seq(.1,.9,.1)
Fx = rep(NA,length(x))
ind = 0
pbs = rep(NA,length(x))
for (i in x){
  ind = ind + 1
  Fx[ind]=my.pbeta1(a,b,i)/beta(3,3) #i couldnt get my.pbeta2 to work so i am showing with beta(3,3)
  pbs[ind]=pbeta(i,a,b)
}
Fx
pbs
err = Fx-pbs
err

#Q4a
iter = 1000
tpval = rep(NA,iter)
wpval = rep(NA,iter)

for (i in 1:iter){
  x = rnorm(20)
  ttest = t.test(x)
  wtest = wilcox.test(x)
  wpval[i] = wtest$p.value
  tpval[i] = ttest$p.value 
}
alpha.t = sum(as.numeric(tpval < .05))/iter
alpha.t

alpha.w = sum(as.numeric(wpval < .05))/iter
alpha.w
#b
iter = 1000
tpval = rep(NA,iter)
wpval = rep(NA,iter)

for (i in 1:iter){
  x = rnorm(20,mean=.5)
  ttest = t.test(x)
  wtest = wilcox.test(x)
  wpval[i] = wtest$p.value
  tpval[i] = ttest$p.value 
}
beta.t = sum(as.numeric(tpval > .05))/iter
power.t = 1-beta.t
power.t
beta.w = sum(as.numeric(wpval > .05))/iter
power.w = 1-beta.w
power.w

#Q5

my.powertest = function(n){
  iter = 1000
  tpval = rep(NA,iter)
  wpval = rep(NA,iter)
  for (i in 1:iter){
    x = rnorm(n)
    y = rnorm(n,mean=.5,sd=sqrt(1.5))
    ttest = t.test(x,y)
    tpval[i] = ttest$p.value 
  }
  beta.t = sum(as.numeric(tpval > .05))/iter
  power.t = 1-beta.t
  power.t
}

N = seq(10,100,10)
powers = rep(NA,length(N))
ind = 0
for(i in N){
  ind = ind +1
  powers[ind]=my.powertest(i)
}
plot(N,powers)
# n ~ 70 for a power > 80%
#b
n = ((1.96+1.645)**2)*(1.5)/(.25)
n
