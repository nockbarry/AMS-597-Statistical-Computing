g <- function(x){
  #integral function
  exp(-x)/(1+x^2)*(x>0)*(x<1)
#this someitmes dosent work for large x, like in the cauchy example
  }

g2 <- function(x){
  y = rep(0,length(x))
  id <- which(x>0 & x<1)
  y[id] = exp(-x[id])/(1+x[id^2])
  y
}

n <- 10000
x = runif(n)
#h1 uniform
omega.hat1 = mean(g(x))
var.omega.hat1 = var(g(x))/n
se.omega.hat1 = sd(var(g(x)))/sqrt(n)
#h2 exp(1)
x = rexp(n)
omega.hat2 = mean(g(x)/dexp(x))
se.omega.hat2 = sd(g(x)/dexp(x))/sqrt(n)

#h3 cauchy
x = rcauchy(n)
omega.hat3 = mean(g2(x)/dcauchy(x))
se.omega.hat3 = sd(g2(x)/dcauchy(x))/sqrt(n)

#h4 is truncated exp
x <- rexp(2*n)
x[x>1] = NA
u <- runif(n)
x = -log(1-u*(1-exp(-1)))

h4 <- function(x){
  exp(-x)/(1-exp(-1))
}

omega.hat4 = mean(g2(x)/h4(x))
se.omega.hat4 = sd(g(x)/h4(x))/sqrt(n)

#h5 is truncated cauchy
u = runif(n)
x = tan(pi*u/4)#solve integral to get this
h5 = function(x){
  4/(pi*(1+x^2))
}

omega.hat5 = mean(g2(x)/h5(x))
se.omega.hat5 = sd(g(x)/h5(x))/sqrt(n)
omega.hat = c(omega.hat1,omega.hat2,omega.hat3,omega.hat4,omega.hat5)
sd.omega.hat = c(se.omega.hat1,se.omega.hat2,se.omega.hat3,se.omega.hat4,se.omega.hat5)

