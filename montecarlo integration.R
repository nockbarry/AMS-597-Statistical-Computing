#montecarlo integration
#estimate w = inte from 0-1 g(x)dx if x1..xn ~U(0,1)
#then w_hat = 1/n sum i to n g(x1...xn)

#estimate w = inte form 0 to 1 exp(-x)dx (should be 1-e^-1)
n=1000
x=runif(1000)
w.hat = mean(sin(-x))
w.hat
w = 1-exp(-1)
error = w-w.hat
error

#reparameterization 
#w = int from a to b g(x)dx
#let y = (x-a)/(b-a)
#then w = int from 0 to 1 g(y(b-a)+a)(b-a)dy
#or draw X from U(a,b)
#compute average g(X)
#w.hat = (b-a)g(X)



#estim phi(-1.645) = int from -inf to -1,645 1/sqrt2pi exp(-t^2 / 2)dt
var_omega_hat = function(n){
  z = rnorm(n)
  gx = (z<= -1.645)
  omega_hat = mean(gx)
  var.gx = var(gx) ##sum((gx-omega_hat)^2)/(n-1)
  return(list(var_omega = var.gx/n, omega_hat = omega_hat ))
}

#reg monte carlo
n = 1000
z = rnorm(1000)
omega1 = mean(z<=-2)
var.omega1 = var(z<= -2)/n

#importance sampling f(x)=1/sqrt(2*pi)*exp(-x^2/2)=dnorm(x)
x = rcauchy(n)
imp.g = function(x){
  (x<=-2)*dnorm(x)/dcauchy(x)
}
#this one is more efficient and more precise
omega2 = mean(imp.g(x))
var.omega2 = var(imp.g(x)/n)
