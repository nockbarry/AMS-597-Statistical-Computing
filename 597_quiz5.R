#AMS 597 QUiz 5
#Nicholas Barrett
n = 10000
k = 0
j = 0
x = rep(NA,n)
while(k<n) {
  y = runif(n=1,min=0,max=2)
  j = j+1
  u = runif(1)
  if (u<=((y**3)/4)/(2*y)) {
    k = k+1
    x[k] = y
  }
}
  
hist(x)
j
