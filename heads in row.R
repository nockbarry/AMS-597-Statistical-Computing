test <- function(n,t=100000){
count = 0 
for (i in 1:t){
  x = rbinom(n,1,.5)
  heads = vector(mode="numeric", length=41)
  for (i in 1:(length(x)-9)) {
    heads[i] = sum(x[i:(i+9)])

  }
  if (max(heads) > 5){count = count +1}
}
print(count)
}


x <- vector(mode="numeric", length=41)
for(i in 10:50){
  depth = 1000000
  x[i-9] = (test(i,depth)/depth)
}

for(i in 10:50){
  print(paste0('in ',i,' rolls p = ',x[i-9]))
}


x <- vector(mode="numeric", length=41)
x[1]=1/1024
for(i in 2:11){
  x[i]=x[1]+(i/(2**11))
  
}

for(i in 12:41){
  x[i]=x[i-1]+(1/(2**11))*(1-x[i-11])
}
x

x <- vector(mode="numeric", length=41)
x[1]=11/1024
for(i in 2:11){
  x[i]=x[1]+((11*i)/(2**10))
  
}

for(i in 12:41){
  x[i]=x[i-1]+(11/(2**10))*(1-x[i-11])
}
x
