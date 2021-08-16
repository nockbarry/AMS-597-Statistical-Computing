#AMS 597 HW 4 Nicholas Barrett
#Q1
#a
dat <- read.delim("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/leukemiaDataSet.txt",header=T,sep='\t') 
str(dat)
set.seed(123)
trainID <- sample(1:72,round(0.7*72))
trainData <- dat[trainID,]
testData <- dat[-trainID,]

library(glmnet)

group = trainData$Group
varmtx <- model.matrix(group~.-1, data=trainData)
response <- trainData$Group 

# alpha=0 means ridge regression. 
ridge <- glmnet(varmtx, response, alpha=0,family = "multinomial")

# Cross validation to find the optimal lambda penalization
cv.ridge <- cv.glmnet(varmtx, response, alpha=0,family = "binomial")


plot(ridge, xvar = "lambda", label=T)
plot(cv.ridge,xvar = "lambda", label=T)


varmtx2 = model.matrix(testData$Group~.-1, data = testData)
#couldnt get this to run
predict.glmnet(ridge,newx = testData,type = "response")

fit = glm(group~.,data = trainData)

#b


#c


#Q2
fexp <- function(n,lam,v){
  x <- v+(-log(runif(n))/lam)
  x
}
y = fexp(1000,2,1)
hist(y)


#Q3
fcauch <- function(n){
  x <- tan(pi*(runif(n)-.5))
}
y = fcauch(1000)
hist(y)

#Q4

#utility function to visualize max
betaplot = function(a,b){
  f = function(x){(1/beta(a,b))*(x**(a-1))*((1-x)**(b-1))}
  x=seq(0,1,length=100)
  plot(x,f(x))
}
#returns max of the plot
maxc = function(a,b){
  f = function(x){(1/beta(a,b))*(x**(a-1))*((1-x)**(b-1))}
  x=seq(0,1,length=1000)
  return(max(f(x)))
}


fbeta <- function(a,b,n){
  k <- 0 #counter
  j <- 0 #iter
  x <- rep(NA,n)
  
  while(k<n){
    y <- runif(1)
    j <- j +1
    u <- runif(1)
    if(u<=(((1/beta(a,b))/maxc(a,b))*(y**(a-1))*((1-y)**(b-1)))){
      k <- k+1
      x[k] <- y
    }
  }
  return(c(x,j))
}

a= 3
b= 2

x = fbeta(a,b,1000)
hist(x[1:1000])
x[1001] # number of iterations

#Q5
fgamma <- function(a,n){
  k <- 0 #counter
  j <- 0 #iter
  x <- rep(NA,n)
  
  while(k<n){
    y <- rexp(1,rate = 1/a)
    j <- j +1
    u <- runif(1)
    if(u<=(((y**(a-1))*exp(-y)/gamma(a)))/(.35*exp(1/a))){
      k <- k+1
      x[k] <- y
    }
  }
  return(c(x,j))
}
#with lower c's than .2 you start losing some accuracy for faster computer


x = fgamma(3,1000)
hist(x[1:1000])
x[1001] # number of iterations
y=rgamma(1000,shape = 3)
hist(y)
