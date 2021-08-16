##AMS 597 HW#1 by Nicholas Barrett
#should work by running 1 line at a time
## Question 1
weight <- c(60,72,35,56,87,80,89,95,76,28,48,59)
mean(weight)
weight**2
length(weight)
count = 0
for (w in weight){if (w>55){count = count + 1}}
count
X <- vector(mode="character", length=length(weight))
iter = 0
for (w in weight){  
  iter = iter +1
  if (w>55 && w<85){
  X[iter]='Yes'
  }
  else {X[iter]='No'}
}
X

## Question 2
tmp <- matrix(rnorm(12),3,4)
tmp
sum(tmp[,1],tmp[,3])
tmp[1,]*tmp[3,]
dim(tmp)
iter = 0
for (i in tmp[1,]){if(i>.5){cat(i)}}
i

## Question 3
#depends, if you want
x1 <- c(1,NA,2,NA,3,NA)
x2 <- c(NA,1,2,3,NA)

#to be the same, then you remove the NA's and compact the vectors and compare
x1[!is.na(x1)] == x2[!is.na(x2)]

#if you want

x1 <- c(1,2,NA,3,NA)
x2 <- c(1,2,NA,3,NA)
#to be the same
#then check if 
x1[!is.na(x1)] == x2[!is.na(x2)]
#and
is.na(x1) == is.na(x2)
#and
length(x1) == length(x2)
# and if all of these are true then the vectors are the same

## Question 4
x1 <- c(1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,5)
x1
x <- factor(x1)
x
y <- c(5,4,3,2,1)
y[x]
#returns a vector with n levels, but the elements of are from z repeated as in y

## Question 5
myMedian.cal <- function(x){
  x = sort(x)
  y <- length(x)
  if (y %% 2 == 0)  {
    med <- (x[(y/2)] + x[(y/2 + 1)])/2  
    return(med)
  }
  else {
    med <- x[(y/2 +.5)]
    return(med)
  }
}

x <- c(1,2,3,4,5,6,7)
median(x)
myMedian.cal(x)

x <- c(1,2,3,4,5,6)
median(x)
myMedian.cal(x)

## Question 6
set.seed(123)
mydna <- paste(sample(c('a','t','c','g'),1000,replace=T),collapse = '')
count <- 0
print(mydna)
mydna <- unlist(strsplit(mydna, split = NULL))
print(mydna)

for (i in 1:length(mydna)){
  if(mydna[i] == "c" && mydna[i+1] == "g") {
    count = count + 1
    mydna[i] <- 'X'
    mydna[i+1] <- 'Y'
    
  }
}

print(count)
print(paste(mydna, collapse = ''))


## Question 7
phone.scrape <- function(link) {
x = read.table(link, sep = "\n")
grep("^\\s*[-. (]*(\\d{3})[-. )]*(\\d{3})[-. ]*(\\d{4})\\s*$", paste(x[,1]), perl=TRUE, value=TRUE)
}
link = "http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/PhoneNumber.txt"
phone.scrape(link)
