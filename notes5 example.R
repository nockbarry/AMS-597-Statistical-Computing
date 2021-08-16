library(ISwR)
str(cystfibr)
attach(cystfibr) #bringgs variables out of cystfibr

fit <- lm(pemax ~ height,data = cystfibr)


fit1 <- (lm(pemax ~ poly(height,2,raw=TRUE)))
# if raw not true will try to orthogonalize the two data vectors

fit2 <- lm(pemax ~height + I(height^2)) #same as above)
