x = read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/PhoneNumber.txt", sep = "\n")

grep("^\\s*[-. (]*(\\d{3})[-. )]*(\\d{3})[-. ]*(\\d{4})\\s*$", paste(x[,1]), perl=TRUE, value=TRUE)


