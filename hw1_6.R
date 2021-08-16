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
    mydna[i+1] <- ' Y'
    
  }
}

print(count)
print(paste(mydna, collapse = ''))



