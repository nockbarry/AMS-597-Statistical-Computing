BarrettReplaceCountry <- function(string){
  string = gsub("America","USA",string, perl = TRUE)
  string = gsub("Britain","UK",string, perl = TRUE)
  string = gsub("England","UK",string, perl = TRUE)
  string = gsub("[0-9.]", "#", string, perl = TRUE)
  return(string)
}

test = "this is America and the UK with 5.0 and 3 and 2"
BarrettReplaceCountry(test)

