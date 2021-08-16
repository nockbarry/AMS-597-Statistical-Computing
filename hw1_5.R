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