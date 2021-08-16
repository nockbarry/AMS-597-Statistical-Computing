#' Generates a corrected Z-test
#' from looney and jones
#'
#' @param x Takes an nx2 numeric double with partially matched pairs
#' @return List of Z score and p value
#'
#' @examples
#'
#' set.seed(123)
#' x = rnorm(20)
#' y = .25 + rnorm(20,sd=.15)
#' x[1:3]=NA
#' y[7:12]=NA
#' data = cbind(x,y)
#' myloon(data)
#'
#' @export

myloon <- function(x){
  #x[,1] is T, x[,2] is N
  checkdata(x)
  x1 = na.omit(x)
  x2 = x[is.na(x[,2]),1] #unmatched  (col 1)
  x3 = x[is.na(x[,1]),2] #unmatched (col 2)
  n1 = dim(x1)[1] #matched pairs
  n2 = length(x2)
  n3 = length(x3)
  n12 = n1 + n2
  n13 = n1 + n3
  n123 = n1+n2+n3
  T.star = c(x1[,1],x2)
  N.star = c(x1[,2],x3)
  T.bar = mean(T.star)
  N.bar = mean(N.star)
  ST = sd(T.star)
  SN = sd(N.star)
  STN1 = cov(x1[,1],x1[,2])
  Zcorr = (T.bar-N.bar)/sqrt(((ST*ST)/n12)+((SN*SN)/n13)-((2*n1*STN1)/(n12*n13)))
  list(Zcorr = Zcorr,p.value = pnorm(Zcorr))
}
