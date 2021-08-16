#' Generates a modified t-statistic
#' from Kim et al
#'
#' @param x Takes an nx2 numeric double with partially matched pairs
#' @return List of t3 score and p value
#'
#' @examples
#'
#' set.seed(123)
#' x = rnorm(20)
#' y = .25 + rnorm(20,sd=.15)
#' x[1:3]=NA
#' y[7:12]=NA
#' data = cbind(x,y)
#' mykim(data)
#'
#' @export


mykim <- function(x){
  #x[,1] is T, x[,2] is N
  checkdata(x)
  x1 = na.omit(x)
  x2 = x[is.na(x[,2]),1] #unmatched (col 1)
  x3 = x[is.na(x[,1]),2] #unmatched  (col 2)
  n1 = dim(x1)[1] #matched pairs
  n2 = length(x2)
  n3 = length(x3)
  nH = 2/((1/n2)+(1/n3))
  D = x1[,1]-x1[,2]
  D.bar = mean(D)
  T.bar = mean(x2)
  N.bar = mean(x3)
  SD = sd(D)
  ST = sd(x2)
  SN = sd(x3)
  t3 = ((n1*D.bar)+nH*(T.bar-N.bar))/sqrt((n1*SD*SD)+(nH*nH*(((SN*SN)/n3) + ((ST*ST)/n2))))
  list(t3.score = t3,p.value = pnorm(t3))
}
