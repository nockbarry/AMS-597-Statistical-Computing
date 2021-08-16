#' Generates a corrected Z-test
#' based on the MLE under homoscedsasticity
#' proposed by Ekbohm
#'
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
#' myekbohm(data)
#'
#' @export



myekbohm <- function(x){
  #x[,1] is T, x[,2] is N
  checkdata(x)
  x1 = na.omit(x)
  x2 = x[is.na(x[,2]),1] #unmatched col 1
  x3 = x[is.na(x[,1]),2] #unmatched col 2
  n1 = dim(x1)[1] #matched pairs
  n2 = length(x2)
  n3 = length(x3)
  n12 = n1 + n2
  n13 = n1 + n3
  n123 = n1+n2+n3

  T.bar = mean(x2)
  N.bar = mean(x3)
  T1.bar = mean(x1[,1])
  N1.bar = mean(x1[,2])
  ST1 = sd(x1[,1])
  SN1 = sd(x1[,2])
  STN1 = cov(x1[,1],x1[,2])
  ST = sd(x2)
  SN = sd(x3)

  r = STN1/(ST1*SN1)
  g = (n1*((n1 + n2 +n3*r)))/(n12*n13 - (n2*n3*r*r))
  f = (n1*((n1 + n3 +n2*r)))/(n12*n13 - (n2*n3*r*r))
  sig.hat2 = (ST1*ST1*(n1-1) + SN1*SN1*(n1-1) + (1+r*r)*(ST*ST*(n2-1) +SN*SN*(n3-1))) / (2*(n1-1) + (1+r*r)*(n2+n3-2))
  V1.star = sig.hat2*( (2*n1*(1-r) + (n2+n3)*(1-r*r) ) / (n12*n13 - n2*n3*r*r) )

  ZE = ((f*(T1.bar-T.bar)) - (g*(N1.bar-N.bar)) + T.bar - N.bar)/sqrt(V1.star)
  list(ZE = ZE , p.value = pnorm(ZE))
}

