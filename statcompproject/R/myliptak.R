#' Performs a liptak weighted Z test
#'
#' @param x Takes an nx2 numeric double with partially matched pairs
#' @param two.sided TRUE/FALSE for pooled alternative hypothesis
#' @param alternaive  takes "two.sided", "less", "greater" for alternaitive for individual t tests
#' @return List of pvalue
#'
#' @examples
#'
#' set.seed(123)
#' x = rnorm(20)
#' y = .25 + rnorm(20,sd=.15)
#' x[1:3]=NA
#' y[7:12]=NA
#' data = cbind(x,y)
#' myliptak(data)
#'
#' @export


myliptak <- function(x,two.sided = FALSE,alternative = 'greater'){
  #x[,1] is T, x[,2] is N
  checkdata(x)
  x1 = na.omit(x)
  x2 = x[is.na(x[,2]),1] #unmatched (col 1)
  x3 = x[is.na(x[,1]),2] #unmatched (col 2)
  n1 = dim(x1)[1] #matched pairs
  n2 = length(x2)
  n3 = length(x3)
  p1i = t.test(x1[,1],x1[,2],paired = TRUE,alternative)$p.value
  p2i = t.test(x2,x3, alternative)$p.value
  Z1i = qnorm(1-p1i)
  Z2i = qnorm(1-p2i)
  w1 = sqrt(2*n1)
  w2 = sqrt(n2+n3)
  pci = 1 - pnorm((w1*Z1i + w2*Z2i)/sqrt(w1*w1 + w2*w2))
  if(two.sided == FALSE){return(list(pci=pci))}
  if(two.sided == TRUE){
    if(pci < .5){ pci.star = pci*2
    } else {
      pci.star = 2*(1-pci)
    }
    return( list(pci.star = pci.star))
  }

}
