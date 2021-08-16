#' Checks Data for formatting
#' Helper function
#' Makes sure data contains NA's. and is a 2xn numeric double
#'
#' @param x Takes an nx2 numeric double
#' @return warning if triggered
#'
checkdata <- function(x){
  nas = sum(as.numeric(is.na(x)))
  if(nas==0){warning('Data contains no missing values, expecting partially matched samples')}
  if(dim(x)[2] != 2){warning('Matrix/list does not have 2 columns, expecting a nx2 double')}
  if(is.numeric(data)==FALSE){warning('Expecting numeric nx2 double')}
}
