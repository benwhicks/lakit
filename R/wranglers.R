# Functions to help wrangling data


#' Not in function
#'
#' This functions returns the negation of
#' the %in% operator.
#' @param x Item to be tested
#' @param y Set for item x to be not in to return true
#' @keywords not in
#' @export
#' '%!in%'

'%!in%' <- function(x,y){
  !('%in%'(x,y))
  }
