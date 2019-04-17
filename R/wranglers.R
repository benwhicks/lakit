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

#' best_bet
#'
#' Function that takes in a list of values and attempts to
#' return a 'best bet' of what the value should be. Can handle
#' various data types.
#' @param candidates list of values to choose from
#' @keywords best bet na remove filter
#' @export
best_bet <- function(candidates) {
  l <- candidates[!is.na(candidates)]
  if (length(l) == 0) {
    return(NA)
  } else {
    t <- table(unlist(l))
    return(names(t)[t == max(t)])
    }
}


