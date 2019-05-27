# Functions to help wrangling data


#' Not in function
#'
#' This functions returns the negation of
#' the %in% operator.
#' @param x Item to be tested
#' @param y Set for item x to be not in to return true
#' @keywords not in
#' @export '%!in%'
'%!in%' <- function(x,y){
  !('%in%'(x,y))
}

#' is_numericable
#'
#' Checks to see if a vector can be coerced to numeric
#' @param x dubious vector
#' @keywords numeric coerce
#' @export is_numericable
is_numericable <- function(x) {
  suppressWarnings(all(!is.na(as.numeric(x))))
}

#' is_duration_string
#'
#' Takes a character string and attempts to determine if
#' it could be a duration type string.
#' Will return true if string starts with a number followed by
#' an 's', and is also possibly followed by a space and then an
#' open parenthesis and a ~ and a number and then anything.
#' So it will falsely return "31.2s (~40 cubits)" as TRUE
#' @param x the string to be examined
#' @keywords duration detect
#' @export is_duration_string
is_duration_string <- function(x) {
  all(str_detect(x, '^[0-9]+(\\.[0-9]+)?s( \\(~[0-9]+.*)?$'))
}


#' as_duration_from_string
#'
#' Gets strings that were durations and returns a duration
#' @param x a string, of form "310s (~5.17 minutes)" form
#' @keywords duration character clean tidy
#' @export as_duration_from_string
as_duration_from_string <- function(x) {
  x <- str_remove(x, " .*$") # removes everything after the space
  return(as.duration(x))
}

#' as_matrix_greedily
#'
#' Takes a data frame and tries to return a numeric matrix, first
#' converting as many fields that might be possible numerics first.
#' This includes 'duration' type characters of the form 31s. Others
#' to be included as needed
#' @param df data frame to be coerced into numeric
#' @keywords dataframe coerce numeric matrix
#' @export
as_matrix_greedily <- function(df) {
  # Find numerics first
  if (any(sapply(df, is_numericable))) {
    numdf <- df %>%
      select_if(is_numericable) %>%
      mutate_all(as.numeric)
    df <- df %>% select(names(df) %!in% names(numdf))
  }
  # Fix durations
  if (any(sapply(df, is_duration_string))) {
    durdf <- df %>%
      select_if(is_duration_string) %>%
      mutate_all(as_duration_from_string) %>%
      mutate_all(as.numeric)
  }
  return(bind_cols(numdf, durdf))
}

#' best_bet
#'
#' Function that takes in a list of values and attempts to
#' return a 'best bet' of what the value should be. Can handle
#' various data types.
#' @param candidates list of values to choose from
#' @keywords best bet na remove filter
#' @export best_bet
best_bet <- function(candidates) {
  l <- candidates[!is.na(candidates)]
  if (length(l) == 0) {
    return(NA)
  } else {
    t <- table(unlist(l))
    return(names(t)[t == max(t)])
    }
}

#' intervals
#'
#' Function that takes a numeric vector and outputs
#' the difference between each succsessive member.
#' It also needs to know where to place a 0 - either at the
#' end of the vector (matching each difference with where it
#' left from) or at the beginning of the vector (matching each
#' difference with where it goes to).
#'
#' @param x a numeric vector (can be dates, etc)
#' @param end_at_0 decision on where to place the 0 to return vector of the same length.
#' Defaults to TRUE, indicating the output matches with where the element is
#' leaving from.
#' @param sub_0 What this places for the end_at_0 parameter. Defaults to 0
#' @export
#' @examples
#'
#' v <- c(1,2,3)
#' intervals(v)
#' intervals(v, FALSE)
#' intervals(v, TRUE, NA)
intervals <- function(x, end_at_0 = TRUE, sub_0 = 0) {
  if (end_at_0) {
    output <- c(x[2 : length(x)] -  x[1: (length(x) - 1)], sub_0)
  } else {
    output <- c(sub_0, x[2 : length(x)] -  x[1: (length(x) - 1)])
  }
  return(output)
}

