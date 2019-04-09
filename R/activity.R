# Function dealing with 'activity' style data.
# This is aimed at dealing with rows of 'events',
# which are basically any table of data where one of the
# columns is a timestamp, where the timestamp is of
# a time or date format and denotes when the event happened

# testTimes <- as.POSIXlt.POSIXct(sample(10000000:11100000 , 100, replace = FALSE))

#' timelist_to_difference function
#'
#' This function converts a vector of timestamps (POSIXct/lt)
#' into a collection of all the differences between each timestamp
#' with each other. The idea is to head towards a frequency distribution
#' of the time series, but rather than using a signal it is using a
#' sequences of events or impulses
#' @param timestamps a vector of POSIXct
#' @keywords timestamp activity
#' @export
#' timelist_to_difference()
timelist_to_difference <- function(timestamps) {
  l = length(timestamps)
  timestamps <- as.numeric(timestamps)
  output <- NULL
  for (n in 1:(l - 1)) {
    output <- append(output, timestamps[n] - timestamps[(n + 1):l])
  }
  output <- lubridate::as.duration(abs(output))
  return(output)
}

#' plot_timestamp_spectrum
#'
#' This takes the output from timelist_to_difference and generates
#' a frequency density plot of the differences, which is akin to
#' a freqency ~ amplitude plot used in spectral analysis.
#' @param timelist_difference a vector of durations (see lubridate) as outputted by the timelist_to_difference function
#' @param trans x-axis transformation, defaults to 'log10'
#' @param ... Extra parameters passed to the geom_density geom
#' @keywords plot spectrum histogram time frequency
#' @export
#' plot_timestamp_spectrum
plot_timestamp_spectrum <- function(timelist_differences, trans = 'log10', ...) {
  df <- data.frame(intervals = as.numeric(timelist_differences))
  g <- ggplot(data = df, aes(x = intervals)) +
    geom_density(...) +
    scale_x_continuous(name = "Interval",
                       trans = trans,
                       breaks = c(1,60, 60*60, 60*60*24, 60*60*24*7),
                       labels = c("1 Second","1 Minute", "1 Hour", "1 Day", "1 Week")) +
    scale_y_continuous(name = "Amplitude") +
    theme_minimal() +
    theme(axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  return(g)
}
