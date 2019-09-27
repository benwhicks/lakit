# Function dealing with 'activity' style data.
# This is aimed at dealing with rows of 'events',
# which are basically any table of data where one of the
# columns is a timestamp, where the timestamp is of
# a time or date format and denotes when the event happened


#' timelist_to_difference function
#'
#' This function converts a vector of timestamps (POSIXct/lt)
#' into a collection of all the differences between each timestamp
#' with each other. The idea is to head towards a frequency distribution
#' of the time series, but rather than using a signal it is using a
#' sequences of events or impulses.
#'
#' The other arguments are passed to the data.frame function and need to be
#' of the form: var = "value", as you might want to pass through an identifier
#' such as a student id or content id with the table of differences for later use
#' @param timestamps a vector of POSIXct
#' @keywords timestamp activity
#' @export timelist_to_difference
#' @return dataframe, with first column called 'intervals'
#' @family timelist
#' @inherit as.duration
#' @examples
#' testTimes <- as.POSIXlt.POSIXct(sample(10000000:11100000 , 1000, replace = FALSE))
#' timelist_to_difference(testTimes)
timelist_to_difference <- function(timestamps) {
  n <- length(timestamps)
  M <- matrix(rep(as.numeric(timestamps), n), ncol = n, nrow = n)
  M_diff <- M - t(M)
  diff <- abs(M_diff[upper.tri(M_diff, diag = FALSE)])
  if (min(diff) == 0) {
    next_min <- min(diff[diff > 0])
    diff <- c(diff[diff > 0], rep(next_min * 0.5, length(diff[diff == 0])))
  }
  output <- lubridate::as.duration(diff)
  return(output)
}

# old_timelist_to_difference <- function(timestamps) {
#   # This function could use some optimisation
#   l = length(timestamps)
#   timestamps <- as.numeric(timestamps)
#   output <- NULL
#   for (n in 1:(l - 1)) {
#     output <- append(output, timestamps[n] - timestamps[(n + 1):l])
#   }
#   if (min(output) == 0) {
#     next_min <- min(output[output > 0])
#     output <- c(output[output > 0], rep(next_min * 0.5, length(output[output == 0])))
#   }
#   output <- as.duration(abs(output))
#   return(output)
# }

#' timelist_to_difference_assembler
#'
#' @param timestamps a vector of timestamp
#' @param ids a corresponding vector of ids, matching to the timestamps
#' @keywords timestamp activity
#' @family timelist
#' @export timelist_to_difference_assembler
#' @examples
#' aa <- data.frame(ids = rep(c(1,2,3,4),25), ts = as.POSIXlt.POSIXct(sample(10000000:11100000 , 100, replace = FALSE)))
#' timelist_to_difference_assembler(aa$ts, aa$ids)
timelist_to_difference_assembler <- function(df, timestamp_var = "timestamp", id_vars = NULL) {
  df <- data.frame(timestamp = timestamps, id = ids)
  u_ids <- unique(ids)
  output <- data.frame()
  for (this_id in u_ids) {
    df_for_id <- df %>% filter(id == this_id)
    output <- rbind(timelist_to_difference(df_for_id$timestamp, id = this_id), output)
    print(paste0("Completed ", which(this_id == u_ids), " of ", length(u_ids)))
    flush.console()
  }
  return(output)
}

#' plot_timestamp_spectrum
#'
#' This takes the output from timelist_to_difference and generates
#' a frequency density plot of the differences, which is akin to
#' a freqency ~ amplitude plot used in spectral analysis.
#' @param df a data frame that contains at least 1 column called 'intervals' of duration type
#' @param trans x-axis transformation, defaults to 'log10'
#' @param ... Extra parameters passed to the geom_density geom
#' @keywords plot spectrum histogram time frequency
#' @export plot_timestamp_spectrum
plot_timestamp_spectrum <- function(df, trans = 'log10', group = NULL, color = NULL, lower_x_lim = 0.1, ...) {
  # This function could use some optimisation
  if("intervals" %!in% names(df)) stop("No column named 'intervals' in data frame")
  df$intervals = as.numeric(df$intervals) # Histogram didn't like date types

  df <- df %>%
    filter(intervals > 0) # ignoring 0 duration events

  g <- ggplot(data = df, aes(x = intervals)) +
    geom_density(aes(group = !!enquo(group), color = !!enquo(color)), ...) +
    scale_x_continuous(name = "Interval",
                       limits = c(lower_x_lim, NA),
                       trans = trans,
                       breaks = c(0.1,1,10,60, 600, 60*60, 60*60*24, 60*60*24*7, 60*60*24*7*52/12.0, 60*60*24*7*26),
                       labels = c("0.1s","1s", "10s", "1m", "10m", "1h", "1d", "1w", "1M", "6M")) +
    xmin(0.1)
    scale_y_continuous(name = "Amplitude") +
    theme_minimal() +
    theme(axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  return(g)
}

#' aa_summary_by_user
#'
#' This summarises 'activity' data, according to the fields:
#' id : A unique key for the user
#' session : A unique key for each 'session', which denotes a single login from a single user
#' timestamp : When the event occured
#'
#' The defaults have been setup to work with Blackboard DDA report data, hence some of the strange names.
#' @param aa dataframe of activity data. Must include column names corresponding to the other params
#' @param max_time_interval The cutoff for duration of any session (duration data type, accepts recognised strings). Any session registering above this is filtered out as it is likely to be someone just logging in and then walking away. Defaults to 2 days.
#' @keywords activity accumulator aa log summary user
#' @export aa_summary_by_user
aa_summary_by_user <- function(aa,
                             max_time_interval = "2 days") {
  if ("id" %!in% names(aa)) return("id field not in names of data frame")
  if ("session" %!in% names(aa)) return("session field not in names of data frame")
  if ("timestamp" %!in% names(aa)) return("timestamp field not in names of data frame")

  aa_sorted <- aa %>%
    filter(!is.na(id), as.numeric(session) > 0) %>%
    select(id, session, timestamp) %>%
    arrange(id, session, timestamp) # needs to be in order for duration calculations

  aa_sorted$duration <- c(aa_sorted$timestamp[2:nrow(aa_sorted)] - aa_sorted$timestamp[1:(nrow(aa_sorted)-1)],0)
  aa_sorted$same_session <- as.numeric(c(aa_sorted[2:nrow(aa_sorted),"session"] == aa_sorted[1:(nrow(aa_sorted)-1),"session"], FALSE))
  aa_sorted <- aa_sorted %>% mutate(duration = as.numeric(duration) * same_session) # in seconds
  aa_sorted$same_session <- NULL

  aa_grouped <- aa_sorted %>%
    select(id, session, duration) %>%
    group_by(id, session)

  user_summary <- aa_grouped %>%
      summarise(clicks = n(),
              time = sum(duration)) %>%
    filter(as.duration(time) < as.duration(max_time_interval)) %>%
    summarise(accesses = n(),
              mean_clicks_per_access = mean(clicks, na.rm = T),
              sd_clicks = sd(clicks, na.rm = T),
              median_time_per_access = as.duration(median(time, na.rm = T)),
              sd_time = as.duration(sd(time, na.rm = T)),
              total_time = as.duration(sum(time, na.rm = T)))

  return(user_summary)
  }

#' aa_summary_by_content
#'
#' This summarises 'activity' data, according to the fields:
#' id : A unique key for the user
#' session : A unique key for each 'session', which denotes a single login from a single user
#' timestamp : When the event occured
#' content : A collection of field names that describe what was accessed
#'
#' The defaults have been setup to work with Blackboard DDA report data, hence some of the strange names.
#' @param aa dataframe of activity data. Must include column names corresponding to the other params
#' @param content A character vector of the field names that define the content
#' @param max_time_interval The cutoff for duration of any session (duration data type, accepts recognised strings). Any session registering above this is filtered out as it is likely to be someone just logging in and then walking away. Defaults to 2 days.
#' @keywords activity accumulator aa log summary user
#' @export aa_summary_by_content
aa_summary_by_content <- function(aa,
                               content = c("event", "handle", "data", "content_pk1"),
                               max_time_interval = "2 days") {
  if ("id" %!in% names(aa)) return("id field not in names of data frame")
  if ("session" %!in% names(aa)) return("session field not in names of data frame")
  if ("timestamp" %!in% names(aa)) return("timestamp field not in names of data frame")
  if (any(content %!in% names(aa))) return("content fields missing from data frame")
  aa_sorted <- aa %>%
    filter(!is.na(id)) %>%
    select(c("id", "session", "timestamp", content)) %>% # selecting only what is required
    arrange(id, session, timestamp) # needs to be in order for duration calculations

  aa_sorted$duration <- c(aa_sorted$timestamp[2:nrow(aa_sorted)] - aa_sorted$timestamp[1:(nrow(aa_sorted)-1)],0)
  aa_sorted$same_session <- as.numeric(c(aa_sorted[2:nrow(aa_sorted),"session"] == aa_sorted[1:(nrow(aa_sorted)-1),"session"], FALSE))
  aa_sorted <- aa_sorted %>% mutate(duration = as.numeric(duration) * same_session) # in seconds
  aa_sorted$same_session <- NULL

  content_summary <- aa_sorted %>%
    group_by_at(content) %>%
    summarise(hits = n(),
              median_time = median(duration, na.rm = T),
              total_time = sum(duration, na.rm = T)) %>%
    arrange(desc(hits))

  return(content_summary)
}

#' aa_summary_by_session (session one)
#'
#' This summarises 'activity' data, according to the fields:
#' id : A unique key for the user
#' session : A unique key for each 'session', which denotes a single login from a single user
#' timestamp : When the event occured
#' content : A collection of field names that describe what was accessed
#'
#' The defaults have been setup to work with Blackboard DDA report data, hence some of the strange names.
#' @param aa dataframe of activity data. Must include column names corresponding to the other params
#' @param max_time_interval The cutoff for duration of any session (duration data type, accepts recognised strings). Any session registering above this is filtered out as it is likely to be someone just logging in and then walking away. Defaults to 2 days.
#' @keywords activity accumulator aa log summary user
#' @export aa_summary_by_session
aa_summary_by_session <- function(aa,
                               content = c("event", "handle", "data", "content_pk1"),
                               max_time_interval = "2 days") {
  if ("id" %!in% names(aa)) return("id field not in names of data frame")
  if ("session" %!in% names(aa)) return("session field not in names of data frame")
  if ("timestamp" %!in% names(aa)) return("timestamp field not in names of data frame")
  if (any(content %!in% names(aa))) return("content fields missing from data frame")
  aa_sorted <- aa %>%
    filter(!is.na(id)) %>%
    select(id, session, timestamp) %>% # selecting only what is required
    arrange(id, session, timestamp) # needs to be in order for duration calculations

  aa_sorted$duration <- c(aa_sorted$timestamp[2:nrow(aa_sorted)] - aa_sorted$timestamp[1:(nrow(aa_sorted)-1)],0)
  aa_sorted$same_session <- as.numeric(c(aa_sorted[2:nrow(aa_sorted),"session"] == aa_sorted[1:(nrow(aa_sorted)-1),"session"], FALSE))
  aa_sorted <- aa_sorted %>% mutate(duration = as.numeric(duration) * same_session) # in seconds
  aa_sorted$same_session <- NULL

  session_summary <- aa_sorted %>%
    group_by(session, id) %>%
    summarise(clicks = n(),
              time = as.duration(sum(duration))) %>%
    filter(time < as.duration(max_time_interval)) %>%
    ungroup() %>%
    summarise(total_accesses = n(),
              mean_clicks_per_access = mean(clicks, na.rm = T),
              median_time_per_access = median(time))

  return(session_summary)
}

