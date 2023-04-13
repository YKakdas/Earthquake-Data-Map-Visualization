show_statistics <- function(df) {
  cat("\n\nStatistics about retrieved earthquake data\n")
  cat("Number of found earthquakes is: ", nrow(df), "\n")
  cat("The boundary box of found earthquakes are as follows\n")
  cat("\t\tLongitudes from ", min(df$longitude), " to ", max(df$longitude), "\n")
  cat("\t\tLatitudes from ", min(df$latitude), " to ", max(df$latitude), "\n")
  cat("Summary statistics of magnitude of earthquakes are as follows\n")
  cat("\t\tMinimum magnitude: ", min(df$mag, na.rm = T), "\n")
  cat("\t\tMaximum magnitude: ", max(df$mag, na.rm = T), "\n")
  cat("\t\tMean magnitude: ", mean(df$mag, na.rm = T), "\n")
  cat("Summary statistics of the time of the earthquakes are as follows\n")
  
  df_time_sorted <- df %>% 
    arrange(time)
  
  cat("\t\tThe first earthquake in the query happened on ", as.character(df_time_sorted$time[1]), "\n")
  cat("\t\tThe last earthquake in the query happened on ", as.character(df_time_sorted$time[length(df_time_sorted$time)]), "\n")
}