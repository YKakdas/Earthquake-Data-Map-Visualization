library(lubridate)
library(tidygeocoder)

create_query <- function(address = NULL, starttime = NULL, endtime = NULL,lat = NULL, lon = NULL, maxradiuskm = 200, minmagnitude = NULL) {
  message <- "Fetching the earthquake data for the given parameters"
  base_query <- "https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv"
  
  lat_query_param <- "&latitude="
  lon_query_param <- "&longitude="
  starttime_query_param <- "&starttime="
  endtime_query_param <- "&endtime="
  maxradiuskm_query_param <- "&maxradiuskm="
  minmagnitude_query_param <- "&minmagnitude="
  
  address_text <- address
  
  
  if (!is.null(address)) {
    address <- geocode_OSM(address, as.data.frame = T)
    lon <- address$lon
    lat <- address$lat
  }
  
  if (!is.null(lon) && !is.null(lat)) {
    lat_query_param <- paste(lat_query_param, lat, sep = "")
    lon_query_param <- paste(lon_query_param, lon, sep = "")
    base_query <- paste(base_query, lat_query_param, lon_query_param, sep = "")
    
    maxradiuskm_query_param <- paste(maxradiuskm_query_param, maxradiuskm, sep = "")
    base_query <- paste(base_query, maxradiuskm_query_param, sep = "")
    
    if(is.null(address)){
      address <- reverse_geo(lat, lon)$address
    }
    
  }
  
  if (is.null(address) || is.na(address)) {
    message <- paste(message, "No address has been specified. Looking for the eartquakes from all over the world", sep = "\n")
  } else{
    address_message <- paste("Looking for the earthquakes for ",maxradiuskm, " km around ", address_text, sep = "")
    message <- paste(message, address_message, sep = "\n")
  }
  
  if (!is.null(starttime)) {
    starttime <- parse_date_time(starttime, orders = c("dmy","dym","myd","mdy","ydm","ymd"))
    starttime_query_param <- paste(starttime_query_param, starttime, sep = "")
    base_query <- paste(base_query, starttime_query_param, sep = "")
  }
  
  if (!is.null(endtime)) {
    endtime <- parse_date_time(endtime, orders = c("dmy","dym","myd","mdy","ydm","ymd"))
    endtime_query_param <- paste(endtime_query_param, endtime, sep = "")
    base_query <- paste(base_query, endtime_query_param, sep = "")
  }
  
  if(is.null(starttime) && is.null(endtime)){
    message <- paste(message, "No start or end time has been specified. Showing results for the most up to date earthquakes", sep = "\n")
  } else{
    if (!is.null(starttime)) {
      temp <- paste("Showing earthquakes starting from the date ", starttime, sep = "")
      message <- paste(message, temp, sep = "\n")
    } else if(endtime) {
      temp <- paste("Showing earthquakes ended before the date ", endtime, sep = "")
      message <- paste(message, temp, sep = "\n")
    }
  }
  
  if (!is.null(minmagnitude)) {
    minmagnitude_query_param <- paste(minmagnitude_query_param, minmagnitude, sep = "")
    base_query <- paste(base_query, minmagnitude_query_param, sep = "")
    temp <- paste("Showing earthquakes with the minimum magnitude of ", minmagnitude, sep = "")
    message <- paste(message, temp, sep = "\n")
  }

  return(list("url" = base_query, "message" = message))
}
