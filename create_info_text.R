create_info_text <- function(df) {
  latitude_text <- paste("<p><strong><em>Latitude:&nbsp;</em></strong>",df$latitude,"</p>", sep = "")
  longitude_text <- paste("<p><strong><em>Longitude:&nbsp;</em></strong>",df$longitude,"</p>", sep = "")
  address_text <- paste("<p><strong><em>Approximate Address:&nbsp;</em></strong>",df$place,"</p>", sep = "")
  time_text <- paste("<p><strong><em>Time:&nbsp;</em></strong>",df$time,"</p>", sep = "")
  magnitude_text <- paste("<p style=\"color:",magnitude_color(df$mag),";\"><strong><em>Magnitude:&nbsp;</em></strong>",df$mag,"</p>", sep = "")
  depth_text <- paste("<p><strong><em>Depth:&nbsp;</em></strong>",df$depth,"</p>", sep = "")
  
  return(paste(latitude_text,longitude_text,address_text,time_text,magnitude_text,depth_text))
}