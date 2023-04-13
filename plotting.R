plot_earthquakes <-
  function(address = NULL,
           starttime = NULL,
           endtime = NULL,
           lat = NULL,
           lon = NULL,
           maxradiuskm = 200,
           minmagnitude = NULL,
           show_info_window_on_click = F,
           show_info_window_on_hover = F,
           show_bounding_box = F) {
    
    query <- create_query(address, starttime, endtime, lat, lon, maxradiuskm, minmagnitude)
    
    url <- query$url
    
    cat(query$message)
    earthquake_data <- read_csv(url, show_col_types = F)
    print(earthquake_data)
    
    if (nrow(earthquake_data) == 0) {
      cat("Couldn't find any earthquakes for given parameters")
    } else {
      earthquake_data$info_window <- create_info_text(earthquake_data)
      earthquake_data$radius <- earthquake_data$mag * 1000
      earthquake_data$color <- magnitude_color(earthquake_data$mag)
      
      mean_long <- mean(earthquake_data$longitude)
      mean_lat <- mean(earthquake_data$latitude)
      
      info_window_on_click <- NULL
      info_window_on_hover <- NULL
      
      if(show_info_window_on_click) info_window_on_click <- "info_window"
      else if(show_info_window_on_hover) info_window_on_hover <- "info_window"
      
      map <- google_map(location = c(mean_lat, mean_long), key = api_secret) %>%
        add_circles(
          data = earthquake_data,
          lon = "longitude",
          lat = "latitude",
          fill_colour = "color",
          stroke_colour = "#000000",
          radius = "radius",
          info_window = info_window_on_click,
          mouse_over = info_window_on_hover,
          fill_opacity = "mag"
        )
      
      if (show_bounding_box) {
        bbox_df <-
          data.frame(
            north = max(earthquake_data$latitude) + 0.1,
            south = min(earthquake_data$latitude) - 0.1,
            east = max(earthquake_data$longitude) + 0.1,
            west = min(earthquake_data$longitude) - 0.1
          )
        map <- map %>% add_rectangles(
          data = bbox_df,
          north = 'north',
          south = 'south',
          east = 'east',
          west = 'west'
        )
      }
      show_statistics(earthquake_data)
      map
    }
  }