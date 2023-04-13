magnitude_color <- function(magnitudes) {
  maxColorValue <- 100
  palette <- colorRampPalette(c("white","green", "yellow", "orange", "red","darkred"))(maxColorValue)
  indices <- as.integer(magnitudes * 10)
  indices[indices <= 0] <- 1
  
  return(palette[indices])
}