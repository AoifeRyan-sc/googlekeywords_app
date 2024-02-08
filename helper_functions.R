rand_coord_opt2 <- function(n, xmin, xmax, ymin, ymax, min_distance, random_state = 12) {
  set.seed(random_state)
  # result <- numeric(n)
  result <- data.frame(x = numeric(), y = numeric())
  
  if (n > 0){
    for (i in 1:n){
      
      if (i == 1){
        attempts <- 0
        repeat {
          coord <- data.frame(x = runif(1, xmin, xmax), y = runif(1, ymin, ymax))
          if (abs(coord[2] - 0) > 20){
            result[i,] <- coord
            break
          }
          
          attempts <- attempts + 1
          if (attempts > 200){
            warning("Unable to find a suitable number, adjust parameters.")
            result[i,] <- coord
            break
          }
        }
        
      } else {
        attempts <- 0
        
        repeat {
          coord <- data.frame(x = runif(1, xmin, xmax), y = runif(1, ymin, ymax))
          
          if (all(apply(result, 1, function(row) abs(coord - row) > min_distance))&
              abs(coord[2] - 0) > 40){
            result <- rbind(result, coord)
            break
          } 
          attempts <- attempts + 1
          if (attempts > 100){
            warning("Unable to find a suitable number, adjust parameters.")
            result <- rbind(result, coord)
            break
          }
        }
      }
      
    }
  }
  # return(data.frame(result))
  return(result)
}
hline <- function(y = 0, color = "#4d4d4d") {
  list(
    type = "line", 
    x0 =0.1,
    x1 = 400000, 
    # xref = "paper",
    y0 = y, 
    y1 = y, 
    width = 0.5,
    line = list(color = color, dash = "dash")
  )
}