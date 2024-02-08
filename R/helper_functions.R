#' returns co-ordinates for point labels while trying to spread points from eachother and x and y axis
#'
#' @param n number of points requiring label
#' @param xmin min x-coord
#' @param xmax maxx x-coord
#' @param ymin min y-coord
#' @param ymax max y-coord
#' @param min_distance miin distance between labels
#' @param random_state random state
#'
#' @return poistions for labels
#' @export
#'
#' @examples 
#' rand_coord_opt2(n = 1, 
#' xmin = 40, 
#' xmax = 100, 
#' ymin = 40, 
#' ymax = 100, 
#' min_distance = 2, 
#' random_state = 12)
rand_coord_opt2 <- function(n, xmin, xmax, ymin, ymax, min_distance, random_state = 12) {
  set.seed(random_state)
  # result <- numeric(n)
  result <- data.frame(x = numeric(), y = numeric())
  
  if (n > 0){
    for (i in 1:n){
      
      if (i == 1){
        attempts <- 0
        repeat {
          coord <- data.frame(x = stats::runif(1, xmin, xmax), y = stats::runif(1, ymin, ymax))
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
          coord <- data.frame(x = stats::runif(1, xmin, xmax), y = stats::runif(1, ymin, ymax))
          
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


#' dotted line bisecting y axis
#'
#' @param y coordinate to place line
#' @param colour colour of line
#'
#' @return line to add to plotly
#' @export
#'
#' @examples hline(0)
hline <- function(y = 0, colour = "#4d4d4d") {
  list(
    type = "line", 
    x0 =0.1,
    x1 = 400000, 
    # xref = "paper",
    y0 = y, 
    y1 = y, 
    width = 0.5,
    line = list(color = colour, dash = "dash")
  )
}