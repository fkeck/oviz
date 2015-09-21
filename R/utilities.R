
dimMosaic <- function(n.plot){
  x <- floor(sqrt(n.plot))
  y <- floor(sqrt(n.plot))
  if(x * y < n.plot) y <- y + 1
  if(x * y < n.plot) x <- x + 1
  return(c(x, y))
}