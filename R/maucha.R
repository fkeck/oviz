#' Maucha diagram
#' 
#' This function draws Maucha diagram to represent the main ion composition of a water sample.
#' 
#' @param x a vector or a matrix giving the concentrations of the ions in mgL^-1. 
#' The values must be ordered as K, Na, Ca, Mg, SO4, Cl, HCO3, CO3.
#' @param col a vector of R colors for each ion. Recycled if necessary.
#' @param labels a logical stating whether labels are displayed.
#' @param labels.col a vector of R colors for each label. Recycled if necessary.
#' @param polygon a logical stating whether the hexadecagon is drawn.
#' @param polygon.col a vector of R colors for the lines of each of the eight sector of the hexadecagon.
#' @param main a character string giving a main title for the diagrams.
#' @param ... other arguments to be passed to the \code{\link[graphics]{plot.default}} function.
#' 
#' @references Maucha, R. Hydrochemische Methoden in der Limnologie: mit besonderer Ber\"ucksichtigung
#' der Verfahren von LW Winkler. Schweizerbart, 1932.
#' 
#' @examples
#' data(ionwaters)


maucha <- function(x, col = NULL,
                   labels = TRUE, labels.col = 1,
                   polygon = FALSE, polygon.col = 1,
                   main = NULL, ...){
  
  if(is.null(col)){
    col <- c("#54c7fd", "#ffcd00", "#ff9600", "#ff2851",
             "#0076fe", "#41cd58", "#ff3823", "#8f8e94")
  }
  col <- rep_len(col, length.out = 8)
  polygon.col <- rep_len(polygon.col, length.out = 8)
  
  if(is.vector(x)){
    n.plot <- 1
    x.all <- t(as.matrix(x))
  } else {
    x.all <- as.matrix(x)
    n.plot <- nrow(x.all)
    if(is.null(main)){
      main <- row.names(x.all)
    }
  }
  
  par.mar.0 <- par("mar")
  par.mfrow.0 <- par("mfrow")
  par(mfrow = c(dimMosaic(n.plot)[1], dimMosaic(n.plot)[2]),
      mar = c(2, 0, 2, 0))
  
  for(j in 1:n.plot){
    
    x <- x.all[j, ]
    
    x[1] <- (x[1] * 1) / 39.0983
    x[2] <- (x[2] * 1) / 22.9898
    x[3] <- (x[3] * 2) / 40.0780
    x[4] <- (x[4] * 2) / 24.3050
    x[5] <- (x[5] * 2) / 96.0626
    x[6] <- (x[6] * 1) / 35.4530
    x[7] <- (x[7] * 1) / 61.0168
    x[8] <- (x[8] * 2) / 60.0089
    
    x[1:4] <- x[1:4] / sum(x[1:4]) * 100
    x[5:8] <- x[5:8] / sum(x[5:8]) * 100
    
    A  <- sum(x)
    R <- sqrt(((A / 16) * 2 / sin(22.5 * pi / 180)))
    a <- x / (R * sin(22.5 * pi / 180))
    
    
    coord.x <- matrix(NA, nrow = 5, ncol = 8)
    coord.y <- matrix(NA, nrow = 5, ncol = 8)
    coord.x[1, ] <- coord.x[5, ] <- rep(0, 8)
    coord.y[1, ] <- coord.y[5, ] <- rep(0, 8)
    coord.x[2, ] <- cos(seq(90, -225, -45) * pi / 180) * R
    coord.y[2, ] <- sin(seq(90, -225, -45) * pi / 180) * R
    coord.x[3, ] <- cos(seq(67.5, -247.5, -45) * pi / 180) * a
    coord.y[3, ] <- sin(seq(67.5, -247.5, -45) * pi / 180) * a
    coord.x[4, ] <- cos(seq(45, -270, -45) * pi / 180) * R
    coord.y[4, ] <- sin(seq(45, -270, -45) * pi / 180) * R
    
    
    if(polygon | labels){
      coord.poly.x <- coord.x
      coord.poly.x[3, ] <- cos(seq(67.5, -247.5, -45) * pi / 180) * R
      coord.poly.y <- coord.y
      coord.poly.y[3, ] <- sin(seq(67.5, -247.5, -45) * pi / 180) * R
    }
    
    if(labels){
      lab <- c(expression("K"^"+"), expression("Na"^"+"), expression("Ca"^"2+"), expression("Mg"^"2+"), 
               expression("SO"["4"]^"2-"), expression("Cl"^"-"), expression("HCO"["3"]^"-"), expression("CO"["3"]^"2-"))
      lab.pos.x <- coord.x[3, ]
      lab.pos.y <- coord.y[3, ]
      lab.pos.x[a < R] <- coord.poly.x[3, a < R]
      lab.pos.y[a < R] <- coord.poly.y[3, a < R]
      lab.pos.x <- lab.pos.x + cos(seq(67.5, -247.5, -45) * pi / 180) * 2
      lab.pos.y <- lab.pos.y + sin(seq(67.5, -247.5, -45) * pi / 180) * 2
    }
    
    
    xlim <- max(abs(c(min(lab.pos.x), max(lab.pos.x))))
    ylim <- max(abs(c(min(lab.pos.y), max(lab.pos.y))))
    plot(0, 0, type = "n", xlim = c(-xlim, xlim), ylim = c(-ylim, ylim),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", asp = 1, bty = "n", main = main[j], ...)
    
    for(i in 1:8){
      polygon(x = coord.x[, i], y = coord.y[, i], col = col[i], border = NA)
      if(polygon){
        lines(x = coord.poly.x[, i], y = coord.poly.y[, i], col = polygon.col[i])
      }
    }
    
    if(labels){
      text(lab.pos.x, lab.pos.y, lab, col = labels.col)
    }
  }
  par(mar = par.mar.0, mfrow = par.mfrow.0)
}
