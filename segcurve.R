##' Plot a function with segments of interest in different colors
##'
##' This function plots a function (or piecewise function) and allows the user to specify segments that will be of different colors. The labels for the segments are displayed in the legend.
##' @title plot of function with colored segments
##' @param f gives the expression for the function, function should be defined outside of segcurve(), eg. f <- function (x) {sin(x)}
##' @param xrange a vector specifying the domain of the plot, eg. xrange <- c(x1, x2)
##' @param title title of the plot, should be given in quotes "", eg. title="figure title"
##' @param xl label of the x-axis, should be given in quotes "", eg. xl="xlabel"
##' @param yl label of the y-axis, should be given in quotes "", eg. yl="ylabel"
##' @param nseg specifies the number of segments of interest
##' @param segs a vector that lists the starting and ending x values of the segments, should be 2*nseg in length, eg. if you want to color segments (1,3) and (7,11), segs=c(1,3,7,11)
##' @param segcols a vector that lists the color for each segment, should be same length as nseg, eg. segcols=c("red", "blue",..)
##' @param seglabs a vector that lists the label of each segment, should be same length as nseg, eg. seglabs = c("segment 1", "segment 2",..)
##' @return a line plot that includes segments in different colors and also plots the maximum and minimum points of the data. The legend displays the labels for the segments and the values of the minimum and maximum points.
##' @author Sumia Tahir
##' @import "graphics"
##' @import "stats"
##' @export
##' @examples
##' f <- function (x) {ifelse(x >= 3, x^2, ifelse(x > -4 & x < 3,  2*x+3, 5*x^3/64))}
##'segcurve(f, xrange=c(-10,10), nseg=3, segs=c(-10,-4,-4,3,3,10), segcols=c("red", "blue", "green"),
##'seglabs=c("5*x^3/64", "2*x + 3", "x^2"), title="Piecewise function", xl="x", yl="f(x)")
##'
##' f <- function(x) {sin(x)}
##' segcurve(f, xrange=c(-10,10), nseg=3, segs=c(-4,-1,0.5,2.5,6,8), segcols=c("red", "blue", "green"),
##' seglabs=c("segment 1", "segment 2", "segment 3"), title="Sine plot", xl="x", yl="sin(x)")
##'

segcurve <- function (f, xrange, title, xl, yl, nseg, segs, segcols, seglabs){

  ##plot initial curve
  par(oma = c(1, 1, 1, 8))
  curve(f, from=xrange[1], to=xrange[2], main=title, xlab=xl, ylab=yl, lwd=2)
  usr <- par("usr")

  ## minimum value of function within interval specified
  miny <- optimize(f, xrange, tol=0.00001)
  ymin <- miny$objective
  xmin <- miny$minimum
  min <- paste(round(c(xmin, ymin), 2), collapse=",")
  points(xmin, ymin, col="red", pch=19)

  ## maximum value of function within interval specified
  maxy <- optimize(f, xrange, tol=0.00001, maximum=TRUE)
  ymax <- maxy$objective
  xmax <- maxy$maximum
  max <- paste(round(c(xmax, ymax),2), collapse=",")
  points(xmax, ymax, col="blue", pch=19)

  ##create matrix for segment pairs
  segmat <- matrix(segs,nrow=nseg, ncol=2, byrow=TRUE)

  ## add segments of different colours
  for (i in 1:nseg) {
    clip(segmat[i,1], segmat[i,2], usr[3], usr[4])
    curve(f, col=segcols[i], add=T, lwd=2)
  }
  ## create empty plot to add legend
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

  legend("topright", legend=c(seglabs, paste("min:", min), paste("max:", max)),
         col=c(segcols, "red", "blue"), inset=c(0,0.1), xpd = TRUE,
         bty = "n", lty=c(rep(1, nseg), NA, NA), pch = c(rep(NA, nseg), 19, 19), lwd=2)
}

