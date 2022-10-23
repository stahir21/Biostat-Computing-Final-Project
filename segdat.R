##' Plots a dataset (x,y) with segments of interest in different colors
##'
##' This function plots a given data set (x, y) and allows the user to specify segments that will be of different colors. The labels for the segments are displayed in the legend.
##' @title data plot with colored segments
##' @param x a vector of x values of the data
##' @param y a vector of y values of the data
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
##' data(ecg_data)
##' y <- ecg_data[168:194,2]
##' x <- ecg_data[168:194,1]
##' segdat(x,y, title="Raw ECG Data - QRS complex", xl="index", yl="ECG signal", nseg=3,
##'     segs=c(170,175,176,185,187,192), segcols=c("red","blue","green"),
##'     seglabs=c("P wave","QRS complex","T wave"))
##'
##' y <- ecg_data[1000:1500,2]
##' x <- ecg_data[1000:1500,1]
##' segdat(x,y, title="Raw ECG Data - Peaks", xl="index", yl="ECG signal", nseg=4,
##'    segs=c(1020,1060,1160,1185,1285, 1310,1400,1450), segcols=c("red","blue","green", "yellow"),
##'    seglabs=c("Peak 1","Peak 2","Peak 3", "Peak 4"))




segdat <- function (x, y, title, xl, yl, nseg, segs, segcols, seglabs){

  ##create initial plot
  par(oma = c(1, 1, 1, 8))
  plot(x,y, type="l", main=title, xlab=xl, ylab=yl,lwd=2)
    usr <- par("usr")

  ## minimum value
  ymin <- min(y)
  xmin <- x[which.min(y)]
  min <- paste(round(c(xmin, ymin), 2), collapse=",")
  points(xmin,ymin, col="red", pch=19)

  ## maximum value
  ymax <- max(y)
  xmax <- x[which.max(y)]
  max <- paste(round(c(xmax, ymax),2), collapse=",")
  points (xmax, ymax, col="blue", pch=19)

  ## create matrix for segment pairs
    segmat <- matrix(segs,nrow=nseg, ncol=2, byrow=TRUE)

  ## add segments to initial plot
    for (i in 1:nseg) {
    clip(segmat[i,1], segmat[i,2], usr[3], usr[4])
    lines(x,y,type="l", col=segcols[i], lwd=2)
  }

  ## create empty plot to add legend
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

  legend("topright", legend=c(seglabs, paste("min:", min), paste("max:", max)),
         col=c(segcols, "red", "blue"), inset=c(0,0.1), xpd = TRUE,
         bty = "n", lty=c(rep(1, nseg), NA, NA), pch = c(rep(NA, nseg), 19, 19), lwd=2)
}
