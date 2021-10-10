# Title: Angle between point 1 and point 2
# Author: Stackoverflow
# Date: 2021-09-25

#calculates the angle between two points
angle <- function(x,y) { 
  z <- x + 1i * y
  res <- 90 - Arg(z) / pi * 180
  res %% 360
}