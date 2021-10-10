cardinal <- function(x){
  if(!is.na(x)) {
    if(x <= 11.25 & x >= 0) {return("N")}
    if(x > 11.25 & x <= 33.75) {return("NNE")}
    if(x > 33.75 & x <= 56.25) {return("NE")}
    if(x > 56.25 & x <= 78.75) {return("ENE")}
    if(x > 78.75 & x <= 101.25) {return("E")}
    if(x > 101.25 & x <= 123.75) {return("ESE")}
    if(x > 123.75 & x <= 146.25) {return("SE")}
    if(x > 146.25 & x <= 168.75) {return("SSE")}
    if(x > 168.5 & x <= 180) {return("S")}
    if(x < -168.5 & x >= -180) {return("S")}
    if(x < -146.25 & x >= -168.5) {return("SSW")}
    if(x < -123.75 & x >= -146.25) {return("SW")}
    if(x < -101.25 & x >= -123.75) {return("WSW")}
    if(x < -78.75 & x >= -101.25) {return("W")}
    if(x < -56.25 & x >= -78.75) {return("WNW")}
    if(x < -33.75 & x >= -56.25) {return("NW")}
    if(x < -11.25 & x >= -33.75) {return("NNW")}
    if(x < 0 & x >= -11.25) {return("N")}
  } else {
    return(NA)
  }
}