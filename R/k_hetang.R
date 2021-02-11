k_hetang <-
function(x) {
  nz <- sum(x==0)
  if(nz!=0) {
    cat(nz,"zeroes excluded\n")
    x <- x[x!=0]
  }
  N  <- sum(x)
  S  <- length(x)
  ma <- max(x)
  mi <- min(x)
  y  <- 1-(mi/ma)^(1/(S-1))
  return(y)
}
