fmay <-
function(k=0.5,x) {
  nz <- sum(x==0)
  if(nz!=0) {
    cat(nz,"zeroes excluded\n")
    x <- x[x!=0]
  }
  S <- length(x)
  nm <- min(x)
  N <- sum(x)
  fa <- (S-1)*log(1-k)
  y <- exp(fa)*(k*(N-nm) + nm) - nm
}
