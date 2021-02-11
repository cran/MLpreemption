estim <-
function(S,k) {
  i <- 1:S
  Ck <- 1 - (1-k)^S
  pi <- (k*(1-k)^(i-1))/Ck
  lpi <- log(pi)
  return(lpi)
}
