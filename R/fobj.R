fobj <-
function(k=0.50,xs) {
  N <- sum(xs)
  S <- length(xs)
  ivec <- 1:S
  aux <- S*log(1-k)
  y <- N - k*sum(ivec*xs) - exp(aux)*(k*N*S + N - k*sum(ivec*xs))
  return(y)
}
