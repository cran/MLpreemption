k_ml <-
function(xs,closed=FALSE,ll=0.001,ul=0.999) {
  if(closed) {
    S <- length(xs)
    i <- 1:S
    N <- sum(xs)
    khat <- N/(sum(xs*i))
  } else {
    out <- uniroot(fobj,c(ll,ul),tol=1e-20,check.conv=TRUE,xs)
    khat <- out$root
  }
  return(khat)
}
