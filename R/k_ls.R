k_ls <-
function(x) {
  nz <- sum(x==0)
  if(nz!=0) {
    cat(nz,"zeroes excluded\n")
    x <- x[x!=0]
  }
  S <- length(x)
  N <- sum(x)
  xr <- x/N
  y <- log(xr)
  ivec <- 1:S
  lm.out <- lm(y~ivec)
  aic <- AIC(lm.out)
  logl <- logLik(lm.out)
  b <- coefficients(lm.out)
  khat <- 1-exp(b[2])
  se <- sqrt(diag(vcov(lm.out)))
  df <- S-2
  b.ul <- b[2] + qt(0.975,df)*se[2]
  b.ll <- b[2] - qt(0.975,df)*se[2]
  k.ll <- 1-exp(b.ul)
  k.ul <- 1-exp(b.ll)
  return(list(khat=khat,k.ul=k.ul,k.ll=k.ll,aic=aic,logl=logl))
}
