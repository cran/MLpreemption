preemption.t <-
function(x1,x2,verbose=TRUE) {
  S1 <- length(x1)
  S2 <- length(x2)
  N1 <- sum(x1)
  N2 <- sum(x2)
  k1.ml <- k_ml(x1)
  k2.ml <- k_ml(x2)
  vml1 <- var.ml(N1,S1,k1.ml)
  vml2 <- var.ml(N2,S2,k2.ml)
  num <- k1.ml-k2.ml
  den <- sqrt(vml1 + vml2)
  Tstat <- num/den
  
  num.df <- (vml1 + vml2)^2
  den.df <- (vml1^2)/N1 + (vml2^2)/N2
  df <- num.df/den.df
  
  pval <- 2*pt(abs(Tstat),df=df,lower.tail = FALSE)
  if(verbose) {
    cat("Preemption t test\n")
    cat("H0: k1 = k2 vs. H1 k1 <> k2\n")
    cat("T = ",Tstat,"\n")
    cat("df = ",df,"\n")
    cat("p-value =",pval,"\n")
  }
  
  return(list(Tstat=Tstat,pval=pval,df=df))
}
