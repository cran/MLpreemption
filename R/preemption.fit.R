preemption.fit <- function(x,method="ml",closed=FALSE,verbose=TRUE) {
  if(method == "ml") {
    khat <- k_ml(x,closed=closed)
    N <- sum(x)
    S <- length(x)
    vml <- var.ml(N,S,khat)
    ll  <- khat - qnorm(0.975)*sqrt(vml)
    ul  <- khat + qnorm(0.975)*sqrt(vml)
  } else if (method == "ls") {
    out.ls <- k_ls(x)
    khat <- out.ls$khat
    ll <- out.ls$k.ll
    ul <- out.ls$k.ul
  } else if (method == "May") {
    khat <- k_may(x)
    ll <- NA
    ul <- NA
  } else if (method == "HeTang") {
    khat <- k_hetang(x)
    ll <- NA
    ul <- NA
  } else if (method == "all") {
    k1 <- k_ml(x,closed=closed)
    k3 <- k_may(x)
    k4 <- k_hetang(x)
    khat <- k1
    N <- sum(x)
    S <- length(x)
    vml <- var.ml(N,S,khat)
    ll.ml  <- khat - qnorm(0.975)*sqrt(vml)
    ul.ml  <- khat + qnorm(0.975)*sqrt(vml)
    ll <- ll.ml
    ul <- ul.ml
    out.ls <- k_ls(x)
    k2 <- out.ls$khat
    ll.ls <- out.ls$k.ll
    ul.ls <- out.ls$k.ul
  } else {
    method <- "ml"
    khat <- k_ml(x,closed=closed) # ML used by default
    vml <- var.ml(N,S,khat)
    ll  <- khat - qnorm(0.975)*sqrt(vml)
    ul  <- khat + qnorm(0.975)*sqrt(vml)
  }
  if(verbose) {
    if(method != "all") {
      cat("Niche preemption model (geometric series)\n")
      cat(paste("Estimation method",method,"\n"))
      cat("k = ",khat,"\t95% CI = (",ll,",",ul,")\n") 
    } else {
      cat("Niche preemption model (geometric series)\n")
      cat("ML estimate:      k = ",k1,"95% CI = (",ll.ml,",",ul.ml,")\n")
      cat("LS estimate:      k = ",k2,"95% CI = (",ll.ls,",",ul.ls,")\n")
      cat("May's estimate:   k = ",k3,"\n")
      cat("He-Tang estimate: k = ",k4,"\n")
    }
  }
  return(list(khat=khat,ll=ll,ul=ul))
}
