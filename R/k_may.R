k_may <-
function(xs,exclude=TRUE) {
  if(exclude) xs <- xs[xs!=0]
  out <- uniroot(fmay,c(0.001,0.999),xs)
  khat <- out$root
  return(khat)
}
