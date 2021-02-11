var.ml <-
function(N,S,k) {
  Ck <- 1-(1-k)^S
  num <- Ck*k*(1-k)
  num <- num*num
  den <- N*(1-2*k)*Ck*Ck + k*N*Ck*(1-((1-k)^S)*(1+k*S)) + N*S*((1-k)^S)*(Ck-S)*(k^2)
  Vk <- num/den
  return(Vk)
}
