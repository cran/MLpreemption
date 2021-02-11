raplot <- function(x,xlab="Species rank",ylab="log (Relative abundance)",
                   main="Rank-Abundance plot",
                   reflines=c(1,2,3,4),alpha=0.05,leg=FALSE) {
  S <- length(x)
  N <- sum(x)
  index <- order(x, decreasing=TRUE)
  rel.abundance.sorted <- x[index]/N
  log.rel.abu <- log(rel.abundance.sorted)
  speciesrank <- 1:S
  plot(speciesrank,log.rel.abu,pch=1,main=main,xlab=xlab,ylab=ylab)
  xs <- sort(x,decreasing = TRUE)
  k.ml <- k_ml(xs)
  k.ht <- k_hetang(xs)
  k.ls <- k_ls(xs)$khat
  k.ma <- k_may(xs)
  lpi.ml <- estim(S,k.ml)
  lpi.ma <- estim(S,k.ma)
  lpi.ht <- estim(S,k.ht)
  lpi.ls <- estim(S,k.ls)
  
  #
  # uncertainty zone
  #
  vml <- var.ml(N,S,k.ml)
  ml.ll <- k.ml - qnorm(1-alpha/2)*sqrt(vml)
  ml.ul <- k.ml + qnorm(1-alpha/2)*sqrt(vml)
  slope.seq <- seq(ml.ll,ml.ul,by=0.001)
  i <- 1:S
  
  for(j in 1:length(slope.seq)) {
    lpi.ml.lims <- estim(S,slope.seq[j])
    segments(i[1],lpi.ml.lims[1],i[S],lpi.ml.lims[S],col="grey",lwd=2)
  }
  
  if(1 %in% reflines) segments(i[1],lpi.ml[1],i[S],lpi.ml[S],col="red")    # ML
  if(2 %in% reflines) segments(i[1],lpi.ma[1],i[S],lpi.ma[S],col="green")  # May
  if(3 %in% reflines) segments(i[1],lpi.ht[1],i[S],lpi.ht[S],col="blue")   # H-T
  if(4 %in% reflines) segments(i[1],lpi.ls[1],i[S],lpi.ls[S],col="black")  # LS
  
  points(speciesrank,log.rel.abu,pch=1)
  if(leg) legend("topright",c("ML","May","He-Tang","LS"),lty=c(1,1),
                 col=c("red","green","blue","black"),cex=0.75)
}
