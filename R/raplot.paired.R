raplot.paired <- function(x,y,xlab="Species rank",ylab="log (Relative abundance)",
                    main="Rank-abundance",
                    sym=c(1,2),alpha=0.05) {

  nz <- max(sum(y!=0),sum(x!=0))
  
  Sx <- length(x)
  Nx <- sum(x)
  indexx <- order(x, decreasing=TRUE)
  rel.abundance.sorted.x <- x[indexx]/Nx
  log.rel.abu.x <- log(rel.abundance.sorted.x)
  speciesrank.x <- 1:Sx
  
  ppar <- par(pty="s")
  on.exit(par(ppar))     
  plot(speciesrank.x[1:nz],log.rel.abu.x[1:nz],pch=sym[1],main=main,
       xlab=xlab,ylab=ylab,col="red")
  xs <- sort(x,decreasing = TRUE)
  k.ml.x <- k_ml(xs)
  lpi.ml.x <- estim(Sx,k.ml.x)
  
  Sy <- length(y)
  Ny <- sum(y)
  indexy <- order(y, decreasing=TRUE)
  rel.abundance.sorted.y <- y[indexy]/Ny
  log.rel.abu.y <- log(rel.abundance.sorted.y)
  speciesrank.y <- 1:Sy
  points(speciesrank.y[1:nz],log.rel.abu.y[1:nz],pch=sym[2],col="blue")
  ys <- sort(y,decreasing = TRUE)
  k.ml.y <- k_ml(ys)
  lpi.ml.y <- estim(Sy,k.ml.y)
  
  #
  # uncertainty zone
  #
  vml <- var.ml(Nx,Sx,k.ml.x)
  ml.ll.x <- k.ml.x - qnorm(1-alpha/2)*sqrt(vml)
  ml.ul.x <- k.ml.x + qnorm(1-alpha/2)*sqrt(vml)
  slope.seq <- seq(ml.ll.x,ml.ul.x,by=0.001)
  i <- 1:Sx
  
  vml.y <- var.ml(Ny,Sy,k.ml.y)
  ml.ll.y <- k.ml.y - qnorm(1-alpha/2)*sqrt(vml.y)
  ml.ul.y <- k.ml.y + qnorm(1-alpha/2)*sqrt(vml.y)
  slope.seq.y <- seq(ml.ll.y,ml.ul.y,by=0.001)
  i <- 1:Sy
  
  
  for(j in 1:length(slope.seq)) {
    lpi.ml.lims.x <- estim(Sx,slope.seq[j])
    segments(i[1],lpi.ml.lims.x[1],i[Sx],lpi.ml.lims.x[Sx],col="grey",lwd=2)
    
    lpi.ml.lims.y <- estim(Sy,slope.seq.y[j])
    segments(i[1],lpi.ml.lims.y[1],i[Sy],lpi.ml.lims.y[Sy],col="grey",lwd=2)
  }
  
  segments(i[1],lpi.ml.x[1],i[Sx],lpi.ml.x[Sx],col="red")    # ML sample 1
  segments(i[1],lpi.ml.y[1],i[Sx],lpi.ml.y[Sx],col="blue")    # ML sample 2
  
  points(speciesrank.x[1:nz],log.rel.abu.x[1:nz],pch=sym[1],col="red")
  
  points(speciesrank.y[1:nz],log.rel.abu.y[1:nz],pch=sym[2],col="blue")
  
  par(ppar)
}
