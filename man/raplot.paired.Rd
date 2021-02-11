\name{raplot.paired}
\alias{raplot.paired}
\title{
Rank-abundance plot for two samples
}
\description{
Function \code{raplot.paired} creates a rank-abundance plot on screen, and can show decaying lines with uncertainty zones for two samples fitted by maximum likelihood.
}
\usage{
raplot.paired(x, y, xlab = "Species rank", ylab = "log (Relative abundance)",
              main = "Rank-abundance", sym = c(1, 2), alpha = 0.05)
}
\arguments{
  \item{x}{Count vector of the first sample}
  \item{y}{Count vector of the second sample}
  \item{xlab}{Label x axis}
  \item{ylab}{Label y axis}
  \item{main}{Main title for the plot}
  \item{sym}{Symbols for first and second sample (c(1,2) by default)}
  \item{alpha}{Significance level (0.05 by default)}
}
\value{NULL}
\references{
Graffelman, J. (2021) Maximum likelihood estimation of the geometric niche preemption model
}
\author{ Jan Graffelman (jan.graffelman@upc.edu) }
\seealso{
\code{\link{raplot}}
}
\examples{
data("Mehrabi")
raplot.paired(Mehrabi[,1],Mehrabi[,2])
}
\keyword{aplot}
