\name{raplot}
\alias{raplot}
\title{
Rank-abundance plot
}
\description{
Function \code{raplot} creates a rank-abundance plot online, and can show decaying lines fitted by various method.
}
\usage{
raplot(x, xlab = "Species rank", ylab = "log (Relative abundance)",
       main = "Rank-Abundance plot", reflines = c(1, 2, 3, 4), alpha = 0.05, leg = FALSE)
}
\arguments{
  \item{x}{Vector of counts (species abundances)}
  \item{xlab}{Label for the x axis}
  \item{ylab}{Label for the y axis}
  \item{main}{Title for the plot}
  \item{reflines}{Lines to be drawn in the plot: 1=ML, 2=LS, 3=May, 4=He-Tang}
  \item{alpha}{Signifance level (0.05 by default)}
  \item{leg}{Show legend \code{leg=TRUE} or not \code{leg=FALSE}}
}
\value{NULL}
\references{
Graffelman, J. (2021) Maximum likelihood estimation of the geometric niche preemption model
}
\author{ Jan Graffelman (jan.graffelman@upc.edu) }
\examples{
data(Fattorini)
raplot(Fattorini[,2])
}
\keyword{aplot}

