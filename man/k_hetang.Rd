\name{k_hetang}
\alias{k_hetang}
\title{
Preemption parameter estimation by He and Tang
}
\description{
Calculates the He-Tang estimator for the geometric series.
}
\usage{
k_hetang(x)
}
\arguments{
  \item{x}{A vector of counts}
}
\value{
a real number
}
\references{He, F. and Tang, D. 2008. Estimating the niche preemption parameter of the geometric series. Acta Oecologica, 33:105--107}
\author{ Jan Graffelman (jan.graffelman@upc.edu) }
\note{
  Zero counts are discarded prior to calculation of the estimator.
}
\seealso{
\code{\link{k_ls}},\code{\link{k_ml}},\code{\link{k_may}}

}
\examples{
data("Ganeshaiah")
x <- Ganeshaiah[,2]
k_hetang(x)
}
\keyword{misc}
