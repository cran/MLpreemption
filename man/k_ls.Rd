\name{k_ls}
\alias{k_ls}
\title{
Preemption parameter estimation by least squares regression
}
\description{
Calculates the least-squares estimator for the geometric series.
}
\usage{
k_ls(x)
}
\arguments{
  \item{x}{A vector of ordered counts (from large to small)}
}
\value{
  \item{khat}{estimate of the preemption parameter}
  \item{k.ll}{lower limit of the confidence interval}
  \item{k.ul}{upper limit of the confidence interval}
  \item{aic}{Akaike's information criterion}
  \item{logl}{log-likelihoood}
}
\references{Magurran, A. 2004. Measuring biological diversity. Blackwell Publishing, Oxford, UK.}
\author{ Jan Graffelman (jan.graffelman@upc.edu) }
\note{
counts should be ordered from large to small.
}
\seealso{
\code{\link{k_ml}},\code{\link{k_hetang}},\code{\link{k_may}}
}
\examples{
data("Ganeshaiah")
x <- Ganeshaiah[,2]
k_ls(x)
}
\keyword{misc}
