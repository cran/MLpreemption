\name{k_ml}
\alias{k_ml}
\title{
Preemption parameter estimation by maximum likelihood.
}
\description{
Calculates the maximum likelihood estimator for the geometric series.
}
\usage{
k_ml(xs, closed = FALSE, ll = 0.001, ul = 0.999)
}
\arguments{
  \item{xs}{A vector of ordered counts (form large to small)}
  \item{closed}{If \code{closed=TRUE} a closed form approximation to the ML estimator will be calculated; otherwise the exact ML estimator is calculated iteratively.}
  \item{ll}{Lower limit for the root searching algorithm}
  \item{ul}{Upper limit for the root searching algorithm}
}
\value{a real value}
\references{
Graffelman, J. (2021) Maximum likelihood estimation of the geometric niche preemption model. 
}
\author{ Jan Graffelman (jan.graffelman@upc.edu) }
\note{
counts should be ordered from large to small.
}
\seealso{
\code{\link{k_ls}},\code{\link{k_hetang}},\code{\link{k_may}}
}
\examples{
data("Ganeshaiah")
x <- Ganeshaiah[,2]
k_ml(x)
}
\keyword{misc}
