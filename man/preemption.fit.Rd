\name{preemption.fit}
\alias{preemption.fit}
\title{
Estimation of the preemption parameter of a geometric series by various methods
}
\description{
Function \code{preemption.fit} can estimate the preemption parameter of a geometric series by four specifici methods, or list all estimates simultaneously for comparison.
}
\usage{
preemption.fit(x, method = "ml", closed = FALSE, verbose = TRUE)
}
\arguments{
  \item{x}{Vector of counts (abundances of species)}
  \item{method}{Estimation method ("ml" = maximum likelihood, "ls" = least squares, "May" = May's equation, "HT" = He-Tang's equation, "all" = lists all four estimators)}
  \item{closed}{If \code{closed=FALSE} the exact value of the ML estimator will be calculated by iteration, if \code{closed=TRUE} a good closed-form approximation is given}
  \item{verbose}{The function is silent if \code{verbose=FALSE} or prints results if \code{verbose=TRUE}}
}
\value{
  \item{khat}{ the estimate of the preemption parameter }
  \item{ll}{ lower limit of 95 confidence interval }
  \item{ul}{ upper limit of 95 confidence interval }
}
\references{
Graffelman, J. (2021) Maximum likelihood estimation of the geometric niche preemption model
}
\author{ Jan Graffelman (jan.graffelman@upc.edu) }
\seealso{
\code{\link{preemption.t}}
}
\examples{
data(Ganeshaiah)
preemption.fit(Ganeshaiah[,2])
}
\keyword{misc}
