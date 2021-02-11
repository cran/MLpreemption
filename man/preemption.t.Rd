\name{preemption.t}
\alias{preemption.t}
\title{
Preemption t test
}
\description{
Function \code{preemption.t} implements a t test for comparing the preemption parameters of the geometric series for two samples. 
}
\usage{
preemption.t(x1, x2, verbose = TRUE)
}
\arguments{
  \item{x1}{Species counts for the first sample}
  \item{x2}{Species counts for the second sample}
  \item{verbose}{The function is silent if \code{verbose=FALSE} or prints results if \code{verbose=TRUE}}
}
\value{
  \item{Tstat}{The t statistic}
  \item{df}{The degrees of freedom}
  \item{pval}{The p-value of the test}
}
\references{
Graffelman, J. (2021) Maximum likelihood estimation of the geometric niche preemption model
}
\author{ Jan Graffelman (jan.graffelman@upc.edu) }
\seealso{
\code{\link{preemption.fit}}
}
\examples{
data(Mehrabi)
x <- sort(Mehrabi[,1],decreasing=TRUE)
y <- sort(Mehrabi[,2],decreasing=TRUE)
results <- preemption.t(x,y)
}
\keyword{htest}
