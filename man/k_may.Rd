\name{k_may}
\alias{k_may}
\title{
Preemption parameter estimation by the equation of May
}
\description{
Calculates the estimator of May for the geometric series.
}
\usage{
k_may(xs, exclude = TRUE)
}
\arguments{
  \item{xs}{A vector of ordered counts}
  \item{exclude}{Automatically exclude zeros (if \code{exclude=TRUE})}
}
\details{
  Solves May equation by using \code{uniroot}.
}
\value{a real value}
\references{
May, R. 1975. Patterns of species abundance and diversity. In Cody, M. and Diamond, M., editors, Ecology and Evolution of Communities, pages 81--120. Harvard Univ. Press.
}
\author{ Jan Graffelman (jan.graffelman@upc.edu) }
\note{
counts should be ordered from large to small.
}
\seealso{
\code{\link{k_ls}},\code{\link{k_hetang}},\code{\link{k_ml}}
}
\examples{
data("Ganeshaiah")
x <- Ganeshaiah[,2]
k_may(x)
}
\keyword{misc}
