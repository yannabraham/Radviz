\name{in.da}
\alias{in.da}
\alias{rv.da}
\title{
Optimization functions for Dimensional Anchors in Radviz
}
\description{
Visual efficiency of Radviz plots depends heavily on the correct arrangement of Dimensional Anchors. These functions implement the optimization strategies described in de cario et al. 2012
}
\usage{
in.da(springs, similarity)
rv.da(springs, similarity)
}

\arguments{
  \item{springs}{
A matrix of 2D dimensional anchor coordinates, as returned by make.S
}
  \item{similarity}{
A similarity matrix measuring the correlation between Dimensional Anchors
}
}
\details{
Following the recommendation of de cario et al. we used a cosine function to calculate the similarity between Dimensional Anchors (see \code{\link{cosine}} for details).
The in.da function implements the independent similarity measure, where the value increases as the Radviz projection improves.
The rv.da function implements the radviz-dependent similarity measure, where the value decreases as the Radviz projection improves. 
}
\value{
A measure of the efficiency of the Radviz projection of the similarity matrix onto a set of springs
}
\author{
Yann Abraham
}
\seealso{
\code{\link{cosine}}
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
scaled <- apply(iris[,das],2,do.L)
sim.mat <- cosine(scaled)
in.da(S,sim.mat) # increases with better projections
rv.da(S,sim.mat) # decreases with better projections
}

\keyword{ visualization }
\keyword{ optimization }
\keyword{ multidimensional }
