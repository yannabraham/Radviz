\name{cosine}
\alias{cosine}
\title{
Compute the Cosine Similarity between the Columns of a Data Set
}
\description{
Given a dataset, compute the cosine similarity between to columns for use in optimization of Dimensional Anchors
}
\usage{
cosine(mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
A matrix or data.frame 
}
}

\value{
A symmetrical matrix with as many rows as there are columns in input
}
\author{
Yann Abraham
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
mat <- iris[,das]
scaled <- apply(mat,2,do.L)
sim.mat <- cosine(scaled)
ncol(mat)
dim(sim.mat)
}
\keyword{ similarity }
\keyword{ visualization }
