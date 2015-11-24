\name{do.optim}
\alias{do.optim}

\title{
Optimize the Dimensional Anchors Position using a Genetic Algorithm
}
\description{
Allows to compute the best arrangement of Dimensional Anchors so that visualization efficiency is maximized. 
}
\usage{
do.optim(springs, similarity, iter = 100, n = 1000, top = round(n * 0.1), lambda = 0.01, nlast = 5, optim = "in.da")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{springs}{
A matrix of 2D dimensional anchor coordinates, as returned by make.S
}
  \item{similarity}{
A similarity matrix measuring the correlation between Dimensional Anchors
}
  \item{iter}{
The maximum number of iterations (defaults to 100)
}
  \item{n}{
The number of permutations of Dimensional Anchors to be created at each generation
}
  \item{top}{
The number of permuations to keep to create the next generation
}
  \item{lambda}{
The threshold for the optimization process 
}
  \item{nlast}{
The number of generations to wait before lambda is applied
}
  \item{optim}{
The optimization function (in or rv)
}
}
\details{
The first generation is a random sampling of all Dimensional Anchors.
For every generation afterwards, only the best solutions (as specified by top) are kept; the solutions are normalized around the unit circle (ie c(1,2,3,4) is equivalent to c(4,1,2,3) for Radviz projection) before the next generation is created.
The next generation consists of 1) all unique best solutions from the previous generation (after circular normalization) and 2) a permutation of all previous solutions.
Briefly, for every Dimensional Anchor position the previous generation is sampled to give a mixture of identical and slightly shifted (mutated) solutions.
The algorithm will stop when the maximum number of iterations (as defined by iter) is reached, or when a number of generations (defined by nlast) as not improved over the best solution by more than a given threshold (specified by lambda).  
}
\value{
a list containing 3 sets of values:
- perfs: the list of the best performances by generation
- best: the best performing arrangement by generation
- last: the top performing arrangements of the last generation 
}
\author{
Yann Abraham
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
scaled <- apply(iris[,das],2,do.L)
rv <- do.radviz(scaled,S)
plot(rv,main='Iris Columns',point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
sim.mat <- cosine(scaled)
in.da(S,sim.mat)
new <- do.optim(S,sim.mat,iter=10,n=100)
new.S <- make.S(tail(new$best,1)[[1]])
new.rv <- do.radviz(scaled,new.S)
x11()
plot(new.rv,main='Optimized columns',point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
}

\keyword{ multidimensional }
\keyword{ optimization }
