\name{plot.radviz}
\alias{plot.radviz}
\title{
A Plotting Functions for the Radviz Object
}
\description{
Plots the Dimensional Anchors and projected data points in a 2D space.
}
\usage{
\method{plot}{radviz}(x, main = NULL, label.color = "orangered4", label.size=1, point.color = "black", point.shape = ".", point.size = 1, add = F, \dots)
}
\arguments{
  \item{x}{
a radviz object as produced by do.radviz
}
  \item{main}{
[Optional] a title to the graph, displayed on top 
}
  \item{label.color}{
The color of the Dimensional Anchors (defaults to orangered4)
}
  \item{label.size}{
numeric character expansion factor for Dimensional Anchor labels;\
multiplied by \code{par("cex")} yields the final character size. NULL and NA are equivalent to 1.0
}
  \item{point.color}{
The point color (defaults to black)
}
  \item{point.shape}{
The point shape (defaults to '.')
}
  \item{point.size}{
the point size (defaults to 1)
}
  \item{add}{
Logical: if add is \code{TRUE} then only the projected points are plotted
}
  \item{\dots}{
not implemented
}
}
\details{
The add allows plotting of additional data such as cluster centers onto an existing plot.
}
\author{
Yann Abraham
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
plot(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
}

\keyword{ visualization }
\keyword{ multidimensional }
