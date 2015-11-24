\name{hexplot}
\alias{hexplot}
\title{
A smoothScatter function for Radviz objects
}
\description{
Plots the Dimensional Anchors and a smoothed color density representation of projected data points in a 2D space.
}
\usage{
hexplot(x, main = NULL, label.color = "orangered4", label.size=1, mincnt=0, color=NULL, style='constant.col')
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
  \item{mincnt}{
numeric; cells with counts smaller than mincnt are not shown (see \link[hexbin]{grid.hexagons} for details)
}
  \item{color}{
if color is not \code{NULL} and corresponds to one of the channels in the \code{hexcols} slot of the Radviz object, cells will be colored using colors in the \code{hexcols} slot 
}
  \item{style}{
character string specifying the type of plotting (see \link[hexbin]{grid.hexagons} for details)
}
}
\details{
Plots the result of a \code{\link{do.hex}} function 
}
\author{
Dan Carr
Nicholas Lewin-Koh
Yann Abraham
}
\seealso{
\code{\link[hexbin]{grid.hexagons}} and \code{\link{hexbin}} for original implementation
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
rv <- do.hex(rv,channels='Sepal.Length',ncols=4,use.quantile=TRUE)
hexplot(rv,color='Sepal.Length')
}

\keyword{ visualization }
\keyword{ multidimensional }