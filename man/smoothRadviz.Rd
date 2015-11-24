\name{smoothRadviz}
\alias{smoothRadviz}
\title{
A smoothScatter function for Radviz objects
}
\description{
Plots the Dimensional Anchors and a smoothed color density representation of projected data points in a 2D space.
}
\usage{
smoothRadviz(x, main = NULL, label.color = "orangered4", label.size=1, smooth.color=colorRampPalette(c("white", blues9)), transformation = function(x) x^0.25, nbin=128, nrpoints = 100, bandwidth)
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
  \item{smooth.color}{
function accepting an integer n as an argument and returning n colors (see \link{smoothScatter} for details)
}
  \item{transformation}{
function mapping the density scale to the color scale
}
  \item{nbin}{
numeric vector of length one (for both directions) or two (for x and y separately) specifying the number of equally spaced grid points for the density estimation; directly used as gridsize in \link{bkde2D} (see \link{smoothScatter} for details)
}
  \item{nrpoints}{
number of points to be superimposed on the density image (see \link{smoothScatter} for details)
}
  \item{bandwidth}{
numeric vector (length 1 or 2) of smoothing bandwidth(s). If missing, a more or less useful default is used. bandwidth is subsequently passed to function \link{bkde2D} (see \link{smoothScatter} for details)
}
}
\details{
The add allows plotting of additional data such as cluster centers onto an existing plot.
}
\author{
Florian Hahne at FHCRC, originally
Yann Abraham
}
\seealso{
\code{\link{smoothScatter}} for original implementation
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
smoothRadviz(rv)
}

\keyword{ visualization }
\keyword{ multidimensional }