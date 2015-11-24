\name{do.hex}
\alias{do.hex}
\title{
Computes 2D hexagonal bins from a Radviz projection
}
\description{
Computes 2D density using hexagon binning of projected data for a Radivz object, using the \code{\link{hexbin}} function from the \link{hexbin} package 
}
\usage{
do.hex(x, n=30, channels=NULL,colramp=function(n) colorspace::diverge_hcl(n),ncols=8,use.quantile=F,fixed=NULL)
}
\arguments{
  \item{x}{
a radviz object as produced by do.radviz
}
  \item{n}{
Number of bins in the grid (see \link[hexbin]{hexbin} for details)
}
  \item{channels}{
[optional] if channels is not \code{NULL}, the function will compute color scales for the specified channels
}
  \item{colramp}{
[optional] function accepting an integer n as an argument and returning n colors
}
  \item{ncols}{
[optional] the number of levels used to cut each channels
}
  \item{use.quantile}{
[optional] if channels is not \code{NULL} and use.quantile is \code{TRUE}, channels will be cut into \code{ncols+1} quantiles\
otherwise a fixed sequence from 0 to 1 in \code{ncols+1} steps will be used  
}
  \item{fixed}{
[optional] if channels is not \code{NULL} and fixed is \code{TRUE}, channels will be cut into \code{length(fixed)+1} steps using the values in \code{fixed} as breaking points  
}
}
\details{
The projected points will be binned into an hexagonal grid of size \code{n}; if channels is not \code{NULL},\
for every channels the median intensity will be estimated over the complete grid and a color will be assigned from \code{colramp} using \code{ncols},\
\code{fixed} and \code{use.quantile}; by default, channels will be split using a fixed sequence from 0 to 1 over \code{ncols+1} levels unless \code{use.quantile} or \code{fixed} are set
}
\value{
the Radviz object with an extra slot \code{hex} containing the hexbin object; if \code{channels} is not \code{NULL} an extra \code{hexcols} will be present containing the color information for every channel  
}
\seealso{
\link{hexplot} for plotting
\link{hexbin} for original implementation
}
\author{
Dan Carr
Yann Abraham
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
rv <- do.hex(rv,channels='Sepal.Length',ncols=4,use.quantile=TRUE)
summary(rv$hex)
}

\keyword{ visualization }
\keyword{ multidimensional }