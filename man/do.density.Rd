\name{do.density}
\alias{do.density}
\title{
Computes 2D density for contour plots of Radviz objects
}
\description{
Computes 2D density estimate of projected data for a Radivz object, using the \code{\link[MASS]{kde2d}} function from the \pkg{MASS} package 
}
\usage{
do.density(x, n=50)
}
\arguments{
  \item{x}{
a radviz object as produced by do.radviz
}
  \item{n}{
Number of grid points in each direction. Can be scalar or a length-2 integer vector (see \link{kde2d} for details)
}
}
\details{
Computes a 2D density estimate of Radviz projected data and stores the results in a \code{density} slot of the Radviz object
}
\value{
the Radviz object with and extra slot \code{density} containing the 2D density estimates for use with \code{\link{contour.radviz}}
}
\seealso{
\link{contour.radviz} for plotting
}
\author{
Yann Abraham
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
rv <- do.density(rv)
contour(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
}

\keyword{ visualization }
\keyword{ multidimensional }
