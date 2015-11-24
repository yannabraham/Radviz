\name{do.radviz}
\alias{do.radviz}
\alias{do.radviz.default}
\title{
Projects a Matrix or a Data Frame to a 2D space defined by Dimensional Anchors
}
\description{
do.radviz will return a projection of a multidimensional dataset onto a 2D space defined by dimensional anchors that have been projected on the unit circle using make.S
}
\usage{
\method{do.radviz}{default}(x,springs, \dots)
}
\arguments{
  \item{x}{
a placeholder for defining the S3 method; currently, expects a data.frame or matrix to be projected, with column names matching row names in springs
}
  \item{springs}{
A matrix of 2D dimensional anchor coordinates, as returned by make.S
}
  \item{\dots}{
Not implemented
}
}
\details{
The function expects that at least some of the column names in df will be matched by row names in springs.
}
\value{
An object of class radviz with the following slots:
data: the original data (x)
springs: the original springs
projected: the projection of data on springs, a matrix of 2D coordinates for every line in df
}\author{
Yann Abraham
}
\seealso{
\code{\link{do.radviz.default}}
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
summary(rv)
}

\keyword{ visualization }
\keyword{ multidimensional }