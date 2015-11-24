\name{Radviz-package}
\alias{Radviz-package}
\alias{Radviz}
\docType{package}
\title{
Radviz Projection of Multidimensional Data
}
\description{
Radviz uses Dimensional Anchors and the spring paradigm to project a multidimensional space in 2D.
This allows for the quick visualization of large and complex datasets. }
\details{
\tabular{ll}{
Package: \tab Radviz\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2012-11-08\cr
License: \tab CC BY-NC-SA\cr
}

}
\author{
Yann Abraham
Maintainer: Yann Abraham <yann.abraham@novartis.com>
}
\references{
see Hoffman et al. 1999 for original implementation
see di caro et al. 2010 for original idea for dimensional anchor arrangements
}
\keyword{ package }
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
plot(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
}
