\name{summary.radviz}
\alias{summary.radviz}
\alias{head.radviz}
\alias{print.radviz}
\alias{subset.radviz}
\title{
Radviz Object Summary, Head, Print, Subset Methods
}
\description{
Provides helper function to deal with Radviz objects
}
\usage{
\method{summary}{radviz}(object,n=8,\dots)
\method{head}{radviz}(x,n=6,\dots)
\method{print}{radviz}(x,n=6,\dots)
\method{subset}{radviz}(x,i,\dots)
}
\arguments{
  \item{x, object}{
An object of class Radviz, as returned by do.radviz
}
  \item{n}{
The number of lines from each slots in the Radviz object to display (defaults to 6)
}
  \item{i}{
A logical or indices vector of the same length as the original data used to create the Radviz object, that is used to subset each slots  
}
  \item{\dots}{
not implemented
}
}
\value{
For subset.radviz, a new Radviz object containing only rows specified in i. Any density or hexbin analysis is dropped.
}
\author{
Yann Abraham
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
summary(rv)
head(rv,5)
print(rv)
# create a subset
srv <- subset(rv,iris$Species=='setosa')
summary(srv)
sum(iris$Species=='setosa') # 50 objects in srv corresponding to setosa values 
}

\keyword{ visualization }
\keyword{ multidimensional }