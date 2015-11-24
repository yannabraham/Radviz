\name{do.L}
\alias{do.L}

\title{
Perform L-Normalization on a Vector
}
\description{
Standardizes all values in a vector to the unit vector ([0,1]) using local min and max  
}
\usage{
do.L(v,na.rm)
}

\arguments{
  \item{v}{
a vector of values
}
  \item{na.rm}{
Logical: should NA be removed? defaults to TRUE
}
}
\details{
This is an alternative to performing a L normalization over the full matrix. 
}

\value{
A vector of values of the same lenght as x, scaled to the unit vector.
}
\author{
Yann Abraham
}
\examples{
data(iris)
mat <- iris[,c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
scaled <- apply(mat,2,do.L)
summary(scaled) # all values are between [0,1]
}
\keyword{ normalization }
