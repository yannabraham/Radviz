\name{make.S}
\alias{make.S}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Define Dimensional Anchors around the Unit Circle
}
\description{
make.S will return xy coordinates for n dimensional anchors equally spaced around the unit circle
}
\usage{
make.S(x)
}

\arguments{
  \item{x}{ x can be a vector of dimensional anchors or a simple number
     }
}
\details{
If x is a vector, values will be used to set the row names of the matrix.
}
\value{
A matrix with 2 columns (x and y coordinates of dimensional anchors) and 1 line per dimensional anchor (so called springs).
If x is a vector, the row names of matrix will be set to the syntactically correct version of values in the vector (through a call to make.names).
Please note that some function expect to match column names of data to row names of the spring matrix.
}\author{
Yann Abraham
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
make.S(length(das)) # without row names
make.S(das) # with row names
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ visualization }
\keyword{ multidimensional }
