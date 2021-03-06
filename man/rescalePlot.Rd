% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rescalePlot.R
\name{rescalePlot}
\alias{rescalePlot}
\title{Rescaling of projected data for plotting}
\usage{
rescalePlot(x, fraction = 0.9)
}
\arguments{
\item{x}{a radviz object as produced by \code{\link{do.radviz}}}

\item{fraction}{numeric value, indicating which fraction of the unit circle should be used for the rescaled plot}
}
\value{
a radviz object as produced by \code{\link{do.radviz}}
}
\description{
Rescaling of projected data for plotting
}
\details{
A different rescaling is used here for plotting the projected data as compared to \code{\link{do.radviz}}. 
Only feature-wise rescaling is applied to the original data (through \code{\link{do.L}}), in accordance with the rescaling used in 
\code{\link{do.optimFreeviz}} and \code{\link{do.optimGraphviz}}. The projected data is then rescaled based on amplitude, 
to cover a pre-specified fraction of the unit circle.
For \code{Freeviz} and \code{Graphviz} objects, the rescaling will issue a warning if some points extend beyond the some anchors: 
in that case only the direction of the anchor can be interpreted but not the magnitude represented by the anchor's position.
}
\examples{
data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
rv <- do.radviz(iris,S)
library(ggplot2)
plot(rv)+geom_point(aes(color=Species))
new.rv <- rescalePlot(rv)
plot(new.rv)+geom_point(aes(color=Species))

}
\author{
Nicolas Sauwen
}
