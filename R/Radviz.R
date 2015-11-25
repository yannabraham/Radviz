#' Radviz Projection of Multidimensional Data
#'
#' Radviz uses Dimensional Anchors and the spring paradigm to project a multidimensional space in 2D.
#' This allows for the quick visualization of large and complex datasets.
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' rv <- do.radviz(iris,S)
#' plot(rv,point.shape=1,point.color=c('red','green','blue')[as.integer(iris$Species)])
#' 
#' @docType package
#' @name Radviz
NULL
#> NULL