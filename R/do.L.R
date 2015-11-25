#' Perform L-Normalization on a Vector
#' 
#' Standardizes all values in a vector to the unit vector ([0,1]) using local min and max
#' 
#' @param v a vector of values
#' @param na.rm Logical: should NA be removed? defaults to \code{TRUE}
#' 
#' @details This is an alternative to performing a L normalization over the full matrix. 
#' 
#' @return A vector of values of the same lenght as x, scaled to the unit vector.
#' 
#' @examples
#' data(iris)
#' mat <- iris[,c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
#' scaled <- apply(mat,2,do.L)
#' summary(scaled) # all values are between [0,1]
#' 
#' @author Yann Abraham
#' @export
do.L <-
function(v,na.rm=T) {
	if(na.rm) {
		v <- v[!is.na(v)]
	}
	min_v <- min(v)
	max_v <- max(v)
	return((v-min_v)/(max_v-min_v))
}
