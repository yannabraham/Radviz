
#' Computation of weighted version of the Davies-Bouldin index. This index serves as a measure of clustering quality of a 2D projection result with known class labels
#' 
#' @param x an object of class Radviz, as returned by \code{\link{do.radviz}}
#' @param className the name of the class column to use
#' 
#' @details If \code{className} is left \code{NULL} (the default) the function expects a single extra column on top of 
#' the data columns (used to define springs) and the standard \code{Radviz} columns.
#' 
#' @return weighted DB index value
#' 
#' @importFrom clusterSim index.DB
#' 
#' @author Nicolas Sauwen
#' @export
DB_weightedIdx <- function(x,className=NULL){
  
  if(x$type == "Graphviz"){
    stop("Davies-Bouldin weighted index can not be computed on radviz object of type 'graphviz'")
  }
  
  data <- x$proj$data
  springs <- x$proj$plot_env$springs
  springNames <- rownames(springs)
  
  dataCols <- colnames(data)
  dataCols <- dataCols[!dataCols %in% c('rx','ry','rvalid')]
  dataCols <- dataCols[!dataCols %in% springNames]
  
  if(is.null(className)) {
    ## try to guess the class column
    if(length(dataCols)==1) {
      cat('using',dataCols,'as class column\n')
      classes <- unlist(data[dataCols])[unlist(!data$rvalid)]
    } else {
      stop('More that one possible class available - please specify class column using `className`\n')
    }
  } else {
    if(className %in% dataCols) {
      cat('using',className,'as class column\n')
      classes <- unlist(data[className])[unlist(!data$rvalid)]
    } else {
      stop(className,'is not a valid column name in the current object\n')
    }
  }
  projectedData <- data[!data[,'rvalid'],c('rx','ry')]
  projectedData <- as.matrix(projectedData)
  
  if(length(classes) != nrow(data)){
    stop("Class labels could not be extracted from the radviz object")
  }
  
  classes <- as.integer(as.factor(classes))
  nClasses <- length(unique(classes))
  clusterSizes <- table(classes)
  
  centers <- apply(projectedData,2,function(x) tapply(x,classes,mean))
  
  S <- lapply(seq(nClasses),function(i) {
    ind <- classes==i
    if(sum(ind)>1) {
      s <- sweep(projectedData[ind,],2,centers[i,],`-`)
      s <- rowSums(s^2)^(1/2)
      s <- mean(s^2)^(1/2)
    } else {
      s <- 0
    }
    return(s)
  })
  S <- unlist(S)
  
  M <- as.matrix(dist(centers, method = "minkowski", p = 2))
  
  R <- matrix(NA,ncol = nClasses, nrow = nClasses)
  for(i in seq(nClasses-1)) {
    for(j in seq(i+1,nClasses)) {
      R[i,j] <- R[j,i] <- (S[i]+S[j])/M[i,j]
    }
  }
  meanRVect <- rowMeans(R,na.rm=TRUE)
  DBIdx <- sum(clusterSizes*meanRVect)/sum(clusterSizes)
  
  return(DBIdx)
}
