do.free <- function(x,springs,classes,lim=1e-5,nitems=NULL,niter=100) {
  # based on http://www.sciencedirect.com/science/article/pii/S1532046407000275
  # and http://www.imia-medinfo.org/wg_idamap/idamap/idamap2005/papers/12%20Demsar%20CR.pdf
  radviz <- list()
  radviz$data <- x
  mat <- as.matrix(x[,rownames(springs)])
  weights <- mat/matrix(rep(rowSums(mat),each=ncol(mat)),nrow=nrow(mat),byrow=T)
  
  # initial projection
  p1 <- weights %*% springs
  
  if(!is.factor(classes)) {
    classes <- factor(classes)
  }
  
  if(is.null(nitems)) {
    nitems <- min(table(classes))
  } else {
    nitems <- floor(nitems/nlevels(classes))
  }
  
  i <- 0
  
  while( i<=niter ) {
    samples <- lapply(levels(classes),function(x) sample(which(classes==x),nitems,replace=TRUE) )
    samples <- unlist(samples)

    forces <- matrix(0,
                     nrow=length(samples),
                     ncol=ncol(p1))
    for(e in seq(length(samples)-1)) {
      for(f in seq(e+1,length(samples))) {
        d <- p1[samples[e],]-p1[samples[f],]
        r <- sum(d^2)^0.5
        if(classes[samples[e]]==classes[samples[f]]) {
          frc <- -r^2
        } else {
          frc <- 1/r^2
        }
        forces[e,] <- forces[e,]+d*frc/r
        forces[f,] <- forces[f,]-d*frc/r
      }
    }
    
    vald <- !apply(forces,1,function(x) any(is.na(x)))
    
    s2 <- t(forces[vald,]) %*% mat[samples[vald],]
    s2 <- t(s2)
    d <- apply(s2,1,function(x) sum(x^2)^0.5)
    a <- atan2(s2[,2],s2[,1])
    s2 <- cbind(d*cos(a)/max(d),
                d*sin(a)/max(d))
    
    p2 <- weights%*%s2
    
    if(!exists('perfs')) {
      perfs <- sum((p2-p1)^2)^0.5
    }
    
    if(sum((p2-p1)^2)^0.5<lim) {
      perfs <- c(perfs,sum((p2-p1)^2)^0.5)
      break()
    } else if(rev(perfs)[1]>sum((p2-p1)^2)^0.5) {
      perfs <- c(perfs,sum((p2-p1)^2)^0.5)
      p1 <- p2
    }
    
    cat(i,min(perfs),'\n',sep='\t')
    i <- i+1
  }
  
  vald <- apply(p2,1,function(x) any(is.na(x)))
  if(any(vald)) {
    warning('at least 1 point could not be projected; check the `valid` slot for details')
  }
  row.names(proj) <- row.names(mat)
  radviz$springs <- s2
  radviz$projected <- p2
  radviz$valid <- unname(!vald)
  radviz$performance <- perfs
  class(radviz) <- 'radviz'
  return(radviz)
}

library(bodenmiller)
library(Radviz)
data(refPhenoMat)
data(refAnnots)

norm <- apply(refPhenoMat,2,do.L,fun=function(x) quantile(x,c(0.025,0.975)))

ct.S <- make.S(dimnames(refPhenoMat)[[2]])

test2 <- do.free(refPhenoMat,ct.S,classes=refAnnots$Cells,nitems=250,niter=25)

plot(test2,
     point.color=as.numeric(refAnnots$Cells))
