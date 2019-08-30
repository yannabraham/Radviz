## ------------------------------------------------------------------------
library(Radviz)

## ------------------------------------------------------------------------
library(bodenmiller)
data(refPhenoMat)
data(refFuncMat)
data(refAnnots)
refMat <- cbind(refPhenoMat,refFuncMat)

## ------------------------------------------------------------------------
# norm <- apply(refPhenoMat,2,do.L,fun=function(x) quantile(x,c(0.025,0.975)))
norm <- apply(refPhenoMat,2,do.L)
weights <- sweep(norm,1,rowSums(norm),`/`)
sim.mat <- cosine(refPhenoMat)
cur.S <- make.S(dimnames(refPhenoMat)[[2]])
ref.optim <- do.optim(cur.S,sim.mat)
cur.S <- make.S(get.optim(ref.optim))

## ------------------------------------------------------------------------
# define a subset of populations that have at least 1000 cells
valid.pops <- names(which(table(refAnnots$Cells)>1000))
norm <- norm[refAnnots$Cells %in% valid.pops,]
refAnnots <- subset(refAnnots,Cells %in% valid.pops)
refAnnots <- droplevels(refAnnots)

## ------------------------------------------------------------------------
Group <- reshape2::acast(data.frame(id=seq(nrow(refAnnots)),
                                    Cells=refAnnots$Cells,
                                    value=1),
                         formula=id~Cells,
                         value.var = 'value',
                         fill=0)
Group <- Group %*% t(Group)

cur.cols <- colorRampPalette(RColorBrewer::brewer.pal(9,'Set1'))(nlevels(refAnnots$Cells))

## ------------------------------------------------------------------------
tol <- 1e-6
cost <- NULL

mb <- 1024
epochs <- 0

## ------------------------------------------------------------------------
# define a balanced epoch
is <- lapply(seq(1000),function(j) sample(seq(ceiling(nrow(norm)/mb)),nrow(norm),replace = TRUE))
ist <- lapply(seq(1000),function(j) {
  test <- lapply(unique(is[[j]]),function(k) table(refAnnots$Cells[is[[j]]==k])/sum(is[[j]]==k))
  test <- do.call(rbind,test)
  return(test)
})

istm <- lapply(ist,function(test) sum(sweep(test,2,table(refAnnots$Cells)/nrow(refAnnots),`-`)^2)^0.5)
istv <- lapply(ist,function(test) mean(apply(test,2,sd)))

plot(unlist(istm),unlist(istv))

# which.min(unlist(istm)*unlist(istv))
# i <- is[which.min(unlist(istm)*unlist(istv))]
# base <- weights[i,]
# group <- Group[i,i]

eps <- rank(unlist(istm)*unlist(istv))

## ------------------------------------------------------------------------
attract <- 0.001
repel <- 0.1
learning_rate <- 0.9
stepFactor <- 1
epsilon <- 1e-8

## ------------------------------------------------------------------------
for(e in seq(3)) {
  cat('Epoch #',epochs,'\n')
  # i <- sample(nrow(norm))
  i <- is[[which(eps==e)]]
  base <- weights[i,]
  group <- Group[i,i]
  L <- seq.int(0,length(i)-1) %/% mb
  l <- 0
  
  ## ------------------------------------------------------------------------
  
  for(l in seq(l,max(L))) {
    cat('\t',l,'\n')
    
    ## project points from minibatch
    vs <- L==l
    pos <- base[vs,] %*% cur.S
    grp <- group[vs,vs]
    
    ## compute distances
    R <- as.matrix(dist(pos))
    R[grp==1] <- -attract * R[grp==1]
    R[grp==0] <- repel/(R[grp==0]+epsilon)
    
    ## compute forces on each point based on all other points
    tn <- t(R) %*% pos
    
    ## compute gradient
    G_ <- t(base[vs,]) %*% tn
    
    ## adjust cur.S according to gradients
    st <- min((rowSums(cur.S^2)^0.5)/(rowSums(G_^2)^0.5))
    
    new.S <- cur.S-learning_rate*G_*st^stepFactor
    new.S <- new.S-mean(new.S) # center
    new.S <- new.S/max(rowSums(new.S^2)^0.5) # rescale
    
    # text(new.S,rownames(new.S))
    
    cost <- c(cost,max(rowSums((new.S-cur.S)^2)^0.5))
    cur.S <- new.S
  }
  
  epochs <- epochs+1
}

plot(seq(length(cost)),cost)
abline(h=tol,col=2,lty=2)
abline(v=seq(0,length(cost),by=ceiling(nrow(norm)/mb))[-1],col=3,lty=2)

plot(cur.S,type='n')
points(base %*% cur.S,
       pch='.',
       col=cur.cols[as.numeric(refAnnots$Cells)[i]])
text(cur.S,rownames(cur.S))
legend('bottomleft',
       legend=levels(refAnnots$Cells),
       pch=16,
       col=cur.cols,
       bty='n')

plot(base %*% cur.S,
       pch=16,
     cex=0.75,
       col=cur.cols[as.numeric(refAnnots$Cells)[i]])
