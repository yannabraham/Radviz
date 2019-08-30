## ------------------------------------------------------------------------
library(Radviz)

## ------------------------------------------------------------------------
library(bodenmiller)
data("refPhenoMat")
data("untreatedPhenoMat")
data("refFuncMat")
data("untreatedFuncMat")
data("refAnnots")
data("untreatedAnnots")
fullMat <- rbind(cbind(refPhenoMat,
                       refFuncMat),
                 cbind(untreatedPhenoMat,
                       untreatedFuncMat))

refAnnots$Treatment <- 'untreated'
fullAnnots <- rbind(refAnnots[,colnames(untreatedAnnots)],
                    untreatedAnnots)
fullAnnots$Treatment <- factor(fullAnnots$Treatment)
fullAnnots$Treatment <- relevel(fullAnnots$Treatment,'untreated')

## ------------------------------------------------------------------------
# define a subset of populations & stimulations that have at least 1000 cells
with(fullAnnots,table(Cells,Treatment))

subMat <- fullMat[with(fullAnnots,Cells %in% c('cd4+','cd8+')),]
subAnnots <- subset(fullAnnots,Cells %in% c('cd4+','cd8+'))

# subMat <- fullMat[with(fullAnnots,Cells %in% c('igm+','igm-')),]
# subAnnots <- subset(fullAnnots,Cells %in% c('igm+','igm-'))

subAnnots <- droplevels(subAnnots)

## ------------------------------------------------------------------------
norm <- apply(subMat,2,do.L,fun=function(x) quantile(x,c(0.025,0.975)))
sim.mat <- cosine(subMat)
cur.S <- make.S(dimnames(refFuncMat)[[2]])
ref.optim <- do.optim(cur.S,sim.mat)
cur.S <- make.S(get.optim(ref.optim))

weights <- sweep(norm[,rownames(cur.S)],1,rowSums(norm),`/`)

## ------------------------------------------------------------------------
Group <- reshape2::acast(data.frame(id=seq(nrow(subAnnots)),
                                    Treatment=subAnnots$Treatment,
                                    value=1),
                         formula=id~Treatment,
                         value.var = 'value',
                         fill=0)
Group <- Group %*% t(Group)

# cur.cols <- colorRampPalette(RColorBrewer::brewer.pal(9,'Set1'))(nlevels(subAnnots$Treatment))
cur.cols <- RColorBrewer::brewer.pal(nlevels(subAnnots$Treatment),'Set1')

## ------------------------------------------------------------------------
# define minibatch size
mb <- 1024

# define a balanced epoch
is <- lapply(seq(250),function(j) sample(seq(ceiling(nrow(norm)/mb)),nrow(norm),replace = TRUE))
ist <- lapply(seq(250),function(j) {
  test <- lapply(unique(is[[j]]),function(k) table(subAnnots$Treatment[is[[j]]==k])/sum(is[[j]]==k))
  test <- do.call(rbind,test)
  return(test)
})

istm <- lapply(ist,function(test) sum(sweep(test,2,table(subAnnots$Treatment)/nrow(subAnnots),`-`)^2)^0.5)
istv <- lapply(ist,function(test) mean(apply(test,2,sd)))

plot(unlist(istm),unlist(istv))

# which.min(unlist(istm)*unlist(istv))
# i <- is[which.min(unlist(istm)*unlist(istv))]
# base <- weights[i,]
# group <- Group[i,i]

eps <- rank(unlist(istm)*unlist(istv))

## ------------------------------------------------------------------------
attract <- 0.01
repel <- 0.5
learning_rate <- 0.9
stepFactor <- 1
epsilon <- 1e-8

## ------------------------------------------------------------------------
cur.S <- make.S(get.optim(ref.optim))

tol <- 1e-6
cost <- NULL

epochs <- 0

## ------------------------------------------------------------------------
for(e in seq(6)) {
  cat('Epoch #',epochs,'\n')
  # i <- sample(nrow(norm))
  i <- is[[which(eps==(1+epochs))]]
  base <- weights[i,]
  # group <- Group[i,i]
  l <- 0
  
  ## ------------------------------------------------------------------------
  
  for(l in seq(length(i)%/%mb)) {
    cat('\t',l,'\n')
    
    ## project points from minibatch
    vs <- i==l
    pos <- base[vs,] %*% cur.S
    grp <- Group[vs,vs]
    
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
points(weights %*% cur.S,
       pch='.',
       col=cur.cols[as.numeric(subAnnots$Treatment)])
text(cur.S,rownames(cur.S))
legend('bottomleft',
       legend=levels(subAnnots$Treatment),
       pch=16,
       col=cur.cols,
       bty='n')

plot(weights %*% cur.S,
     pch=16,
     cex=0.75,
       col=cur.cols[as.numeric(subAnnots$Treatment)])
legend('bottomleft',
       legend=levels(subAnnots$Treatment),
       pch=16,
       col=cur.cols,
       bty='n')
