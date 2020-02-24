## compute distance matrix
d.iris <- dist(iris[,das])

## define a kNN matrix
n.iris <- as.matrix(d.iris)
n.iris <- apply(n.iris,1,function(x,k=12) {
  x[order(x)>(k+1)] <- 0
  return(x)
})
diag(n.iris) <- 0

## compute weights for kNN matrix
w.iris <- n.iris
w.iris <- exp(-w.iris^2/(2*median(w.iris[w.iris!=0])^2))
w.iris[n.iris==0] <- 0

## create graph
g.iris <- graph.adjacency(w.iris,mode='undirected',weight=TRUE,diag=FALSE)

V(g.iris)$Species <- as.character(iris[,'Species'])
V(g.iris)$color <- as.numeric(iris[,'Species'])

plot(g.iris,
     vertex.label=NA)

## project using Radviz
new.S <- do.optimGraphviz(iris[,das],
                          g.iris)

grv <- do.radviz(iris[,das],
                new.S,
                graph=g.iris)

plot(grv)+
  geom_point(aes(color=iris[,'Species']))
