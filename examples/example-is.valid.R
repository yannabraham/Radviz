data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
iris0 <- rbind(iris,c(rep(0,length(das)),NA))
S <- make.S(das)
rv0 <- do.radviz(iris0,S)

sum(!is.valid(rv0)) # should be 1

# to find which points where invalid in the data
which(!is.valid(rv0))

# to review the original data points
rv1 <- subset(rv0,is.valid(rv0))

summary(rv1)