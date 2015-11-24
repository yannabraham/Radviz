head.radviz <-
function(x,n=6,...) {
	cat('Data\n')
	print(head(x$data,n=n))
	cat('Springs\n')
	print(head(x$springs,n=n))
	cat('Projection\n')
	print(head(x$projected,n=n))
}
