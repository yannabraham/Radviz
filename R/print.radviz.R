print.radviz <-
function(x,n=6,...) {
	cat('A radviz object generated using\n')
	print(head(x$data,n))
}
