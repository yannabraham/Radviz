summary.radviz <-
function(object,n=8,...) {
	cat('A Radviz object with',nrow(object$data),'objects and',ncol(object$data),'dimensions\n')
	das <- rownames(object$springs)
	if(length(das)>n) {
		das <- das[seq(1,n)]
		das <- c(das,'...')
	}
	cat('Dimensional Anchors are',das,'\n')
}
