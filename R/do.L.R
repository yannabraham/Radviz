do.L <-
function(v,na.rm=T) {
	if(na.rm) {
		v <- v[!is.na(v)]
	}
	min_v <- min(v)
	max_v <- max(v)
	return((v-min_v)/(max_v-min_v))
}
