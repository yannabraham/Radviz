do.optim <- function (springs, similarity, iter = 100, n = 1000, top = round(n * 
						0.1), lambda = 0.01, nlast = 5, optim = "in.da") 
{
	cat("Selected optimization function:",optim,'\n')
	if (!exists(optim,mode='function')) {
		stop("Optim must specify a valid function")
	}
	# test whether the optim function returns a valid result?
	# ie a single value of type integer
	test.optim <- do.call(optim, list(springs, similarity))
	if(class(test.optim)!=class(1) | length(test.optim)!=1) {
		stop(optim,"must return a single numeric value")
	} 
	circle <- function(spr) {
		i <- which.min(rank(spr))
		if (i > 1) {
			crc <- spr[c(seq(min(i, length(spr)), length(spr)), 
							seq(i - 1, 1))]
			return(crc)
		}
		else {
			return(spr)
		}
	}
	init <- function(springs, n) {
		return(lapply(seq(1, n), function(x) {
							circle(sample(rownames(springs), length(rownames(springs))))
						}
				)
		)
	}
	select <- function(best, springs, n) {
		chr <- unique(best)
		f <- do.call("rbind", best)
		new <- lapply(seq(1, n - length(chr)), function(x, springs, 
						f) {
					seq <- c()
					da <- rownames(springs)
					for (i in seq(1, nrow(springs) - 1)) {
						avail <- f[, i]
						avail <- avail[avail %in% da]
						if (length(avail) == 0) {
							avail <- da
						}
						seq <- c(seq, sample(avail, 1))
						da <- da[!da %in% seq]
					}
					seq <- circle(c(seq, da))
					return(seq)
				}, springs, f)
		return(c(chr, new))
	}
	chr <- init(springs, n)
	best <- list(NULL)
	perfs <- do.call(optim,list(springs, similarity))
	cat("Starting performance:", perfs, "\n")
	last <- 0
	i <- 0
	while (i <= iter) {
		cur.perfs <- unlist(
				lapply(chr, function(x) {
							row.names(springs) <- x
							do.call(optim, list(springs, similarity))
						}
				)
		)
		best <- c(best, chr[which.min(cur.perfs)])
		cat(i, "Current performance:", min(cur.perfs), "\n")
		chr <- chr[order(abs(cur.perfs), decreasing = T)]
		seeds <- chr[seq(1, top)]
		chr <- select(seeds, springs, n)
		perfs <- c(perfs, min(cur.perfs))
		last <- abs(perfs[seq(max(1, length(perfs) - nlast), 
								length(perfs))] - min(perfs))
		if (i > nlast & all(last < lambda)) {
			cat("Execution stopped after", i, "iterations: no better solution found in the last", 
					nlast, "iterations\n")
			break
		}
		i <- i + 1
	}
	return(list(perfs = perfs, best = best, last = seeds))
}