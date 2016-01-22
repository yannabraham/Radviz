#' Optimize the Dimensional Anchors Position using a Genetic Algorithm
#' 
#' Allows to compute the best arrangement of Dimensional Anchors so that
#' visualization efficiency is maximized.
#' 
#' @param springs A matrix of 2D dimensional anchor coordinates, as returned by \link{make.S}
#' @param similarity A similarity matrix measuring the correlation between Dimensional Anchors
#' @param iter The maximum number of iterations (defaults to 100)
#' @param n The number of permutations of Dimensional Anchors to be created at each generation
#' @param top The number of permuations to keep to create the next generation
#' @param lambda The threshold for the optimization process
#' @param nlast The number of generations to wait before lambda is applied
#' @param optim The optimization function (in or rv)
#' 
#' @details The first generation is a random sampling of all Dimensional Anchors.
#'            For every generation afterwards, only the best solutions (as specified by top)
#'            are kept; the solutions are normalized around the unit circle (ie c(1,2,3,4)
#'            is equivalent to c(4,1,2,3) for Radviz projection) before the next generation
#'            is created. The next generation consists of
#'            \itemize{
#'              \item all unique best solutions from the previous generation
#'                      (after circular normalization)
#'              \item a permutation of all previous solutions.
#'            }
#'            Briefly, for every Dimensional Anchor position the previous generation is sampled
#'            to give a mixture of identical and slightly shifted (mutated) solutions.
#'            The algorithm will stop when the maximum number of iterations (as defined by \code{iter})
#'            is reached, or when a number of generations (defined by \code{nlast}) as not improved over
#'            the best solution by more than a given threshold (specified by \code{lambda}).
#'            
#' @return a list containing 3 sets of values:
#'          \itemize{
#'            \item \code{perfs} the list of the best performances by generation
#'            \item \code{best} the best performing arrangement by generation
#'            \item \code{last} the top performing arrangements of the last generation
#'          }
#' 
#' @examples
#' data(iris)
#' das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
#' S <- make.S(das)
#' scaled <- apply(iris[,das],2,do.L)
#' rv <- do.radviz(scaled,S)
#' plot(rv,main='Iris Columns',
#'      point.shape=1,
#'      point.color=c('red','green','blue')[as.integer(iris$Species)])
#' sim.mat <- cosine(scaled)
#' in.da(S,sim.mat) # the starting value
#' new <- do.optim(S,sim.mat,iter=10,n=100)
#' new.S <- make.S(get.optim(new))
#' new.rv <- do.radviz(scaled,new.S)
#' plot(new.rv,main='Optimized columns',
#'      point.shape=1,
#'      point.color=c('red','green','blue')[as.integer(iris$Species)])
#' 
#' @author Yann Abraham
#' @export
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