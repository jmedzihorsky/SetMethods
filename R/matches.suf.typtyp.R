matches.suf.typtyp <-
function(results,
		 outcome,
		 solution=1,
		 term=NULL,
		 max_pairs=5)
	{
		X <- pimdata(results=results, outcome=outcome, solution=solution)
		y <- results$tt$initial.data[, outcome]
		if (!is.null(term)) {
			xa <- X[, term]
			ya <- X[, 'out']
			typical <- (xa>0.5) & (ya>0.5) & (xa<=ya) 
			x <- X[typical, term]
			y <- X[typical, 'out']
			r <- rownames(X)[typical]
			if (length(r)>1) {
				K <- t(combn(r, 2))
				aux.f <-
					function(p)
					{
						i <- which(r==p[1])
						j <- which(r==p[2])
						s <- (1-(abs(x[i]-x[j])+abs(y[i]-y[j])))/(x[i]+x[j])
						return(s)
					}
				s <- apply(K, 1, aux.f)
				R <- data.frame(typical1=K[,1],
								typical2=K[,2],
								distance=s,
								term=rep(term, length(s)),
								best_matching_pair=rep(FALSE, length(s)))	
				R <- R[order(s), ]
				R[R$distance==min(R$distance), 'best_matching_pair'] <- TRUE
				rownames(R) <- NULL
				return(R[1:(min(c(nrow(R), max_pairs))), ])
			} else {
				R <- data.frame(typical1=NULL,
								typical2=NULL,
								distance=NULL,
								term=NULL,
								best_matching_pair=NULL)	
				return(R)
			}
		} else {
			nt <- ncol(X)-2
			tn <- colnames(X)[1:nt]
			L <- list()
			for (i in 1:nt)
			{
				L[[i]] <- matches.suf.typtyp(results=results, outcome=outcome,
											 term=tn[i], max_pairs=max_pairs)
			}
			return(L)
		}	             
	}
