cases.suf.dcn <-
function(results,
		 outcome,
		 solution=1)
	{
		X <- pimdata(results=results, outcome=outcome, solution=solution)
		y <- results$tt$initial.data[, outcome]
		aux <-
			function(i)
			{
				fil <- (X[,i] > 0.5) & (y < 0.5) 
				Z <- data.frame(x=X[fil, i],
							   	y=y[fil],
							   	s=rep(FALSE, sum(fil)),
								term=rep(colnames(X)[i], sum(fil)),
								case=rownames(X)[fil])
				s <- 1 - (Z$x-Z$y)/Z$x
				Z$s[s==min(s)] <- TRUE
				Z$Sd <- s 
				colnames(Z)[1:3] <- c('term_membership', outcome, 'most_deviant')
				return(Z[, c(5, 4, 1, 2, 6, 3)])
			}
		R <- do.call(rbind, lapply(1:(ncol(X)-1), aux))
		return(R[R$term!='solution_formula', ])
	}
