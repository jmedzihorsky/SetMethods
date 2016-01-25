pimdata <-
function(results,
		 outcome,
		 solution=1)
	{
		s <- results$solution[[solution]]
		P <- results$pims[colnames(results$pims)%in%s]
		P$solution_formula <- apply(P, 1, max)
		data <- results$tt$initial.data
		if (results$options$neg.out) {
			P$out <- 1-data[, outcome]
		} else {
			P$out <- data[, outcome]
		}
		return(P)
	}
