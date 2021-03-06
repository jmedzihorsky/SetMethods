pimdata <-
function(results,
			 outcome, intermed=FALSE,
			 solution=1)
	{if (!intermed){
		  s <- results$solution[[solution]]
		  P <- results$pims[colnames(results$pims)%in%s]}
   else{
      s <- results$i.sol$C1P1$solution[[solution]]
      P <- results$i.sol$C1P1$IC$overall$pims[colnames(results$i.sol$C1P1$IC$overall$pims)%in%s]}
		P$solution_formula <- apply(P, 1, max)
		data <- results$tt$initial.data
		if (results$options$neg.out) {
			P$out <- 1-data[, outcome]
		} else {
			P$out <- data[, outcome]
		}
		return(P)
	}
