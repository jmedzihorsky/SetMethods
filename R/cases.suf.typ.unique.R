cases.suf.typ.unique <-
function(results,
		 outcome,
		 solution=1)
	{
		A <- cases.suf.typ(results, outcome, solution)
		cases <- unique(A$case)
		s <- vapply(cases, function(i) sum(A[A$case==i,3]>0.5), FUN.VALUE=numeric(1))
		R <- A[A$case %in% cases[s==1], ]
		return(R)
	}
