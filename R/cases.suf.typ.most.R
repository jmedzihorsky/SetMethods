cases.suf.typ.most <-
function(results,
		 outcome,
		 solution=1)
	{
		R <- cases.suf.typ(results, outcome, solution=1)
		return(R[R$most_typical, ])
	}
