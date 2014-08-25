\name{pimplot}
\alias{pimplot}
\title{
	Prime implicant plots.
}
\description{
	Plot prime implicant plots from an object of class "qca".  
}
\usage{
pimdata(results, outcome, solution = 1)
}
\arguments{
	\item{results}{
		An object of class "qca".
	}
	\item{outcome}{
		A character string with the name of the outcome.
	}
	\item{solution}{
		A numeric vector where the first number indicates the number of the
		solution according to the order in the "qca" object.
	}
	\item{case_labels}{
		Logical. Print case labels?
	}
	\item{lab_color}{
		Color of the labels.
	}
	\item{lab_jitter}{
		Logical.  Add jitter to label positions?
	}
}
\details{
}
\value{
	A plot.
}
\references{
}
\author{
	Juraj Medzihorsky
}
\note{
}
\seealso{
	\code{\link[QCA:eqmcc]{eqmcc}}
	\code{\link[SetMethods:pimdata]{pimdata}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{ QCA }
