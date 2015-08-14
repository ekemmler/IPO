\name{getDefaultRetGroupStartingParamsGA}
\alias{getDefaultRetGroupStartingParamsGA}

\title{
	For the optimisation with genetic Algorithm. Gives a List of parameters for xcms-method retcor.loess
  and group.density which are optimized by default
}
\description{
  This function creates a list of parameters used in the xcms-method 
  retcor.loess and group.density. Per default the following parameters 
  have a defined range where optimization should start:\cr
  retcor.loess parameters: 'missing'; 'extra', 'span'\cr
  group.density parameters: 'bw', 'minfrac', 'mzwid'\cr  
}
\usage{
  getDefaultRetGroupStartingParamsGA()
}

\arguments{
}
\details{
  * Do not delete a parameter from the list returned.\cr
  * Optimization of qualitative parameters is not supported yet.\cr
  * If you want to optimize additional parameter just set an lower and
  an upper bound (e.g. params$max <- c(4,8))\cr
  * If you dont want to optimize a parameter set a default value 
  (e.g. params$max <- 10)
}
\value{
  A List of parameters used in the xcms-methods retcor.loess and group.density
}
\author{
Gunnar Libiseller, Emanuel Kemmler
}
\examples{
params <- getDefaultRetGroupStartingParams()
params$bw <- 10
params$missing <- c(0,4)
params
}

\keyword{method}

