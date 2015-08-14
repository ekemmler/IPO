\name{fitnessRG}
\alias{fitnessRG}
\title{
Calculation of fitness value for genetic Algorithm for grouping and retention-time-correction
}
\description{
    This function calculates a fitness value for an individual of the genetic Algorithm by 
	using the function getRGTVValues.
}
\usage{
    fitness(params, xSet = NULL, startingParams)
}
\arguments{
  \item{params}{A vector of optimized values for retcor.loess and group.density parameters. 
  Parameters (consider order): minfrac, bw, mzwid, missing, extra, span }
  \item{xSet}{a xcmsSet}
  \item{startingParams}{
     A list of parameters which are needed by retcor.loess and group.density. Used for non optimized parameters.
  }
}

\details{
    This function calculates a fitness value to evaluate the given parameter settings. The XCMS functions group() and retcor() are used to
	calculate a xcmsSet which is evaluated by a score using getRGTVValues().
}
\references{

  Smith, C.A. and Want, E.J. and O'Maille, G. and Abagyan,R. and
  Siuzdak, G.: XCMS: Processing mass spectrometry data for metabolite
  profiling using nonlinear peak alignment, matching and
  identification, Analytical Chemistry, 78:779-787 (2006)

  Ralf Tautenhahn, Christoph Boettcher, Steffen Neumann: Highly
  sensitive feature detection for high resolution LC/MS BMC
  Bioinformatics, 9:504 (2008)

}
\author{
Gunnar Libiseller, Emanuel Kemmler
}
\seealso{
  \code{\link{getDefaultRetGroupStartingParamsGA}}
  \code{\link{getRGTVValues}}
  \code{\link{optimizeRetGroupGA}}
}
\examples{
\dontrun{
	mzmlfile <- file.path(find.package("msdata"), "microtofq/MM14.mzML")
	xSet <- xcmsSet(mzmlfile)
	fitness(params=c(1, 5, 0.5, 1, 2, 1), samples=mzmlfile, startingParams=getDefaultXcmsStartingParamsGA())
	}
}
\keyword{method}
