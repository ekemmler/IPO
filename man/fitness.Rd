\name{fitness}
\alias{fitness}
\title{
Calculation of fitness value for genetic Algorithm
}
\description{
    This function calculates a fitness value for an individual of the genetic Algorithm by 
	using the function calcPPS.
}
\usage{
    fitness(params, samples = NULL, startingParams, nSlaves)
}
\arguments{
  \item{params}{A vector of optimized values for XCMS::findPeaks.centWave parameters. 
  Parameters (consider order): peakwidth(min), peakwidth(max),ppm, snthresh, mzdiff, prefilter, value_of_prefilter }
  \item{samples}{A list of files, passed to xcmsSet().}
  \item{startingParams}{
     A list of parameters which are needed by XCMS::findPeaks.centWave. Used for non optimized parameters.
  }
  \item{nSlaves}{Number of slaves the optimization process should spawn.}
}

\details{
    This function calculates a fitness value to evaluate the given parameter settings. The XCMS function xcmsSet() is used to
	calculate a xcmsSet which is evaluated by calcPPS(). Returns the PPS from clacPPS() as fitness value.
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
  \code{\link{getDefaultXcmsSetStartingParamsGA}}
  \code{\link{calcPPS}}
  \code{\link{optimizeXcmsSetGA}}
}
\examples{
\dontrun{
	mzmlfile <- file.path(find.package("msdata"), "microtofq/MM14.mzML")
	fitness(params=c(4.6, 110, 39.5, -0.0131, 1, 12.4, 230), samples=mzmlfile, startingParams=getDefaultXcmsStartingParamsGA(), nSlaves=4)
	}
}
\keyword{method}
