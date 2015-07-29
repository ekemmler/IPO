\name{optimizeXcmsSetGA}
\alias{optimizeXcmsSetGA}
\title{
Optimisation of peak picking parameters with genetic Algorithm by using natural, stable 13C isotopes
}
\description{
    This function provides optimisation of peak picking parameters with genetic Algorithm by using natural, 
    stable 13C isotopes.
}
\usage{
optimizeXcmsSetGA(files, startingParams=getDefaultXcmsSetStartingParamsGA(), 
  parallel=TRUE, run=30, popSize=30, elitism=1, pcrossover=0.8, pmutation=0.2)
}
\arguments{
  \item{files}{A list of files, passed to xcmsSet().}
  \item{startingParams}{
     A list of parameters which are needed by XCMS::findPeaks.centWave. List-items with
     two values will be optimized. The first value defines the lower limit, the 
     second one the upper limit.
  }
  \item{parallel}{Parameter of the GA package. If TRUE, different parameter settings will be calculated parallel.}
  \item{run}{Parameter of the GA package. The number of consecutive iterations without any improvement in the best parameter settings
  before the GA is stopped.}
  \item{popSize}{Parameter of the GA package. Number of different parameter settings per iteration.}
  \item{elitism}{Parameter of the GA package. The number of best parameter settings to keep at each iteration}
  \item{pcrossover}{Parameter of the GA package. The probability of crossover between pairs of parameter settings. Used for generating new parameter settings.}
  \item{pmutation}{Parameter of the GA package. Provides a higher variation in the parameter settings}    
}
\details{
    This function provides optimisation of centWave parameters with genetic Algorithm by using the PPS from calcPPS() as fitness value.
}
\references{

  Scrucca L (2012).  GA: A Package for Genetic Algorithms in R.
  Journal of Statistical Software, 53(4), 1-37,
  http://www.jstatsoft.org/v53/i04/

  Smith, C.A. and Want, E.J. and O'Maille, G. and Abagyan,R. and
  Siuzdak, G.: XCMS: Processing mass spectrometry data for metabolite
  profiling using nonlinear peak alignment, matching and
  identification, Analytical Chemistry, 78:779-787 (2006)

  Ralf Tautenhahn, Christoph Boettcher, Steffen Neumann: Highly
  sensitive feature detection for high resolution LC/MS BMC
  Bioinformatics, 9:504 (2008)

  H. Paul Benton, Elizabeth J. Want and Timothy M. D. Ebbels: Correction
  of mass calibration gaps in liquid chromatography-mass spectrometry
  metabolomics data Bioinformatics, 26:2488 (2010)
  
  C. Kuhl and R. Tautenhahn and C. Boettcher and T. R. Larson and S. Neumann: CAMERA: 
  an integrated strategy for compound spectra extraction
  and annotation of liquid chromatography/mass spectrometry data sets 
  Analytical Chemistry 84:283 (2012)

}
\author{
Gunnar Libiseller, Emanuel Kemmler
}
\seealso{
  \code{\link{getDefaultXcmsSetStartingParamsGA}}
  \code{\link{fitness}}
  \code{\link{calcPPS}}
  \code{\link{findIsotopes.IPO}}
  \code{\link{findIsotopes.CAMERA}}
}
\examples{
\dontrun{
library(IPO)
 
mzdatafiles <- list.files(file.path(find.package("mtbls2"), "mzdata"), 
                          full.names=TRUE)[1:4]
 
resultPP <- optimizeXcmsSetGA(mzdatafiles)

}
}
\keyword{method}

