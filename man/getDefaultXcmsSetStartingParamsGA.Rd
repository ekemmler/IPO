\name{getDefaultXcmsSetStartingParamsGA}
\alias{getDefaultXcmsSetStartingParamsGA}
\title{
  For the optimisation with genetic Algorithm. Creates a List of parameters for xcms-methods xcmsSet.findPeak which are optimized by default
}
\description{
  This function creates a list of parameters used in the xcmsSet.findPeak-method 
  'centWave' for the optimisation with genetic Algorithm. Per default the following parameters 
  have a defined range where optimization should start:\cr
  'peakwidth' (split into 'min_peakwidth' and 'max_peakwidth'), 
  'ppm', 'mzdiff', 'snthresh', 'prefilter' (split into 'prefilter' and 'value_of_prefilter').\cr
  With 'nSlaves' it is possible to specify the number of slaves the fitness function should spawn.\cr
}
\usage{
getDefaultXcmsSetStartingParams())
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
  A List of parameters for the xcmsSet.findPeak-method
  'centWave'
}
\author{
Gunnar Libiseller, Emanuel Kemmler
}
\examples{
params <- getDefaultXcmsSetStartingParamsGA()
params$snthresh <- c(5,15)



## The function is currently defined as
function() {
return(list(min_peakwidth=c(1,35), max_peakwidth=c(35,80), ppm=c(5,100),
                mzdiff=c(-1,1), snthresh=c(1,20), noise=0, prefilter=c(0,10), 
                value_of_prefilter=c(50,150),  mzCenterFun="wMean", integrate=1, 
                fitgauss=FALSE, verbose.columns=FALSE, nSlaves=1)) 
  }

}
\keyword{method}

