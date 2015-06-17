library(GA)
library(xcms)
library(IPO)

getDefaultXcmsSetStartingParamsGA <- function() 
{    
    return(list(min_peakwidth=c(1,35), max_peakwidth=c(35,80), ppm=c(5,100),
                mzdiff=c(-1,1), snthresh=c(1,20), noise=0, prefilter=3, 
                value_of_prefilter=100,  mzCenterFun="wMean", integrate=1, 
                fitgauss=FALSE, verbose.columns=FALSE))
 }


fitness <- function(param, samples = NULL)
{
  
	xSet <- xcmsSet(files=samples, method="centWave", 
					        peakwidth=c(param[1], param[2]), ppm=param[3], 
				          snthresh=param[4], mzdiff=param[5],
				          prefilter=c(3, 100),
				          mzCenterFun="wMean", integrate=1,
			 	          fitgauss=FALSE, verbose.columns=FALSE,
				      	  nSlaves = 1)

	pps <- calcPPS(xSet)[5]
	return(pps)
}	

optimizeXcmsSetGA <- function(files=NULL, params=getDefaultXcmsSetStartingParamsGA(), parallel=TRUE, run=30, popSize=30, elitism=1, pcrossover=0.8, pmutation=0.2)
{  
	GA <- ga(type = "real-valued", fitness = fitness(files=files), 
				min = c(params$min_peakwidth[1], params$max_peakwidth[1], params$ppm[1], params$snthresh[1], params$mzdiff[1]), 
				max = c(params$min_peakwidth[2], params$max_peakwidth[2], params$ppm[2], params$snthresh[2], params$mzdiff[2]), 
				run = run, popSize = popSize, elitism = elitism, 
				pcrossover = pcrossover, pmutation = pmutation, keepBest = FALSE,
        parallel = parallel, monitor = plot, 
				names =c("peakwidth(min)", "peakwidth(max)","ppm", "snthresh", "mzdiff"))
  
  plot(GA)
  summary(GA)
  return(GA)
							 
}
