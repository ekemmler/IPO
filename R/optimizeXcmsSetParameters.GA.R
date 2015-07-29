getDefaultXcmsSetStartingParamsGA <- function() 
{    
    return(list(min_peakwidth=c(1,35), max_peakwidth=c(35,80), ppm=c(5,100),
                mzdiff=c(-1,1), snthresh=c(1,20), noise=0, prefilter=c(0,10), 
                value_of_prefilter=c(50,150),  mzCenterFun="wMean", integrate=1, 
                fitgauss=FALSE, verbose.columns=FALSE, nSlaves=1))
 }


fitness <- function(params, samples = NULL, startingParams)
{
  
	xSet <- xcmsSet(files=samples, method="centWave", 
					      peakwidth=c(params[1], params[2]), ppm=params[3], 
				          snthresh=params[4], mzdiff=params[5],
				          prefilter=c(params[6], params[7]),
				          mzCenterFun=startingParams$mzCenterFun, integrate=startingParams$integrate,
			 	          fitgauss=startingParams$fitgauss, verbose.columns=startingParams$verbose.columns,
				      	  nSlaves = startingParams$nSlaves)

	pps <- calcPPS(xSet)[5]
	return(pps)
}	

optimizeXcmsSetGA <- function(files=NULL, startingParams=getDefaultXcmsSetStartingParamsGA(), parallel=TRUE, run=30, popSize=30, elitism=1, pcrossover=0.8, pmutation=0.2)
{  
	GA <- ga(type = "real-valued", fitness = fitness, samples=files, startingParam=startingParams,
				min = c(startingParams$min_peakwidth[1], startingParams$max_peakwidth[1], startingParams$ppm[1], startingParams$snthresh[1], startingParams$mzdiff[1], startingParams$prefilter[1], startingParams$value_of_prefilter[1]), 
				max = c(startingParams$min_peakwidth[2], startingParams$max_peakwidth[2], startingParams$ppm[2], startingParams$snthresh[2], startingParams$mzdiff[2], startingParams$prefilter[2], startingParams$value_of_prefilter[2]), 
				run = run, popSize = popSize, elitism = elitism, 
				pcrossover = pcrossover, pmutation = pmutation, keepBest = FALSE,
				parallel = parallel, monitor = plot, 
				names =c("peakwidth(min)", "peakwidth(max)","ppm", "snthresh", "mzdiff", "prefilter", "value_of_prefilter"))
  
  plot(GA)
  return(list(summary=summary(GA), result=GA)						 
}
