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

optimizeXcmsSetGA <- function(files=NULL, params=getDefaultXcmsSetStartingParamsGA(), parallel=TRUE, run=30, popSize=30, elitism=1, pcrossover=0.8, pmutation=0.2, monitor="plot")
{  
	GA <- ga(type = "real-valued", fitness = fitness, samples=files, startingParam=params,
				min = c(params$min_peakwidth[1], params$max_peakwidth[1], params$ppm[1], params$snthresh[1], params$mzdiff[1], params$prefilter[1], params$value_of_prefilter[1]), 
				max = c(params$min_peakwidth[length(params$min_peakwidth)], params$max_peakwidth[length(params$max_peakwidth)], params$ppm[length(params$ppm)],
				params$snsthresh[length(params$snthresh)], params$mzdiff[length(params$mzdiff)], params$prefilter[length(params$prefilter)], 
				params$value_of_prefilter[length(params$value_of_prefilter)]), 
				run = run, popSize = popSize, elitism = elitism, 
				pcrossover = pcrossover, pmutation = pmutation, keepBest = FALSE,
				parallel = parallel, monitor = monitor, 
				names =c("peakwidth(min)", "peakwidth(max)","ppm", "snthresh", "mzdiff", "prefilter", "value_of_prefilter"))
  
  plot(GA)
  return(summary(GA))						 
}
