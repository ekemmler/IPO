getDefaultXcmsSetStartingParamsGA <- function() 
{    
    return(list(min_peakwidth=c(1,35), max_peakwidth=c(35,80), ppm=c(5,100),
                mzdiff=c(-1,1), snthresh=c(1,20), noise=0, prefilter=c(0,10), 
                value_of_prefilter=c(50,150),  mzCenterFun="wMean", integrate=1, 
                fitgauss=FALSE, verbose.columns=FALSE))
 }


fitness <- function(param, samples = NULL, startingParam, nSlaves)
{
  
	xSet <- xcmsSet(files=samples, method="centWave", 
					      peakwidth=c(param[1], param[2]), ppm=param[3], 
				          snthresh=param[4], mzdiff=param[5],
				          prefilter=c(param[6], param[7]),
				          mzCenterFun=startingParam$mzCenterFun, integrate=startingParam$integrate,
			 	          fitgauss=startingParam$fitgauss, verbose.columns=startingParam$verbose.columns,
				      	  nSlaves = nSlaves)

	pps <- calcPPS(xSet)[5]
	return(pps)
}	

optimizeXcmsSetGA <- function(files=NULL, startingParams=getDefaultXcmsSetStartingParamsGA(), parallel=TRUE, plot=plot, run=30, popSize=30, elitism=1, pcrossover=0.8, pmutation=0.2, nSlaves=4)
{  
	GA <- ga(type = "real-valued", fitness = fitness, c(files, startingParams, nSlaves),
				min = c(startingParams$min_peakwidth[1], startingParams$max_peakwidth[1], startingParams$ppm[1], startingParams$snthresh[1], startingParams$mzdiff[1], startingParams$prefilter[1], startingParams$value_of_prefilter[1]), 
				max = c(startingParams$min_peakwidth[2], startingParams$max_peakwidth[2], startingParams$ppm[2], startingParams$snthresh[2], startingParams$mzdiff[2], startingParams$prefilter[2], startingParams$value_of_prefilter[2]), 
				run = run, popSize = popSize, elitism = elitism, 
				pcrossover = pcrossover, pmutation = pmutation, keepBest = FALSE,
				parallel = parallel, monitor = plot, 
				names =c("peakwidth(min)", "peakwidth(max)","ppm", "snthresh", "mzdiff", "prefilter", "value_of_prefilter"))
  
  plot(GA)
  summary(GA)
  return(GA)						 
}
