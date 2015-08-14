getDefaultRetGroupStartingParamsGA <- function()
{
	return(list(bw=c(0.5,10), minfrac=c(0,1.5), minsamp=1, mzwid=c(0,1), max=50, sleep=0,  ##group.density parameters
			missing=c(0,7), extra=c(0,8), smooth ="loess", span=c(0,2), family="gaussian"))  ##retcor.loess parameters
}

fitnessRG <- function(params, xSet, startingParams) 
{
  xSet <- group(xSet, minfrac=param[1], bw=param[2], minsamp=startingParams$minsamp, mzwid=param[3], max=startingParams$max, sleep=startingParams$sleep) 
  ## Check whether any groups were found
  if ( length(groups(mtblsSet)>0)) 
  {
    xSet <- retcor(xSet, missing=param[4], extra=param[5], span=param[6], smooth=startingParams$smooth, family=startingParams$family)
    xSet <- group(xSet, minfrac=param[1], bw=param[2], minsamp=startingParams$minsamp, mzwid=param[3], max=startingParams$max, sleep=startingParams$sleep)
  
    RGTVValues <- IPO:::getRGTVValues(xSet)  
  
    bad_groups_bool <- RGTVValues$bad_groups == 0
    RGTVValues$bad_groups[bad_groups_bool] <- 1
    RGTVValues$good_groups[bad_groups_bool] <- RGTVValues$good_groups[bad_groups_bool] + 1
  
    GS <- RGTVValues$good_groups ^ 2 / RGTVValues$bad_groups
    RCS <- 1/RGTVValues$mean_rel_rt_diff

    return(GS * RCS)
  }
  else{return(0)}
}

optimizeRetGroupGA <- function(xSet=NULL, params=getDefaultRetGroupStartingParamsGA(), parallel=TRUE, run=50, popSize=30, elitism=1, pcrossover=0.8, pmutation=0.2, maxiter=500)
{
	GA <- ga(type = "real-valued", fitness = fitnessRG, xSet = xSet, startingParams = params,
							 min = c(params$minfrac[1], params$bw[1], params$mzwid[1], params$missing[1], params$extra[1], params$span[1]), 
                             max = c(params$minfrac[length(params$minfrac)], params$bw[length(params$bw)], params$mzwid[length(params$mzwid)],
							 params$missing[length(params$missing)], params$extra[length(params$extra)], params$span[length(params$span)]),
							 run = run, popSize = popSize, elitism = elitism, 
                             pcrossover = pcrossover, pmutation = pmutation, keepBest = TRUE,
                             parallel = TRUE, monitor = plot, maxiter = maxiter, 
                             names =c("gr_minfrac", "gr_bw", "gr_mzwid", "ret_missing", "ret_extra", "ret_span"))
	plot(GA)
	return(summary(GA))
}