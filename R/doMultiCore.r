
doMultiCore = function(rhsFormula, data, operatorFunction, .export = NULL, .packages = NULL, ...){

  if (!("parallel" %in% installed.packages ())){
    install.packages("parallel", repos = "http://cran.rstudio.com/")
  }
  if (!("doParallel" %in% installed.packages ())){
    install.packages("doParallel", repos = "http://cran.rstudio.com/")
  }
  if (!("foreach" %in% installed.packages ())){
    install.packages("foreach", repos = "http://cran.rstudio.com/")
  }
  if (!("plyr" %in% installed.packages ())){
    install.packages("plyr", repos = "http://cran.rstudio.com/")
  }
  library(parallel)
  library(doParallel)
  library(foreach)
  library(plyr)
  
  aFormTerms = terms(rhsFormula)
	if(attr(aFormTerms, "response")) stop("Please use a right hand sided formula only")
	term.labels = attr(aFormTerms, "term.labels")
	bFound = term.labels %in% colnames(data)
	if (any(!bFound)){
		notFound = term.labels[!bFound]
		msg = paste("Could not find the following columns in the data frame",notFound)
		stop(msg)
	}
	cell.split = drop(interaction(data[term.labels]))
	nCores = detectCores()
	cpu.split = subdivCells(cell.split, nCores)
	aSplitList = split(data, cpu.split)
	cl = makeCluster(nCores)
	registerDoParallel(cl)
	print(paste("Please wait, processing on ", nCores, " CPU cores ..."))
	anExportList = c(.export, "doSingleCore")
  aPackagesList = c(.packages, "plyr")
	aResult =  foreach(i = 1:nCores, .combine = rbind, .packages = aPackagesList, .export = anExportList) %dopar% doSingleCore(aSplitList[[i]], rhsFormula, operatorFunction, aProgress = "none", ...) 
	stopCluster(cl)
	return(aResult)
}

doSingleCore = function(data, rhsFormula, operatorFunction, aProgress = progress_win(title="processing ..."), ... ){
  aResult = ddply(data, rhsFormula, .fun = operatorFunction, .progress = aProgress, ...)
	return(aResult)
}

subdivCells = function(cellID, numDiv){
	cellID = as.factor(cellID)
	nCells = length(levels(cellID))
	idx = subdiv(nCells, numDiv)
	dvIdx = vector(length = length(cellID))
	for (i in 1:numDiv){
		dvIdx[cellID %in% levels(cellID)[idx == i]] = i
	}
	return(dvIdx)
}

subdiv = function(nCells, numDiv){
	idx = rep(1:numDiv, ceiling(nCells/numDiv))[1:nCells]
	return(idx)
}

