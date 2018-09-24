
getPseudoGroupPost = function(codaSamples) {
  
  shapeNames = varnames(codaSamples)[grep('shape',varnames(codaSamples))]
  scaleNames = varnames(codaSamples)[grep('scale',varnames(codaSamples))]
  shiftNames = varnames(codaSamples)[grep('shift',varnames(codaSamples))]
  
  codaDF = as.data.frame(as.matrix(codaSamples))
  
  
  #make pseudo-group posteriors for shape, scale, shift
  shapePGP = rowMeans(codaDF[,shapeNames])
  scalePGP = rowMeans(codaDF[,scaleNames])
  shiftPGP = rowMeans(codaDF[,shiftNames])
  
  return(list(shapePGP,scalePGP,shiftPGP))
  
}


weibMean = function(shape, scale){
  
  gammaTerm = 1 + 1/shape
  
  weibMean = scale*gamma(gammaTerm)
  
  return(weibMean)
  
}

weibSTD = function(shape, scale){
  
  firstGammaTerm = 1 + (2/shape)
  secondGammaTerm = 1+ (1/shape)
  scaleSquared = scale^2
  
  firstTerm = gamma(firstGammaTerm)
  secondTerm = gamma(secondGammaTerm)
  secondTermSquared = secondTerm^2
  
  weibVar = scaleSquared * (firstTerm - secondTermSquared)
  
  weibSTD = sqrt(weibVar)
  
  return(weibSTD)
  
}

weibMeanFromPost = function(codaSamples, cond){
  
  shapeName = paste('shape[',cond,']',sep='')
  scaleName = paste('scale[',cond,']',sep='')
  shiftName = paste('shift[',cond,']',sep='')
  
  codaDF = as.data.frame(as.matrix(codaSamples))
  
  shapeDens = density(codaDF[,shapeName])
  shapeMode = shapeDens$x[which.max(shapeDens$y)]
  
  scaleDens = density(codaDF[,scaleName])
  scaleMode = scaleDens$x[which.max(scaleDens$y)]
  
  shiftDens = density(codaDF[,shiftName])
  shiftMode = shiftDens$x[which.max(shiftDens$y)]
  
  unshiftedPostMean = weibMean(shapeMode, scaleMode)
  
  shiftedPostMean = unshiftedPostMean + shiftMode
  
  return(shiftedPostMean)

}

weibSTDFromPost = function(codaSamples, cond){
  
  shapeName = paste('shape[',cond,']',sep='')
  scaleName = paste('scale[',cond,']',sep='')
  
  codaDF = as.data.frame(as.matrix(codaSamples))
  
  shapeDens = density(codaDF[,shapeName])
  shapeMode = shapeDens$x[which.max(shapeDens$y)]
  
  scaleDens = density(codaDF[,scaleName])
  scaleMode = scaleDens$x[which.max(scaleDens$y)]
  
  postSTD = weibSTD(shapeMode, scaleMode)
  
  return(postSTD)
  
}


HDIofMCMC = function(sampleVec, credMass=0.95) {
  # Computes highest density interval from a sample of representative values,
  #  estimated as shortest credible interval.
  # Args:
  #  sampleVec: a vector of representative values from a probability distribution
  #  credMass: a scaler between 0 and 1, indicates the mass within the credible 
  #            interval that is to be estimated.
  # Value:
  #  HDIlim: vector containing the limits of the HDI
  sortedPts = sort(sampleVec)
  ciIdxInc = ceiling(credMass*length(sortedPts))
  nCIs = length(sortedPts) - ciIdxInc
  ciWidth = rep(0,nCIs)
  for (i in 1:nCIs) {
    ciWidth[i] = sortedPts[i+ciIdxInc]-sortedPts[i]
  }
  HDImin = sortedPts[which.min(ciWidth)]
  HDImax = sortedPts[which.min(ciWidth)+ciIdxInc]
  HDIlim = c(HDImin,HDImax)
  return(HDIlim)
}

computeChiSq = function(behavData, shape, scale, shift, theBins){
  
  #get counts for each bin
  bins = theBins
  counts = hist(behavData$rt, breaks=bins, plot=FALSE)$counts
  histDens = hist(behavData$rt, breaks=bins, plot=FALSE)$density
  #normalize histDens
  histDens = histDens*(1/sum(histDens))
  
  #get cumulative probability
  binProbs = NULL
  for (i in 1:(length(bins)-1)){
    binProbs[i] = cumWeib(bins[i+1], shape, scale, shift) - cumWeib(bins[i], shape, scale, shift)
  }
  #normalize binProbs, for chisquare test
  binProbs = binProbs*(1/sum(binProbs))
  
  #do the test
  goodFitTest = chisq.test(x = histDens, p=binProbs) #hist densities against bin probabilities
  
  #calculate RMSE
  rMeanSqE = RMSE(binProbs,histDens)
  
  
  return(list(goodFitTest, rMeanSqE))
  
  
}

cumWeib = function(x, shape, scale, shift){
  
  cumWeibVal = 1 - exp(-((x-shift)/scale)^(shape))
  
  return(cumWeibVal)
  
}

  
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
} 
  
  
  
  
