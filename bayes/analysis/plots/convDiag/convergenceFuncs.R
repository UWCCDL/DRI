#functions to examine MCMC convergence

#determine the autocorrelation between samples across a range of lags and produce a plot
MCMCacf = function(codaSamples, parName = varnames(codaSamples)[1]) {
  
  nChain = length(codaSamples)
  xMat = NULL
  yMat = NULL
  chainIdx = NULL
  
  for (cIdx in 1:nChain) {
    
    acfInfo = acf(codaSamples[,parName][[cIdx]],plot=FALSE)
    xMat = cbind(xMat,acfInfo$lag)
    yMat = cbind(yMat,acfInfo$acf)
    
    numLags = nrow(xMat)
    chainIdx = c(chainIdx,rep(cIdx,numLags))
    
  }
  
  acfDF = data.frame(c(xMat),c(yMat))
  colnames(acfDF) = c('lag','acf')
  acfDF$chainIdx = as.factor(chainIdx)
  #can divide lag interval by thinning size to get 0,1,2,3... lags:
  #thinSize = acfDF$lag[2] - acfDF$lag[1]
  #acfDF$lag = acfDF$lag/thinSize
  
  effChnLngth = effectiveSize(codaSamples[,parName])
  
  require(ggplot2)
  plot = ggplot(acfDF, aes(group=chainIdx, col=chainIdx)) + 
    geom_hline(aes(yintercept=0)) + 
    geom_segment(aes(x=lag, y=acf, xend=lag, yend=acf-acf)) + 
    geom_point(aes(x=lag, y=acf),size=3) +
    annotate('text', x = max(xMat)/1.25, y = max(yMat)/1.25, label=paste('ESS = ', round(effChnLngth),sep='')) + 
    ggtitle('Autocorrelation of MCMC chains across sample lags')
  
  return(plot)
  
}


MCMC_MCSE = function(codaSamples, parName = varnames(codaSamples)[1]) {
  
  if (parName=='all'){
    parName = varnames(codaSamples)
  }
  
  
  MCSEs = NULL
  for (param in parName) {
    
    print(param)
    
    effChnLngth = effectiveSize(codaSamples[,param])
    MCSE = sd(as.matrix(codaSamples[,param]))/sqrt(effChnLngth) 
    MCSEs = c(MCSEs, MCSE)
    
  }
  
  MCSEdf = data.frame(parName,MCSEs)
  return(MCSEdf)
  
}


MCMCpostDens = function(codaSamples, parName = varnames(codaSamples)[1]) {
  
  nChain = length(codaSamples)
  
  xMat = NULL
  yMat = NULL
  hdiLims = NULL
  xColNames = NULL
  yColNames = NULL
  chainIdx = NULL
  
  for (cIdx in 1:nChain) {
    
    densInfo = density(codaSamples[,parName][[cIdx]])
    xMat = cbind(xMat,densInfo$x)
    yMat = cbind(yMat,densInfo$y)
    hdiLims = cbind(hdiLims,HDIofMCMC(codaSamples[,parName][[cIdx]]))
    
    numPoints = nrow(xMat)
    chainIdx = c(chainIdx,rep(cIdx,numPoints))
    
  }
  
  maxProbDens = max(densInfo$y)
  
  densDF = data.frame(c(xMat),c(yMat))
  colnames(densDF) = c('x','y')
  densDF$chainIdx = as.factor(chainIdx)
  
  effChnLngth = effectiveSize(codaSamples[,parName])
  MCSE = sd(as.matrix(codaSamples[,parName]))/sqrt(effChnLngth) 
  
  require(ggplot2)
  plot = ggplot(densDF, aes(x = x, y = y, group = chainIdx, color = chainIdx)) +
    geom_line() +
    geom_segment(aes(x=hdiLims[1,1], y = 0, xend=hdiLims[2,1], yend = 0), color = 'red') + geom_segment(aes(x=hdiLims[1,1], y=-maxProbDens/50, xend=hdiLims[1,1], yend=maxProbDens/50), color = 'red') + geom_segment(aes(x=hdiLims[2,1], y=-maxProbDens/50, xend=hdiLims[2,1], yend=maxProbDens/50), color = 'red') + 
    geom_segment(aes(x=hdiLims[1,2], y = maxProbDens/25, xend=hdiLims[2,2], yend = maxProbDens/25), color = 'green') + geom_segment(aes(x=hdiLims[1,2], y=(-maxProbDens/50)+maxProbDens/25, xend=hdiLims[1,2], yend=(maxProbDens/50)+maxProbDens/25), color = 'green') + geom_segment(aes(x=hdiLims[2,2], y=(-maxProbDens/50)+maxProbDens/25, xend=hdiLims[2,2], yend=(maxProbDens/50)+maxProbDens/25), color = 'green') + 
    geom_segment(aes(x=hdiLims[1,3], y = -maxProbDens/25, xend=hdiLims[2,3], yend = -maxProbDens/25), color = 'blue') + geom_segment(aes(x=hdiLims[1,3], y=(-maxProbDens/50)-maxProbDens/25, xend=hdiLims[1,3], yend=(maxProbDens/50)-maxProbDens/25), color = 'blue') + geom_segment(aes(x=hdiLims[2,3], y=(-maxProbDens/50)-maxProbDens/25, xend=hdiLims[2,3], yend=(maxProbDens/50)-maxProbDens/25), color = 'blue') + 
    xlab(parName) +
    ylab('Density') +
    ggtitle(paste('Posterior densities of parameter ',parName,sep='')) +
    annotate('text',x = max(densDF$x)/1.25, y = max(densDF$y)/1.25, label = paste('MCSE = ',round(MCSE, digits = 6),sep=''))
  
  return(plot)
  
}

#function to get ESS of all estimated parameters
MCMCess = function(codaSamples) {
  
  require(coda)
  
  parNames = varnames(codaSamples)
  ESS = NULL
  for (parName in parNames) { ESS = c(ESS,effectiveSize(codaSamples[,parName])) }
  
  paramESS = data.frame(parNames,ESS)
  
  return(paramESS)
  
}

chainTracePlot = function(codaSamples, parName = varnames(codaSamples)[1]){
  
  nChain = length(codaSamples)
  xMat = NULL
  yMat = NULL
  chainIdx = NULL
  
  for (cIdx in 1:nChain){
    
    lenChain = length(codaSamples[,parName][[cIdx]])
    if (lenChain >= 5000){
      last5k = codaSamples[,parName][[cIdx]][(lenChain-4999):lenChain]
    
      xMat = cbind(xMat, seq(from = (lenChain-4999), to = lenChain))
      yMat = cbind(yMat, last5k)
    
      chainIdx = c(chainIdx,rep(cIdx,length(last5k)))
    } else {
      last1k = codaSamples[,parName][[cIdx]][(lenChain-999):lenChain]
      
      xMat = cbind(xMat, seq(from = (lenChain-999), to = lenChain))
      yMat = cbind(yMat, last1k)
      
      chainIdx = c(chainIdx,rep(cIdx,length(last1k)))
    }
  }
  
  chainDF = data.frame(c(xMat),c(yMat))
  colnames(chainDF) = c('x','y')
  chainDF$chainIdx = as.factor(chainIdx)
  
  require(ggplot2)
  plot = ggplot(chainDF, aes(x = x, y = y, group = chainIdx, color = chainIdx)) +
    geom_line() +
    #geom_point() +
    xlab('Sampling iteration') +
    ylab('Parameter value') +
    ggtitle(paste('Trace plot of ',parName,' sampling, last 5000 samples',sep=''))
  
  return(plot)
  
}

shrinkFactorPlot = function(codaSamples, parName = varnames(codaSamples)[1], maxBins = 50) {
  
  gDiagOutput = gelmanDiag(codaSamples, parName, maxBins)
  grStats = gDiagOutput[1][[1]]
  binIdx = gDiagOutput[2][[1]]
  x = seq(1, length(grStats))
  
  plotDF = data.frame(x = binIdx, y = grStats)
  
  library(grid)
  library(scales)
  placeholder = textGrob('shrink factor plot here')
  
  plot = ggplot(plotDF, aes(x = x, y = y)) + 
    geom_line() + 
    scale_x_continuous(labels = comma) + 
    xlab('Last iteration in chain') + 
    ylab('Potential Scale Reduction Factor') + 
    ggtitle(paste('Gelman-Rubin diagnostic for parameter ',parName, sep='')) +
    ylim(0.98,1.02)
  
  return(plot)
}

convergenceDiag = function(codaSamples, parName = varnames(codaSamples)[1], saveFolder){
  
  if (parName=='all'){
    parName = varnames(codaSamples)
  }
  
  for (param in parName) {
    
    #chain trace plot
    tracePlot = chainTracePlot(codaSamples, parName = param)
    
    #autocorrelation plot
    acfPlot = MCMCacf(codaSamples, parName = param)
    
    #shrink factor plot
    sfPlot = shrinkFactorPlot(codaSamples, parName = param, maxBins = 50)
    
    #posterior overlay plot
    postDensPlot = MCMCpostDens(codaSamples, parName = param)
    
    require(gridExtra)
    plots = arrangeGrob(tracePlot,acfPlot,sfPlot,postDensPlot)
    ggsave(file = paste(saveFolder,param,'_convergence.pdf',sep=''), plots, width = 11, height = 8.5, units = 'in')
  }
  
}

gelmanDiag = function(codaSamples, parName, maxBins=NULL){
  
  nChains = length(codaSamples)
  nSampPerC = nrow(codaSamples[[1]])
  
  if (is.null(maxBins)){
    chainMeans = NULL
    chainVars = NULL
    for (i in 1:nChains){
      chainMeans[i] = mean(codaSamples[[i]][,parName])
      chainVars[i] = var(codaSamples[[i]][,parName])
    }
    
    overallMean = mean(chainMeans)
    betweenChainVar = (nSampPerC/(nChains-1)) * sum((chainMeans - overallMean)^2)
    withinChainVar = (1/nChains) * sum(chainVars)
    pooledVar = (((nSampPerC - 1)/nSampPerC) * withinChainVar) + ((nChains + 1)/(nChains * nSampPerC)) * betweenChainVar
    grStat = pooledVar/withinChainVar
    
    return(grStat)
    
  } else {
    bins = round(seq(1, nSampPerC, length.out = maxBins))
    bins = bins[2:maxBins]
    chainMeans = matrix(,nrow = nChains, ncol = (length(bins)))
    chainVars = matrix(,nrow = nChains, ncol = (length(bins)))
    for (i in 1:nChains){
      for (b in 1:(length(bins))){
        chainMeans[i,b] = mean(codaSamples[[i]][,parName][1:bins[b]])
        chainVars[i,b] = var(codaSamples[[i]][,parName][1:bins[b]])
      }
    }
    
    overallMeans = colMeans(chainMeans)
    grStat = NULL
    for (b in 1:(length(bins))){
      nSampPerSubC = length(1:bins[b])
      betweenChainVar = (nSampPerSubC/(nChains-1)) * sum((chainMeans[,b] - overallMeans[b])^2)
      withinChainVar = (1/nChains) * sum(chainVars[,b])
      pooledVar = (((nSampPerSubC - 1)/nSampPerSubC) * withinChainVar) + ((nChains + 1)/(nChains * nSampPerSubC)) * betweenChainVar
      grStat[b] = pooledVar/withinChainVar
      
    }
  }
  return(list(grStat,bins))
}


