condRTHist_plusPPDTESTING = function(codaDF, behavData, cond, bins, condLabel, fname){
  
  if (cond == 'all'){
    cond = as.numeric(as.factor(unique(behavData$cond)))
  }
  
  conds = sort(unique(behavData$cond))
  behavData$cond = as.numeric(as.factor(behavData$cond))
  
  for (c in cond){
    
    #create weibull parameter names
    shapeName = paste('shape[',c,']',sep='')
    scaleName = paste('scale[',c,']',sep='')
    shiftName = paste('shift[',c,']',sep='')
    
    #get the mode of this condition's shape parameter
    postDens = density(codaDF[,shapeName])
    shapeMode = postDens$x[which.max(postDens$y)]
    shapeMean = mean(codaDF[,shapeName])
    
    #get the mode of this condition's scale parameter
    postDens = density(codaDF[,scaleName])
    scaleMode = postDens$x[which.max(postDens$y)]
    scaleMean = mean(codaDF[,scaleName])
    
    #get the mode of this condition's shift parameter
    postDens = density(codaDF[,shiftName])
    shiftMode = postDens$x[which.max(postDens$y)]
    shiftMean = mean(codaDF[,shiftName])
    
    #create histogram of subj RTs
    condData = subset(behavData, behavData$cond == c)
    
    png(filename = fname, width=5/2, height = 5/2, units='in', res=1200, pointsize=6)
    
    h = hist(condData$rt, 
             freq=F, 
             breaks=bins,
             xlim = c(0, 3.5),
             ylim = c(0, 2),
             xlab = expression(bold('Response Time (s)')),
             ylab = expression(bold('Probability Density')),
             #ylab = NULL,
             #main = paste('RTs and posterior predictive Weibull distribution, \n',condLabel,sep=''),
             #main=NULL,
             main=condLabel,
             xaxt='n',
             yaxt='n')
    
    axis(1,seq(0,3.5,0.5),pos=0) #x-axis
    axis(2,seq(0,2,0.5),pos=0) #y-axis
    
    #create posterior weibull density
    x = seq(0, max(condData$rt)+0.5, length=100)
    
    #mode
    condPostWeib = dweibull(x-shiftMode, shape = shapeMode, scale = scaleMode)
    lines(x, condPostWeib, col='blue', lwd=2)
    
    #get a measure of the fit
    fitMetrics = computeChiSq(condData, shapeMode, scaleMode, shiftMode, theBins = bins)
    
    fitStats = fitMetrics[[1]]
    condRMSE = fitMetrics[[2]]

    #parameter values and chi-sq stats
    # text(x = c(2.75, 2.75, 2.75, 2.75, 2.75, 2.75),
    #      y = c(2*0.95,2*0.9,2*0.85,
    #            2*0.6,2*0.55,2*0.5),
    #      labels = c(paste('shape Mode = ',round(shapeMode,2),sep=''),
    #                 paste('scale Mode = ',round(scaleMode,2),sep=''),
    #                 paste('shift Mode = ',round(shiftMode,2),sep=''),
    #                 paste('X-squared = ',round(as.numeric(fitStats$statistic),3),sep=''),
    #                 paste('DF = ',as.numeric(fitStats$parameter),sep=''),
    #                 paste('p-value = ',round(as.numeric(fitStats$p.value),3),sep='')))
    
    #parameter values only
    text(x = c(2.75, 2.783, 2.825),
         y = c(2*0.95,2*0.9,2*0.85),
         labels = c(paste('shape Mode = ',round(shapeMode,2),sep=''),
                    paste('scale Mode = ',round(scaleMode,2),sep=''),
                    paste('shift Mode =',sprintf('%1.2f',round(shiftMode,2),sep=''))))   

    
    
    dev.off()
    
    return(fitMetrics)

  }
}

