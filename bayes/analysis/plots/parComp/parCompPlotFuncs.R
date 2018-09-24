#functions and script to create shape, scale, shift comparison plot for publication


#small theme for inset scatter plot
theme_Small <- function(base_size=5, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
  + theme(text = element_text(),
          line = element_line(size = rel(0.8)),
          plot.title = element_blank(),
          plot.background = element_blank(),
          plot.margin = margin(0,0,0,0,'mm'),
          axis.title = element_text(size = rel(0.6), face='bold'),
          axis.title.x = element_text(margin=margin(0,0,0,0)),
          axis.title.y = element_text(margin=margin(0,0,0,0)),
          axis.text = element_text(size=rel(0.6), face='bold'), 
          #axis.text.x = element_text(margin=margin(0,0,0,0)),
          #axis.text.y = element_text(margin=margin(0,0,0,0)),
          axis.line = element_line(colour="black", size=rel(0.2)),
          axis.ticks = element_line(size=rel(0.2)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
          
  ))
  
}

#theme for larger density plots. May be overwritten by cowplot in plotPostPubQ(), but it looks fine
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
  + theme(plot.title = element_text(face = "bold", size = rel(1)),
          text = element_text(),
          line = element_line(size=rel(1)),
          plot.background = element_blank(),
          panel.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          #axis.title.y = element_text(angle=90,vjust =2),
          axis.title.y = element_blank(),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(size=rel(1)), 
          axis.line = element_line(colour="black", size=rel(0.2)),
          axis.ticks = element_line(size=rel(0.2)),
          #panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          legend.margin = margin(0,0,0,0, "cm"),
          legend.title = element_text(face="italic"),
          plot.margin=unit(c(1,1,1,1),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
  ))
  
}


plotPostPubQ = function(par1PostVec, par2PostVec, numSPoints, vecName, plotLabel,bSize){
  
  postVec = par1PostVec - par2PostVec
  
  hdi = HDIofMCMC(postVec)
  
  densInfo= density(postVec)
  xMat = densInfo$x
  yMat = densInfo$y
  
  maxPostDens = max(densInfo$y)
  
  maxX = max(densInfo$x)
  minX = min(densInfo$x)
  maxMax = max(c(abs(minX),abs(maxX)))
  
  densDF = data.frame(c(xMat),c(yMat))
  colnames(densDF) = c('x','y')
  
  hdi1y = densDF$y[which(abs(densDF$x-hdi[1])==min(abs(densDF$x-hdi[1])))]
  hdi2y = densDF$y[which(abs(densDF$x-hdi[2])==min(abs(densDF$x-hdi[2])))]
  
  #####################################################
  
  require(ggplot2)
  mainPlot = ggplot(densDF, aes(x = x, y = y)) +
    geom_line() + 
    geom_ribbon(data = subset(densDF, hdi[1] <= x & x <= hdi[2]), 
                aes(ymin=0, ymax=y),
                alpha=0.5) +
    geom_segment(aes(x=hdi[1],
                     y=0,
                     xend=hdi[1],
                     yend=hdi1y),
                 linetype='21212121',
                 size = 0.2) +
    geom_segment(aes(x=hdi[2],
                     y=0,
                     xend=hdi[2],
                     yend=hdi2y),
                 linetype='21212121',
                 size = 0.2) +
    geom_segment(aes(x=hdi[1],
                     y=0,
                     xend=hdi[2],
                     yend=0),
                 linetype='21212121',
                 size = 0.2) +
    geom_segment(aes(x=0,
                     y=0,
                     xend=0,
                     yend=maxPostDens),
                 color='red',
                 linetype='dashed') +
    xlab(vecName) +
    xlim(-maxMax,maxMax) + 
    theme_Publication(base_size = bSize)
  
  #####################################################
  
  sPointsIdx = length(par1PostVec) - numSPoints
  
  par1ShortVec = par1PostVec[sPointsIdx:length(par1PostVec)]
  par2ShortVec = par2PostVec[sPointsIdx:length(par2PostVec)]
  parDF = data.frame(par1ShortVec,par2ShortVec)
  colnames(parDF) = c('par1','par2')
  
  temp = cbind(par1PostVec,par2PostVec)
  subMax = round(max(temp) + 0.2*max(temp),1)
  subMin = round(min(temp) - 0.2*min(temp),1)
  
  subPlot = ggplotGrob(ggplot(parDF ,aes(par1,par2)) +
                         geom_point(size = 0.1, stroke=0.4, shape=16) +
                         geom_abline(intercept=0, linetype='dashed', size = 0.1) +
                         xlim(subMin, subMax) +
                         ylim(subMin, subMax) +
                         labs(x='PMd-SIL', y='Vertex-SIL') +
                         theme_Small())
  
  #####################################################
  
  require(cowplot)
  insetPlot = ggdraw() +
    draw_plot(mainPlot,0,0,1,1) +
    draw_plot(subPlot, 0.675, 0.675, 0.3, 0.3)
  
  #####################################################
  require(grid)
  require(gridExtra)
  if (plotLabel=='A'){
    
    label = textGrob(plotLabel, gp=gpar(fontface='bold', fontsize=7))
    yLabel = textGrob('Probability density', gp=gpar(fontface='bold', fontsize=5), rot=90, hjust = 0.3)
    layMat = rbind(c(1,NA),
                   c(2,3))
    cWidths = c(0.0625,1)
    rHeights = c(0.0625,1)
    finalPlot = grid.arrange(label,yLabel,insetPlot,layout_matrix=layMat, widths=cWidths, heights=rHeights)
    
  } else {
    
    label = textGrob(plotLabel, gp=gpar(fontface='bold', fontsize=7))
    layMat = rbind(c(1,NA),
                   c(NA,2))
    cWidths = c(0.0625,1)
    rHeights = c(0.0625,1)
    finalPlot = grid.arrange(label,insetPlot,layout_matrix=layMat, widths=cWidths, heights=rHeights)
    
  }
  
  return(finalPlot)
}







