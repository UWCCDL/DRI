#create the publication-quality shape, scale, shift comparison plots between PMd-SIL (6) and Vertex-SIL(8)


library(coda)
#load HBM results
load('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/results/codaSamples/cond3WeibModel_codaSamples.RData')
codaDF = as.data.frame(as.matrix(codaSamples))

source('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/analysis/analysisFuncs.R')
source('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/analysis/plots/parComp/parCompPlotFuncs.R')
#make individual subplots
p1 = plotPostPubQ(codaDF[,'shape[6]'], codaDF[,'shape[8]'], numSPoints = 10000, 'shape', 'A', 6)
p2 = plotPostPubQ(codaDF[,'scale[6]'], codaDF[,'scale[8]'], numSPoints = 10000, 'scale', 'B', 6)
p3 = plotPostPubQ(codaDF[,'shift[6]'], codaDF[,'shift[8]'], numSPoints = 10000, 'shift', 'C', 6)

#group subplots into larger plot, add title
groupedPlot = grid.arrange(p1,
                           p2,
                           p3,
                           ncol=3)

pWidth = 5 #grouped plot should be 5 inches wide
pHeight = pWidth/3 #3 subplots, so divide width by 3 to keep subplots square

ggsave('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/results/parComps/groupedTest_1200DPI_NEW_TEST.png', 
       plot=groupedPlot, device='png', width = pWidth, height = pHeight, units='in', dpi=1200)

