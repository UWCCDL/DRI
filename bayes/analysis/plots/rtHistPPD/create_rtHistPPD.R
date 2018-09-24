library(coda)
library(dplyr)

######################################################################################################################################
#load HBM results

load('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/results/codaSamples/cond3WeibModel_codaSamples.RData')
codaDF = as.data.frame(as.matrix(codaSamples))

######################################################################################################################################
#load behavioral data

setwd('/projects/DRI/DRI_bayes/data')

DRI.data = read.csv('DRI_bayes.csv', sep = ',', header = TRUE)

DRI.data$subj_idx = as.factor(DRI.data$subj_idx)

DRI.data$cond = interaction(DRI.data$SF, DRI.data$PV, DRI.data$ELN, DRI.data$infIns)

DRI.data = subset(DRI.data, DRI.data$response==1)

y = DRI.data$rt
numTrials = nrow(DRI.data)
numConds = length(unique(DRI.data$cond))
conds = as.numeric(DRI.data$cond)

numPars = length(unique(conds))

minimumRTs = aggregate(DRI.data$rt, by=list(DRI.data$cond), FUN=min)
colnames(minimumRTs) = c('cond_idx','minRT')
minRTs = minimumRTs$minRT

######################################################################################################################################
#create condMap/List
condMap = data.frame(levels(DRI.data$cond),seq(1,24,length.out=24))
colnames(condMap) = c('condName','numericEquiv')

condList = levels(DRI.data$cond)

######################################################################################################################################

source('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/analysis/analysisFuncs.R')
source('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/analysis/plots/rtHistPPD/rtHistPPD.R')

#use minRT of the condition as the start of the bins for chiSq test (otherwise larger shift value will cause cumWeib to return NA)
condRTHist_plusPPDTESTING(codaDF, DRI.data, 6, bins = seq(minRTs[6],3.5,0.25), 'PMd-SIL', fname = '/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/results/rtHistPPD/rtHistPPD_PMd.png')

condRTHist_plusPPDTESTING(codaDF, DRI.data, 8, bins = seq(minRTs[8],3.5,0.25), 'Vertex-SIL', fname = '/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/results/rtHistPPD/rtHistPPD_Ver.png')


