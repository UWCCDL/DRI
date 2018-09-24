

library(coda)
source('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/analysis/analysisFuncs.R')
source('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/analysis/convergenceFuncs.R')

load('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/results/codaSamples/cond3WeibModel_codaSamples.RData')

convDiagFolder = '/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/results/convDiag/'

convergenceDiag(codaSamples, parName = 'all', saveFolder=convDiagFolder)
