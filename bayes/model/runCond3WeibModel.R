
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

dataList = list(y=y,
                cond=conds,
                minRT = minRTs,
                Ncond = numConds,
                Ntotal = numTrials)

setwd('/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/model/')

#parameters for JAGS to monitor
parameters = c('shape','scale','shift','nu1','nu2','eta1','eta2')


initsList = list(
  list(shape = runif(numPars, min=0.9, max=2),
       scale = runif(numPars, min=0.3, max=4),
       shift = runif(numPars, min=0, max=0.4),
       nu1 = 1.875,
       nu2 = 1.25,
       eta1 = 8,
       eta2 = 8),
  list(shape = runif(numPars, min=0.9, max=2),
       scale = runif(numPars, min=0.3, max=4),
       shift = runif(numPars, min=0, max=0.4),
       nu1 = 1.6, #1.25
       nu2 = 1, #0.5
       eta1 = 5, #5
       eta2 = 5), #5
  list(shape = runif(numPars, min=0.9, max=2),
       scale = runif(numPars, min=0.3, max=4),
       shift = runif(numPars, min=0, max=0.4),
       nu1 = 2.5,
       nu2 = 2,
       eta1 = 15,
       eta2 = 15))

require(rjags)
require(runjags)

adaptSteps = 10000
burnInSteps = 50000
nChains = 3
numSavedSteps = 1000000
thinSteps = 5
nIter = ceiling((numSavedSteps*thinSteps)/nChains)

#using runjags
runJagsOut = run.jags(method ="parallel",
                      model = "cond3WeibModel.txt",
                      monitor = parameters,
                      data = dataList,
                      inits = initsList,
                      n.chains = nChains,
                      adapt = adaptSteps,
                      burnin = burnInSteps,
                      sample = nIter,
                      thin  = thinSteps,
                      summarise = FALSE,
                      plots = FALSE)

codaSamples = as.mcmc.list(runJagsOut)

MCMCpars = list(adaptSteps = adaptSteps, burnInSteps = burnInSteps, nChains = nChains, numSavedSteps = numSavedSteps, thinSteps=thinSteps, nIter=nIter)

folder = '/projects/DRI/DRI_bayes/dweibullSims/models/cond3WeibModel/results/codaSamples/'
fname = 'cond3WeibModel_codaSamples.RData'
save(codaSamples,initsList,MCMCpars,file = paste(folder,fname,sep=''))




