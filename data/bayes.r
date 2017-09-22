#DRI error rate bayesion analysis
#first with noncommital priors
#then with priors informed by the fact that
#subjs perform close to ceiling

library(rjags)


#set wd to data dir
setwd('/media/storage/testing_ground/R/RR_TMS_data')

#win path
setwd('C:\\Users\\ausma_000\\Desktop')

#minilith path
setwd('/home/patrick/Dropbox/Documents/Projects/RuleRep/RuleRep_TMS/data/')

#read data file
data = read.table('table4R.txt', header=T, sep=",")

#Convert some values to make it more readable
data$infIns[data$infIns == 0] = "Inferred"
data$infIns[data$infIns == 1] = "Instructed"

data$SF[data$SF == 0] = "Symbol"
data$SF[data$SF == 1] = "Finger"

data$PV[data$PV == 0] = "PMd" 
data$PV[data$PV == 1] = "Vertex"

data$ELN[data$ELN == 0] = "Early" 
data$ELN[data$ELN == 1] = "Late"
data$ELN[data$ELN == 2] = "noStim"

#convert subject IDs to factor
data$subjID = as.factor(data$subjID)
# data$SF = as.factor(data$SF)
# data$PV = as.factor(data$PV)
# data$ELN = as.factor(data$ELN)
# data$infIns = as.factor(data$infIns)

#replace "NaN"s with NA
data[data == 'NaN'] = NA

#add condition index column to data frame
data$condIdx = interaction(data$SF,data$PV,data$ELN,data$infIns)

#remove subj 2406 first block (NaNed out) and other random trials where subj didn't respond
data = subset(data, !is.na(data$ruleRT) & !is.na(data$PV) & !is.na(data$stimRT))

###########################################################################################################################

#let's just do S/F x Inf/Ins late stim trials, because that's where most of the effect lies

#bayes data
bd = aggregate(data$success,
               by=list(SF=data$SF,
                       PV=data$PV,
                       ELN=data$ELN,
                       infIns=data$infIns),
               FUN=function(x) c(success = sum(x), n = length(x)))
bd$prct = bd$x[,1]/bd$x[,2]

#this essentially assumes that there was one subject per condition
#need to ask John about how to handle within subjects
bd$condNumber = c(1:24)

#make the data list for the model file
dataList = list(
  z = bd$x[,1],
  N = bd$x[,2],
  p = bd$condNumber,
  Nsubj = 24, #truly 8, but see above
  Ncat = 24
  )

#use the model file from the psych 548 midterm
#in this model, theta describes the probability that a given subject
#will get a random trial in a given condition correct
#omega[mm] and kappa[mm] describe the beta distribution (through [A,B])
#from which thetas are drawn across subjects
#omega.0 and kappa.0 describe the beta distribution
#from which omega[mm]s are drawn across conditions
#kappa.0 and kappa[mm] are described by noncommittal gamma dists
modelString = "
# Model file for Target Identification problem on Psych 548 Midterm.
# -----------------------------

model {

# Hyperpriors for omega0 and kappa0, the parameters of 
# the hyperdistribution from which omega[cc] and kappa[cc] 
# are sampled.  

omega.0 ~ dbeta( 1.0, 1.0 ) 

kappaMinusTwo.0 ~ dgamma( 0.01, 0.01 )

kappa.0 <- kappaMinusTwo.0 + 2

# Compute the A0 and B0 parameters of the hyper beta distribution
A0 <-  omega.0*(kappa.0 - 2) + 1
B0 <-  (1 - omega.0)*(kappa.0 - 2) + 1

# Hyperprior for omega[cc] and kappa[cc], 
# the condition-specific omega and kappa
for ( mm in 1:Ncat ) {
omega[mm] ~ dbeta( A0, B0 )

# mean = 1, sd = 10 (generic vague)
kappaMinusTwo[mm] ~ dgamma( 0.01, 0.01 ) 

kappa[mm] <- kappaMinusTwo[mm] + 2
}

# Prior for theta[j] 
for ( j in 1:Nsubj ) {
aSubj[j] <- omega[p[j]]*(kappa[p[j]]-2)+1
bSubj[j] <- (1-omega[p[j]])*(kappa[p[j]]-2)+1

theta[j] ~ dbeta( aSubj[j], bSubj[j] ) 
}

# Likelihood function
for ( i in 1:Nsubj ) {
z[i] ~ dbin( theta[i], N[i] )
}

}  #close bracket for the model syntax

" # close quote for modelString

#make the model .txt
writeLines( modelString, con = "DRI_bayes_m1.txt" )

#init the mcmc chains
parameters  = c("theta","omega","kappa","omega.0","kappa.0") 

adaptSteps = 500         # Number of steps to adapt the samplers
burnInSteps = 500         # Number of steps to burn-in the chains
nChains = 2            # nChains should be 2 or more for diagnostics 

#the number of saved steps for each chain (i.e. 3 for the first, 2000 for the second)
numSavedSteps = c(3, 2000)

#thinning rate
thinSteps = 20

nIter = ceiling(numSavedSteps*thinSteps/nChains)

saveName = "DRI_bayes_m1-POST-"

#create, init, and adapt model:
jagsModel = jags.model( "DRI_bayes_m1.txt", 
                        data     = dataList,
                        n.chains = nChains, 
                        n.adapt  = adaptSteps)

#burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel, n.iter = burnInSteps )

# #sample the posterior
# mcmc.out = list()
# for (i in 1:2) {
#   cat( "Sampling final MCMC chains for i =", i, 
#        ", and n.iter =", nIter[i], ".\n\n" )
#   
#   mcmc.out[i] = coda.samples(jagsModel, 
#                               variable.names = parameters, 
#                               n.iter = nIter[i], 
#                               thin = thinSteps)
# } #figure out the warning this throws

mcmc.out = coda.samples(jagsModel, 
                           variable.names = parameters, 
                           n.iter = nIter[2], 
                           thin = thinSteps)

mcmc.mat = as.matrix(mcmc.out)
mcmc.df = as.data.frame(mcmc.mat)


source("/home/patrick/Dropbox/Documents/2017/Winter Quarter/Psych 548/Materials/Week7/Wed/kruschke_r/DBDA2E-utilities.R")
attach( "/home/patrick/Dropbox/Documents/2017/Winter Quarter/Psych 548/Materials/Week7/Wed/jmfuns.rda", pos = 2)


res.param(samples=mcmc.df$`omega[1]`,plot.dist=TRUE,show.conf=TRUE)
plotPost(mcmc.df$`omega[1]`, credMass=0.95)

###########################################################################################################################

#another way that may account for within subjects
bd1 = data.frame(data$subjID,data$SF,data$infIns,data$success)
colnames(bd1) = c('subjID','SF','infIns','success')

bd1.sum = aggregate(bd1$success,by=list(subjID=bd1$subjID,
                                        SF=data$SF,
                                        PV=data$PV,
                                        ELN=data$ELN,
                                        infIns=data$infIns),
                    FUN=function(x) c(success = sum(x), n = length(x)))
bd1.sum$prct = bd1.sum$x[,1]/bd1.sum$x[,2]

#this essentially says that every subject participated in every condition
#may account for within subjs?
bd1.sum$condNumber = rep(c(1:8),times=nrow(bd1.sum)/length(unique(bd1.sum$subjID)))

#make the data list for the model file
dataList = list(
  z = bd1.sum$x[,1],
  N = bd1.sum$x[,2],
  p = bd1.sum$condNumber,
  Nsubj = 8, #truly 8, but see above
  Ncat = 24
)

#init the mcmc chains
parameters  = c("theta","omega","kappa","omega.0","kappa.0") 

adaptSteps = 500         # Number of steps to adapt the samplers
burnInSteps = 500         # Number of steps to burn-in the chains
nChains = 2            # nChains should be 2 or more for diagnostics 

#the number of saved steps for each chain (i.e. 3 for the first, 2000 for the second)
numSavedSteps = c(3, 2000)

#thinning rate
thinSteps = 20

nIter = ceiling(numSavedSteps*thinSteps/nChains)

saveName = "DRI_bayes_m1-POST-"

#create, init, and adapt model:
jagsModel = jags.model( "DRI_bayes_m1.txt", 
                        data     = dataList,
                        n.chains = nChains, 
                        n.adapt  = adaptSteps)

#burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel, n.iter = burnInSteps )

#sample the posterior
mcmc.out = list()
for (i in 1:2) {
  cat( "Sampling final MCMC chains for i =", i, 
       ", and n.iter =", nIter[i], ".\n\n" )
  
  mcmc.out[i] = coda.samples(jagsModel, 
                             variable.names = parameters, 
                             n.iter = nIter[i], 
                             thin = thinSteps)
}






