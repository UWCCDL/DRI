#script to perform DRI ANOVAs and create plots

#load appropriate libraries
library(ggplot2)
library(gridExtra)
library(GGally)

#set wd to data dir
setwd('/media/storage/testing_ground/R/RR_TMS_data')

#read data file
data = read.table('table4R.txt', header=T, sep=",")

###########################################################################################################################

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

#replace "NaN"s with NA
data[data == 'NaN'] = NA

#add condition index column to data frame
data$condIdx = interaction(data$SF,data$PV,data$ELN,data$infIns)

###########################################################################################################################

#remove subj 2406 first block (NaNed out) and other random trials where subj didn't respond
data = subset(data, data$PV!="NaN" & data$ruleRT!="NaN" & data$stimRT!="NaN")

#get subset of only correct trials
correct = subset(data, data$success==1)

#get subset of only error trials
incorrect = subset(data, data$success!=1)

###########################################################################################################################

#aggregate success rate data
success = aggregate(data[c("success")], list(infIns=data$infIns,
                                             SF=data$SF,
                                             PV=data$PV,
                                             ELN=data$ELN,
                                             subjID=data$subjID),
                    mean)

#aggregate ruleRT data
ruleRT = aggregate(data[c("ruleRT")], list(infIns=data$infIns,
                                             SF=data$SF,
                                             PV=data$PV,
                                             ELN=data$ELN,
                                             subjID=data$subjID),
                    mean)

#aggregate stimRT data
stimRT = aggregate(data[c("stimRT")], list(infIns=data$infIns,
                                             SF=data$SF,
                                             PV=data$PV,
                                             ELN=data$ELN,
                                             subjID=data$subjID),
                    mean)

###########################################################################################################################


#scatterplots - color by trial cond - successes and errors
  #global ruleRT:stimRT for all conditions
  #per subject ruleRT:stimRT for all conditions
  #same as the two above, for single conditions and joint conditions

#global ruleRT:stimRT
#create the plot, considering only correct trials, with stimRT on xaxis and ruleRT on yaxis
sp1 = ggplot(correct, aes(x=stimRT, y=ruleRT))
#color the points by subjID and print the plot
(sp1+geom_point(aes(color=subjID))
    +labs(title='The relationship between ruleRT and stimRT, correct trials only',x = 'stimRT (s)', y = 'ruleRT (s)')
    +scale_x_continuous(breaks = round(seq(0,round(max(data$stimRT))+1,by = 0.1),1))
    +scale_y_continuous(breaks = round(seq(0,round(max(data$ruleRT))+1,by = 0.1),1)))

#create the plot, considering only error trials, with stimRT on xaxis and ruleRT on yaxis
sp2 = ggplot(incorrect, aes(x=stimRT, y=ruleRT))
#color the points by subjID and print the plot
(sp2+geom_point(aes(color=subjID))
  +labs(title='The relationship between ruleRT and stimRT, incorrect trials only',x = 'stimRT (s)', y = 'ruleRT (s)')
  +scale_x_continuous(breaks = round(seq(0,round(max(data$stimRT))+1,by = 0.1),1))
  +scale_y_continuous(breaks = round(seq(0,round(max(data$ruleRT))+1,by = 0.1),1)))

#per subject ruleRT:stimRT, correct trials - color by trial condition
#currently plots all subjects

#get unique subjIDs
subjIDs = as.numeric(levels(data$subjID))
#make empty list to put plots into
subjsPlots = list()
for (i in 1:length(subjIDs)){
  
  #get subject subsets
  subjSubset = subset(correct, correct$subjID==subjIDs[i])
  #make plots
  subjsPlots[[i]] = (ggplot(subjSubset, aes(x=stimRT, y=ruleRT))
                    +geom_point(aes(color=condIdx))
                    +theme(legend.position='none')
                    +scale_x_continuous(breaks = seq(0,5,by = 0.2))
                    +scale_y_continuous(breaks = seq(0,5,by = 0.2))
                    +labs(title=paste('Subject',toString(subjIDs[i])),x = 'stimRT (s)', y = 'ruleRT (s)'))
  

}

#plot all 8
do.call("grid.arrange", c(subjsPlots,ncol=2))

#global scatterplot for the different conditions
conds = levels(correct$condIdx)
condsPlots = list()

for (i in 1:length(levels(correct$condIdx))){
  
  #get cond subset
  condSubset = subset(correct, correct$condIdx==conds[i])
  #make plots
  condsPlots[[i]] = (ggplot(condSubset, aes(x=stimRT, y=ruleRT))
                    +geom_point(aes(color=subjID))
                    +theme(legend.position='none')
                    +scale_x_continuous(breaks = seq(0,5,by = 0.2))
                    +scale_y_continuous(breaks = seq(0,5,by = 0.2))
                    +labs(title=paste('Condition',conds[i]),x = 'stimRT (s)', y = 'ruleRT (s)'))
  
}

# plot all
do.call("grid.arrange", c(condsPlots,ncol=4))



###########################################################################################################################

#histogram/density plots

#global ruleRT hist w/ density
hp1 = ggplot(correct, aes(x=ruleRT))
(hp1+geom_histogram(binwidth = 0.1, aes(y = ..density..))
    +geom_density()
    +labs(title='ruleRTs',x = 'ruleRT (s)'))

#global stimRT hist w/ density
hp1 = ggplot(correct, aes(x=stimRT))
(hp1+geom_histogram(binwidth = 0.1, aes(y = ..density..))
  +geom_density()
  +labs(title='stimRTs',x = 'stimRT (s)'))


#per subject stimRT hists w/ densities
subjhPlots = list()
for (i in 1:length(subjIDs)){
  
  #get subject subsets
  subjSubset = subset(correct, correct$subjID==subjIDs[i])
  #make plots
  subjhPlots[[i]] = (ggplot(subjSubset, aes(x=stimRT))
                     +geom_histogram(binwidth = 0.1, aes(y = ..density..))
                     +geom_density()
                     +labs(title=paste('Subject',toString(subjIDs[i])),x = 'stimRT (s)'))
  
  
}

do.call("grid.arrange", c(subjhPlots,ncol=2))

#plot all subject stimRT density estimates on top of one another
ggplot(correct, aes(stimRT, fill = subjID)) +geom_density(alpha = 0.2)

#plot all condition stimRT density estimates on top of one another
ggplot(correct, aes(stimRT, fill = condIdx)) +geom_density(alpha = 0.2)

###########################################################################################################################
#condition mean bar graphs
#bar means with bootstrapped CIs







###########################################################################################################################

#Controls tests
#ns v. Vs
#for stimRTs:
#1. compare Vertex stimulation trials against PMd block no stimulation trials (to show Vstim is the same as not being stimulated over PMd)
#2. PMd early stimulation trials against




#StimRT
#2way ANOVAs, drop nostim conds, 16 cond combos, 12 total 2ways
#3ways (4 of them) 
#4way


#RuleRT do the same as stimRT