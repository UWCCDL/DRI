#script to perform DRI ANOVAs and create plots

#load appropriate libraries
library(ggplot2)
library(grid)
# library(gridExtra)
# library(GGally)

#set wd to data dir
setwd('/media/storage/testing_ground/R/RR_TMS_data')

#win path
setwd('C:\\Users\\ausma_000\\Desktop')

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
# data$SF = as.factor(data$SF)
# data$PV = as.factor(data$PV)
# data$ELN = as.factor(data$ELN)
# data$infIns = as.factor(data$infIns)

#replace "NaN"s with NA
data[data == 'NaN'] = NA

#add condition index column to data frame
data$condIdx = interaction(data$SF,data$PV,data$ELN,data$infIns)

###########################################################################################################################

#remove subj 2406 first block (NaNed out) and other random trials where subj didn't respond
data = subset(data, !is.na(data$ruleRT) & !is.na(data$PV) & !is.na(data$stimRT))

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
                    +labs(title=paste('Subject',toString(c('stimRT'),subjIDs[i])),x = 'stimRT (s)', y = 'ruleRT (s)'))
  

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

dCT1 = subset(correct, correct$PV=='Vertex' | (correct$PV=='PMd' & correct$ELN=='noStim'))

dCT1$ELN[dCT1$ELN == 'Early'] = "Stim"
dCT1$ELN[dCT1$ELN == 'Late'] = "Stim"

dControlTest1.1 = aggregate(dCT1[c('stimRT')], by=list(subjID = dCT1$subjID,
                                                       nsVs = dCT1$ELN),
                            FUN=mean)
summary(aov(stimRT ~ (nsVs) + Error(subjID), dControlTest1.1))




#for stimRTs:
#1. compare Vertex stimulation trials against PMd block no stimulation trials (to show Vstim is the same as not being stimulated over PMd)
#2. PMd early stimulation trials against

#get subset of all vertex trials and PMd nostim trials 
dCT2 = subset(correct, correct$PV=='Vertex' | (correct$PV=='PMd' & correct$ELN=='noStim'))

#get the means of the different conditions
dControlTest2.1 = aggregate(dCT2[c('stimRT')], by=list(subjID = dCT2$subjID, 
                                                       SF = dCT2$SF, 
                                                       PV = dCT2$PV, 
                                                       ELN = dCT2$ELN, 
                                                       infIns = dCT2$infIns), 
                            FUN=mean)

#run the ANOVA
#removed ELN from the error term here because I was getting a warning that the "Error() model is singular" - 
#ELN only has one level for PMd trials in this case, while it has 3 for Vertex trials
summary(aov(stimRT ~ (ELN * SF * PV * infIns) + Error(subjID/(SF * PV * infIns)), dControlTest2.1))

#this one comes out "looking better"
summary(aov(stimRT ~ (ELN * SF * PV * infIns) + Error(subjID), dControlTest2.1))

#try collapsing across SF, infIns just to see
dControlTest2.2 = aggregate(dCT2[c('stimRT')], by=list(subjID = dCT2$subjID, 
                                                       PV = dCT2$PV, 
                                                       ELN = dCT2$ELN), 
                            FUN=mean)
#no interaction because ELN is only one level for PMd in this case
summary(aov(stimRT ~ (PV * ELN) + Error(subjID/PV), dControlTest2.2))

summary(aov(stimRT ~ (PV * ELN) + Error(subjID), dControlTest2.2))

#collapse across ELN too for funsicals
dControlTest2.3 = aggregate(dCT2[c('stimRT')], by=list(subjID = dCT2$subjID, 
                                                       PV = dCT2$PV), 
                            FUN=mean)

summary(aov(stimRT ~ (PV) + Error(subjID/PV), dControlTest2.3))

#the same as the one above
summary(aov(stimRT ~ (PV) + Error(subjID), dControlTest2.3))





#StimRT
#2way ANOVAs, drop nostim conds, 6 total 2ways
#SF/PV/EL/InfIns

#***NEED TO GET SUBSET WITH ONLY E AND L TRIALS***
d2.3ways = subset(correct, correct$ELN=='Early' | correct$ELN=='Late')
#1.SF/PV
d2ways.1 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    SF = d2.3ways$SF,
                                                    PV = d2.3ways$PV),
                      FUN=mean)
summary(aov(stimRT ~ (SF*PV)+Error(subjID/(SF*PV)),d2ways.1))
summary(aov(stimRT ~ (SF*PV)+Error(subjID),d2ways.1))

#2.SF/EL
d2ways.2 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                   SF = d2.3ways$SF,
                                                   EL = d2.3ways$ELN),
                     FUN=mean)
summary(aov(stimRT ~ (SF*EL)+Error(subjID/(SF*EL)),d2ways.2))
summary(aov(stimRT ~ (SF*EL)+Error(subjID),d2ways.2))

#3.SF/InfIns
d2ways.3 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    SF = d2.3ways$SF,
                                                    infIns = d2.3ways$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (SF*infIns)+Error(subjID/(SF*infIns)),d2ways.3))
summary(aov(stimRT ~ (SF*infIns)+Error(subjID),d2ways.3))

#4.PV/EL
d2ways.4 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    PV = d2.3ways$PV,
                                                    EL = d2.3ways$ELN),
                     FUN=mean)
summary(aov(stimRT ~ (PV*EL)+Error(subjID/(PV*EL)),d2ways.4))
summary(aov(stimRT ~ (PV*EL)+Error(subjID),d2ways.4))

#5.PV/InfIns
d2ways.5 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    PV = d2.3ways$PV,
                                                    infIns = d2.3ways$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (PV*infIns)+Error(subjID/(PV*infIns)),d2ways.5))
summary(aov(stimRT ~ (PV*infIns)+Error(subjID),d2ways.5))

#6.EL/InfIns
d2ways.6 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    EL = d2.3ways$ELN,
                                                    infIns = d2.3ways$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (EL*infIns)+Error(subjID/(EL*infIns)),d2ways.6))
summary(aov(stimRT ~ (EL*infIns)+Error(subjID),d2ways.6))


#3ways (4 of them)
#SF/PV/EL/InfIns
#SF/PV/EL
d3ways.1 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    SF = d2.3ways$SF,
                                                    PV = d2.3ways$PV,
                                                    EL = d2.3ways$ELN),
                     FUN=mean)
summary(aov(stimRT ~ (SF*PV*EL)+Error(subjID/(SF*PV*EL)),d3ways.1))
summary(aov(stimRT ~ (SF*PV*EL)+Error(subjID),d3ways.1))

#SF/PV/InfIns
d3ways.2 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    SF = d2.3ways$SF,
                                                    PV = d2.3ways$PV,
                                                    infIns = d2.3ways$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (SF*PV*infIns)+Error(subjID/(SF*PV*infIns)),d3ways.2))
summary(aov(stimRT ~ (SF*PV*infIns)+Error(subjID),d3ways.2))

#SF/EL/InfIns
d3ways.3 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    SF = d2.3ways$SF,
                                                    EL = d2.3ways$ELN,
                                                    infIns = d2.3ways$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (SF*EL*infIns)+Error(subjID/(SF*EL*infIns)),d3ways.3))
summary(aov(stimRT ~ (SF*EL*infIns)+Error(subjID),d3ways.3))


#PV/EL/InfIns
d3ways.4 = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                    PV = d2.3ways$PV,
                                                    EL = d2.3ways$ELN,
                                                    infIns = d2.3ways$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (PV*EL*infIns)+Error(subjID/(PV*EL*infIns)),d3ways.4))
summary(aov(stimRT ~ (PV*EL*infIns)+Error(subjID),d3ways.4))

#4way
#SF/PV/EL/InfIns
d4way = aggregate(d2.3ways[c('stimRT')], by=list(subjID=d2.3ways$subjID,
                                                 SF = d2.3ways$SF,
                                                 PV = d2.3ways$PV,
                                                 EL = d2.3ways$ELN,
                                                 infIns = d2.3ways$infIns),
                  FUN=mean)
summary(aov(stimRT ~ (SF*PV*EL*infIns)+Error(subjID/(SF*PV*EL*infIns)),d4way))
summary(aov(stimRT ~ (SF*PV*EL*infIns)+Error(subjID),d4way))



#RuleRT do the same as stimRT


###########################################################################################################################

#Andrea's note

#Symbol condition, early stim trials
dT1 = subset(correct, correct$SF=='Symbol' & correct$ELN=='Early')
dTest1.1 = aggregate(dT1[c('stimRT')], by=list(subjID=dT1$subjID,
                                               PV = dT1$PV,
                                               infIns = dT1$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (PV*infIns)+Error(subjID/(PV*infIns)),dTest1.1))
summary(aov(stimRT ~ (PV*infIns)+Error(subjID),dTest1.1))

#Finger condition, early stim trials
dT2 = subset(correct, correct$SF=='Finger' & correct$ELN=='Early')
dTest2.1 = aggregate(dT2[c('stimRT')], by=list(subjID=dT2$subjID,
                                               PV = dT2$PV,
                                               infIns = dT2$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (PV*infIns)+Error(subjID/(PV*infIns)),dTest2.1))
summary(aov(stimRT ~ (PV*infIns)+Error(subjID),dTest2.1))

#Symbol condition, late stim trials
dT3 = subset(correct, correct$SF=='Symbol' & correct$ELN=='Late')
dTest3.1 = aggregate(dT3[c('stimRT')], by=list(subjID=dT3$subjID,
                                               PV = dT3$PV,
                                               infIns = dT3$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (PV*infIns)+Error(subjID/(PV*infIns)),dTest3.1))
summary(aov(stimRT ~ (PV*infIns)+Error(subjID),dTest3.1))

#Finger condition, late stim trials
dT4 = subset(correct, correct$SF=='Finger' & correct$ELN=='Late')
dTest4.1 = aggregate(dT4[c('stimRT')], by=list(subjID=dT4$subjID,
                                               PV = dT4$PV,
                                               infIns = dT4$infIns),
                     FUN=mean)
summary(aov(stimRT ~ (PV*infIns)+Error(subjID/(PV*infIns)),dTest4.1))
summary(aov(stimRT ~ (PV*infIns)+Error(subjID),dTest4.1))


###########################################################################################################################

#barplots of "Andrea note" comparisons done above

#Symbol condition, early stim trials barplot
dT1.bp = aggregate(dTest1.1[c('stimRT')], by=list(PV=dTest1.1$PV,
                                                  infIns = dTest1.1$infIns),
                   FUN=function(x) c(mn = mean(x), sem = (sd(x)/sqrt(length(unique(dTest1.1$subjID))))))


dT1bplot = ggplot(dT1.bp, aes(fill=infIns,y=stimRT[,1],x=PV))

#make p1 plot and grab the legend - test to see how little of the plot you have to make before you lose legend
p1 = (dT1bplot+geom_bar(position=position_dodge(),stat='identity')
  +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25))
#gets just the legend for a given plot, in this case p1
tmp = ggplot_gtable(ggplot_build(p1)) 
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
legend = tmp$grobs[[leg]] 

#clear p1 and remake without legend
rm(p1)
p1 = (dT1bplot+geom_bar(position=position_dodge(),stat='identity')
      +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
      +theme(legend.position='none'))


#Finger condition, early stim trials barplot
dT2.bp = aggregate(dTest2.1[c('stimRT')], by=list(PV=dTest2.1$PV,
                                                  infIns = dTest2.1$infIns),
                   FUN=function(x) c(mn = mean(x), sem = (sd(x)/sqrt(length(unique(dTest2.1$subjID))))))


dT2bplot = ggplot(dT2.bp, aes(fill=infIns,y=stimRT[,1],x=PV))
p2 = (dT2bplot+geom_bar(position=position_dodge(),stat='identity')
  +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
  +theme(legend.position='none'))

#Symbol condition, late stim trials barplot
dT3.bp = aggregate(dTest3.1[c('stimRT')], by=list(PV=dTest3.1$PV,
                                                  infIns = dTest3.1$infIns),
                   FUN=function(x) c(mn = mean(x), sem = (sd(x)/sqrt(length(unique(dTest3.1$subjID))))))


dT3bplot = ggplot(dT3.bp, aes(fill=infIns,y=stimRT[,1],x=PV))
p3 = (dT3bplot+geom_bar(position=position_dodge(),stat='identity')
  +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
  +theme(legend.position='none'))

#Finger condition, late stim trials barplot
dT4.bp = aggregate(dTest4.1[c('stimRT')], by=list(PV=dTest4.1$PV,
                                                  infIns = dTest4.1$infIns),
                   FUN=function(x) c(mn = mean(x), sem = (sd(x)/sqrt(length(unique(dTest4.1$subjID))))))


dT4bplot = ggplot(dT4.bp, aes(fill=infIns,y=stimRT[,1],x=PV))
p4 = (dT4bplot+geom_bar(position=position_dodge(),stat='identity')
  +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
  +theme(legend.position='none'))

bplots = list(p1,p2,p3,p4,legend)
do.call("grid.arrange", c(bplots,ncol=2))

###########################################################################################################################

#try interaction line plot?
#symbol late
(ggplot(dT3.bp, aes(color=infIns,y=stimRT[,1],x=PV))
 +geom_point()+geom_line(aes(group=infIns)))

