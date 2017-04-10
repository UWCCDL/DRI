library(ggplot2)
library(grid)
library(gridExtra)
# library(GGally)

setwd('/home/patrick/Desktop')

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


dT1 = subset(correct, correct$SF=='Symbol' & correct$ELN=='Early')
dTest1.1 = aggregate(dT1[c('stimRT')], by=list(subjID=dT1$subjID,
                                               PV = dT1$PV,
                                               infIns = dT1$infIns),
                     FUN=mean)

dT2 = subset(correct, correct$SF=='Finger' & correct$ELN=='Early')
dTest2.1 = aggregate(dT2[c('stimRT')], by=list(subjID=dT2$subjID,
                                               PV = dT2$PV,
                                               infIns = dT2$infIns),
                     FUN=mean)
dT3 = subset(correct, correct$SF=='Symbol' & correct$ELN=='Late')
dTest3.1 = aggregate(dT3[c('stimRT')], by=list(subjID=dT3$subjID,
                                               PV = dT3$PV,
                                               infIns = dT3$infIns),
                     FUN=mean)

dT4 = subset(correct, correct$SF=='Finger' & correct$ELN=='Late')
dTest4.1 = aggregate(dT4[c('stimRT')], by=list(subjID=dT4$subjID,
                                               PV = dT4$PV,
                                               infIns = dT4$infIns),
                     FUN=mean)




#Symbol condition, early stim trials barplot
dT1.bp = aggregate(dTest1.1[c('stimRT')], by=list(PV=dTest1.1$PV,
                                                  infIns = dTest1.1$infIns),
                   FUN=function(x) c(mn = mean(x), sem = (sd(x)/sqrt(length(unique(dTest1.1$subjID))))))


dT1bplot = ggplot(dT1.bp, aes(fill=infIns,y=stimRT[,1],x=PV))

#make p1 plot and grab the legend - test to see how little of the plot you have to make before you lose legend
p1 = (dT1bplot+geom_bar(position=position_dodge(),stat='identity')
      +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
      +theme(text=element_text(size=15,
                               face='bold')))

#gets just the legend for a given plot, in this case p1
tmp = ggplot_gtable(ggplot_build(p1)) 
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
legend = tmp$grobs[[leg]] 

#clear p1 and remake without legend
rm(p1)
p1 = (dT1bplot+geom_bar(position=position_dodge(),stat='identity')
      +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
      +theme(legend.position='none',
             text=element_text(size=15,
                               face='bold'))
      +ylab('Early')
      +xlab('Symbol')
      +ylim(0,1.5))


#Finger condition, early stim trials barplot
dT2.bp = aggregate(dTest2.1[c('stimRT')], by=list(PV=dTest2.1$PV,
                                                  infIns = dTest2.1$infIns),
                   FUN=function(x) c(mn = mean(x), sem = (sd(x)/sqrt(length(unique(dTest2.1$subjID))))))


dT2bplot = ggplot(dT2.bp, aes(fill=infIns,y=stimRT[,1],x=PV))
p2 = (dT2bplot+geom_bar(position=position_dodge(),stat='identity')
      +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
      +theme(legend.position='none',
             text=element_text(size=15,
                               face='bold'))
      +ylab('Early')
      +xlab('Finger')
      +ylim(0,1.5))

#Symbol condition, late stim trials barplot
dT3.bp = aggregate(dTest3.1[c('stimRT')], by=list(PV=dTest3.1$PV,
                                                  infIns = dTest3.1$infIns),
                   FUN=function(x) c(mn = mean(x), sem = (sd(x)/sqrt(length(unique(dTest3.1$subjID))))))


dT3bplot = ggplot(dT3.bp, aes(fill=infIns,y=stimRT[,1],x=PV))
p3 = (dT3bplot+geom_bar(position=position_dodge(),stat='identity')
      +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
      +theme(legend.position='none',
             text=element_text(size=15,
                               face='bold'))
      +ylab('Late')
      +xlab('Symbol')
      +ylim(0,1.5)
      +annotate('text',
                label='***',
                fontface='bold',
                size=7,
                x='PMd',
                y=1.4))

#Finger condition, late stim trials barplot
dT4.bp = aggregate(dTest4.1[c('stimRT')], by=list(PV=dTest4.1$PV,
                                                  infIns = dTest4.1$infIns),
                   FUN=function(x) c(mn = mean(x), sem = (sd(x)/sqrt(length(unique(dTest4.1$subjID))))))


dT4bplot = ggplot(dT4.bp, aes(fill=infIns,y=stimRT[,1],x=PV))
p4 = (dT4bplot+geom_bar(position=position_dodge(),stat='identity')
      +geom_errorbar(aes(ymax = stimRT[,1]+stimRT[,2], ymin=stimRT[,1]-stimRT[,2]), position=position_dodge(.9), width=0.25)
      +theme(legend.position='none',
             text=element_text(size=15,
                               face='bold'))
      +ylab('Late')
      +xlab('Finger')
      +ylim(0,1.5))

bplots = list(p1,p2,p3,p4,legend)
bplots = list(p1,p2,p3,p4)

do.call("grid.arrange", c(bplots,
                          ncol=2,
                          top="Effect of PMd rTMS on execution response time",
                          left='Timing of stimulation (s)',
                          bottom='Rule type'))


title = textGrob("Effect of PMd rTMS on execution response time",
                 gp=gpar(fontface='bold',
                         fontsize=25))
big.ylab = textGrob('Timing of stimulation (s)',
                    gp=gpar(fontface='bold',
                            fontsize=20),
                    rot=90)
big.xlab = textGrob('Rule type',
                    gp=gpar(fontface='bold',
                            fontsize=20))

grid.arrange(p1,p2,p3,p4,
             ncol=2,
             #top=title,
             left=big.ylab,
             bottom=big.xlab)

# #doesn't work
# grid.arrange(bplots,
#              ncol=2,
#              top="Effect of PMd rTMS on execution response time",
#              left='Timing of stimulation',
#              bottom='Rule type')

