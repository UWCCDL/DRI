library(Rmisc)
library(matlab)
#library(pwr)
source("functions.R")
data <- read.table("andrea_data.csv", header=T, sep=",")

# We are interested only in Simon results
data$Instructed[data$Instructed == 0] <- "Inferred"
data$Instructed[data$Instructed == 1] <- "Instructed"

data$Rule[data$Rule == 0] <- "Symbolic"
data$Rule[data$Rule == 1] <- "Concrete"

data$Stimulation[data$Stimulation == 0] <- "Early"
data$Stimulation[data$Stimulation == 1] <- "Late"
data$Stimulation[data$Stimulation == 2] <- "NoStimulation"

data$Site[data$Site == 0] <- "PMd"
data$Site[data$Site == 1] <- "Vertex"

data$Subject <- as.factor(data$Subject)

data <- subset(data, data$Site != "NaN")
acc <- aggregate(data[c("Accuracy")], list(Instructed=data$Instructed, Rule=data$Rule, 
                                           Stimulation=data$Stimulation, Site=data$Site,
                                           Subject=data$Subject),
                 mean)
  

summary(aov(acc$Accuracy ~ (Instructed * Rule * Stimulation * Site) 
            + Error(Subject/(Instructed * Rule * Stimulation * Site)), 
            acc))

correct <- subset(data, data$Accuracy == 1 & !is.na(data$ResponseTime))

d <- aggregate(correct[c("EncodingTime", "ResponseTime")], list(Instructed=correct$Instructed, 
                                                                Rule=correct$Rule, 
                                                                Stimulation=correct$Stimulation, 
                                                                Site=correct$Site,
                                                                Subject=correct$Subject),
                 mean)


summary(aov(EncodingTime ~ (Instructed * Rule * Stimulation * Site) 
            + Error(Subject/(Instructed * Rule * Stimulation * Site)), 
            d))


summary(aov(ResponseTime ~ (Instructed * Rule * Stimulation * Site) 
            + Error(Subject/(Instructed * Rule * Stimulation * Site)), 
            d))

ds <- subset(d, d$Stimulation != "NoStimulation")

summary(aov(ResponseTime ~ (Instructed * Stimulation * Rule * Site) 
            + Error(Subject/(Instructed * Stimulation * Rule * Site)), 
            ds))

# Make sure the control conditions are equal

dv <- subset(d, d$Stimulation != "PMd")
summary(aov(ResponseTime ~ (Stimulation * Rule * Site * Instructed) + Error(Subject/(Stimulation * Rule * Site * Instructed)), dv))

pdf("InferredLateStim.pdf", width = 5, height = 5)

k1 <- subset(ds, ds$Stimulation == "Late" & ds$Instructed == "Inferred")
ms <- tapply(k1$ResponseTime, list(k1$Site, k1$Rule), mean)
ses <- tapply(k1$ResponseTime, list(k1$Site, k1$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
        ylim=c(0,2), main="Inferred Rules, Late Stimulation", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90, length=0.1)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90, length=0.1)
# Adds some numbers
text(x = xs, y = ms + ses + 0.05, labels = round(ms, 3))
box(bty="o")
dev.off()

tapply(k1$ResponseTime, list(k1$Site, k1$Rule), mean)

a1 <- aov(ResponseTime ~ (Rule * Site) + Error(Subject/(Rule * Site)), data=k1)
summary(a1 <- aov(ResponseTime ~ (Rule * Site) + Error(Subject/(Rule * Site)), data=k1)
)

#TukeyHSD(a1, "Site", ordered=T)

t.test(ResponseTime ~ Rule, subset(k1, k1$Site == "PMd"), paired=T)
t.test(ResponseTime ~ Site, subset(k1, k1$Rule == "Symbolic"), paired=T)
t1 <- subset(k1, k1$Site=="PMd" & k1$Rule=="Symbolic")
t2 <- subset(k1, k1$Site=="Vertex" & k1$Rule=="Concrete")
t.test(t1$ResponseTime, t2$ResponseTime, paired=T)

# Cohen's d

cd <- subset(k1, k1$Rule == "Symbolic")
ms <- tapply(cd$ResponseTime, cd$Site, mean)
sds <- tapply(cd$ResponseTime, cd$Site, sd)
m <- ms[1] - ms[2]
sdpooled <- sqrt(mean(sds**2))
m/sdpooled

# power
sub <- subset(k1, k1$Site == "PMd")
ms <- tapply(sub$ResponseTime, sub$Rule, mean)
d = abs(ms[1] - ms[2])/var(sub$ResponseTime)

pdf("InstructedLateStim.pdf", width = 5, height = 5)
k2 <- subset(ds, ds$Stimulation == "Late" & ds$Instructed == "Instructed")
ms <- tapply(k2$ResponseTime, list(k2$Site, k2$Rule), mean)
ses <- tapply(k2$ResponseTime, list(k2$Site, k2$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,2), main="Instructed Rules, Late Stimulation", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90, length=0.1)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90, length=0.1)
# Adds some numbers
text(x = xs, y = ms + ses + 0.05, labels = round(ms, 2))
box(bty="o")
dev.off()

# Only effect of rule
summary(aov(ResponseTime ~ (Rule * Site) + Error(Subject/(Rule * Site)), data=k2))
tapply(k2$ResponseTime, list(k2$Site, k2$Rule), mean)       

pdf("InferredEarlyStim.pdf", width = 5, height = 5)
k3 <- subset(ds, ds$Stimulation == "Early" & ds$Instructed == "Inferred")
ms <- tapply(k3$ResponseTime, list(k3$Site, k3$Rule), mean)
ses <- tapply(k3$ResponseTime, list(k3$Site, k3$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,1.75), main="Inferred Rules, Early Stimulation", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90, length=0.1)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90, length=0.1)

text(x = xs, y = ms + ses + 0.05, labels = round(ms, 2))
box(bty="o")
dev.off()

# only effect of rule
summary(aov(ResponseTime ~ (Rule * Site) + Error(Subject/(Rule * Site)), data=k3))

tapply(k3$ResponseTime, list(k3$Site, k3$Rule), mean)       


pdf("InstructedEarlyStim.pdf", width = 5, height = 5)
k4 <- subset(ds, ds$Stimulation == "Early" & ds$Instructed == "Instructed")

ms <- tapply(k4$ResponseTime, list(k4$Site, k4$Rule), mean)
ses <- tapply(k4$ResponseTime, list(k4$Site, k4$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,1.75), main="Instructed Rules, Early Stimulation", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90, length = 0.1)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90, length =0.1)

text(x = xs, y = ms + ses + 0.05, labels = round(ms, 2))
box(bty="o")
dev.off()


# only effect of rule
summary(aov(ResponseTime ~ (Rule * Site) + Error(Subject/(Rule * Site)), data=k4))


# Main effects of instructions

ms <- tapply(ds$ResponseTime, list(ds$Instructed, ds$Rule), mean)
ses <- tapply(ds$ResponseTime, list(ds$Instructed, ds$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,1.6), main="Instructed Rules, Early Stimulation", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90)

# For grant purposes, let's forget about inferred vs. instructed.

g <- aggregate(correct[c("EncodingTime", "ResponseTime")], list(Rule=correct$Rule, 
                                                                Stimulation=correct$Stimulation, 
                                                                Site=correct$Site,
                                                                Subject=correct$Subject),
               mean)


summary(aov(EncodingTime ~ (Rule * Stimulation * Site) 
            + Error(Subject/(Rule * Stimulation * Site)), 
            g))


summary(aov(ResponseTime ~ (Rule * Stimulation * Site) 
            + Error(Subject/(Rule * Stimulation * Site)), 
            g))


ms <- tapply(g$ResponseTime, list(g$Site, g$Rule), mean)
ses <- tapply(g$ResponseTime, list(g$Site, g$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,1.6), main="All Stimulations", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90)

text(x = xs, y = ms + ses + 0.05, labels = round(ms, 2))



# Early stim
# ==========

early <- subset(g, g$Stimulation=="Early")

# Encoding times
# --------------

ms <- tapply(early$EncodingTime, list(early$Site, early$Rule), mean)
ses <- tapply(early$EncodingTime, list(early$Site, early$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,1.6), main="Effect of Early Stimulation\non Response Times ", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90, length=0.1)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90, length=0.1)

text(x = xs, y = ms + ses + 0.05, labels = round(ms, 2))


summary(aov(EncodingTime ~ (Rule * Site) 
            + Error(Subject/(Rule * Site)), 
            early))


# Response times
# --------------

ms <- tapply(early$ResponseTime, list(early$Site, early$Rule), mean)
ses <- tapply(early$ResponseTime, list(early$Site, early$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,1.6), main="Effect of Early Stimulation\non Response Times ", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90, length=0.1)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90, length=0.1)


text(x = xs, y = ms + ses + 0.05, labels = round(ms, 2))


summary(aov(ResponseTime ~ (Rule * Site) 
            + Error(Subject/(Rule * Site)), 
            early))


# Late Stim
# =========

late <- subset(g, g$Stimulation=="Late")
ms <- tapply(late$ResponseTime, list(late$Site, late$Rule), mean)
ses <- tapply(late$ResponseTime, list(late$Site, late$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,1.5), main="Effect of Early Stimulation\non Response Times ", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90)

text(x = xs, y = ms + ses + 0.05, labels = round(ms, 2))


summary(aov(ResponseTime ~ (Rule * Site) 
            + Error(Subject/(Rule * Site)), 
            late))


plot.by.2factors(subset(late, late$Rule=="Symbolic"), "ResponseTime", "Subject", "Site") 
plot.by.2factors(subset(early, late$Rule=="Symbolic"), "ResponseTime", "Subject", "Site") 

plot.by.2factors(subset(late, late$Rule=="Concrete"), "ResponseTime", "Subject", "Site") 
plot.by.2factors(subset(early, late$Rule=="Concrete"), "ResponseTime", "Subject", "Site") 



# Pics for grant

k1 <- subset(ds, ds$Stimulation == "Late" & ds$Instructed == "Inferred")
ms <- tapply(k1$ResponseTime, list(k1$Site, k1$Rule), mean)
ses <- tapply(k1$ResponseTime, list(k1$Site, k1$Rule), se)
xs <- barplot(ms, beside = T, legend=F, 
              ylim=c(0, 1.6), main="Re-encoded Tasks", border="white", 
              xlab="Task Type", ylab="Respons Times (secs)",
              col=c("grey65", "grey45", "red", "grey45")
              #col=c("grey65", "grey45")
              )
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, length=0.05, angle=90)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, length=0.05, angle=90)

# Adds some numbers
#text(x = xs, y = ms + ses + 0.05, labels = round(ms, 3))
text(x = xs, y=0.05, labels=c("PMd", "Vertex"), adj=c(0,0.5), srt=90, col="white")
box(bty="o")

k2 <- subset(ds, ds$Stimulation == "Late" & ds$Instructed == "Instructed")
ms <- tapply(k2$ResponseTime, list(k2$Site, k2$Rule), mean)
ses <- tapply(k2$ResponseTime, list(k2$Site, k2$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,2), main="Previously Encoded", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90)
# Adds some numbers
text(x = xs, y = ms + ses + 0.05, labels = round(ms, 3))


## Quick DDM
#  we need 

get.vaTer <- function(Pc, VRT, MRT, s=0.1) {
  s2 = s^2
  # The default value for the scaling parameter s equals 0.1
  if (Pc == 0)
    cat("Oops, Pc == 0!\n")
  if (Pc == 0.5)
    cat("Oops, Pc == .5!\n")
  if (Pc == 1)
    cat("Oops, Pc == 1!\n")
  # If Pc equals 0, .5, or 1, the method will not work, and
  # an edge correction is required.
  L = qlogis(Pc)
  # The function ???qlogis??? calculates the logit.
  x = L*(L*Pc^2 - L*Pc + Pc - .5)/VRT
  v = sign(Pc-.5)*s*x^(1/4)
  # This gives drift rate.
  a = s2*qlogis(Pc)/v
  # This gives boundary separation.
  y = -v * a / s2
  MDT = (a/(2*v)) * (1-exp(y))/(1+exp(y))
  print(paste(MDT, MRT))
  Ter = MRT - MDT
  # This gives nondecision time.
  return(list(v, a, Ter))
}

test <- subset(data, data$Instructed=="Inferred" & data$Stimulation=="Late" & !is.na(data$ResponseTime))

drift1 <- aggregate(test[c("ResponseTime", "Accuracy")], 
                    list(Rule = test$Rule, 
                         Site = test$Site, 
                         Subject = test$Subject), 
                    mean)

drift2 <- aggregate(test[c("ResponseTime")], 
                    list(Rule = test$Rule, 
                         Site = test$Site, 
                         Subject = test$Subject), 
                    var)

names(drift2)[4] <- "VRT"

drift <- merge(drift1, drift2, all=T)

drift$Accuracy[drift$Accuracy == 1] <- 0.99999999

for (s in unique(drift$Subject)) {
  for (t in unique(drift$Rule)) {
    for (c in unique(drift $Site)) {
      sub <- subset(drift, drift$Subject==s & drift$Rule==t & drift$Site==c)
      pars <- get.vaTer(sub$Accuracy, sub$VRT, sub$ResponseTime)
      #print(pars)
      drift$v[drift$Subject==s & drift$Rule==t & drift$Site==c] <- pars[[1]]
      drift$a[drift$Subject==s & drift$Rule==t & drift$Site==c] <- pars[[2]]
      drift$Ter[drift$Subject==s & drift$Rule==t & drift$Site==c] <- pars[[3]]
    }
  }
}

plot.by.2factors(drift, "v", "Rule", "Site", rng=c(0, 0.4, 0.1))
plot.by.2factors(drift, "a", "Rule", "Site", rng=c(0, 1, 0.1))
plot.by.2factors(drift, "Ter", "Rule", "Site", rng=c(0,1,0.1))

ms <- tapply(drift$Ter, list(drift$Site, drift$Rule), mean)
ses <- tapply(drift$Ter, list(drift$Site, drift$Rule), se)
xs <- barplot(ms, beside = T, legend=F, 
              ylim=c(0, 1), main="Re-encoded Tasks", border="white", 
              xlab="Task Type", ylab="Respons Times (secs)",
              col=c("grey65", "grey45", "red", "grey45")
              #col=c("grey65", "grey45")
)

arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, length=0.05, angle=90)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, length=0.05, angle=90)

# Adds some numbers
#text(x = xs, y = ms + ses + 0.05, labels = round(ms, 3))
text(x = xs, y=0.05, labels=c("PMd", "Vertex"), adj=c(0,0.5), srt=90, col="white")
box(bty="o")


summary(aov(a ~ (Rule * Site) + Error(Subject/(Rule * Site)), drift))
summary(aov(v ~ (Rule * Site) + Error(Subject/(Rule * Site)), drift))
summary(aov(Ter ~ (Rule * Site) + Error(Subject/(Rule * Site)), drift))

# Encoding times

k2 <- subset(ds, ds$Stimulation == "Late" & ds$Instructed == "Instructed")
ms <- tapply(k2$ResponseTime, list(k2$Site, k2$Rule), mean)
ses <- tapply(k2$EncodingTime, list(k2$Site, k2$Rule), se)
xs <- barplot(ms, beside = T, legend=T, 
              ylim=c(0,2), main="Instructed Rules, Late Stimulation", border="white", col=c("grey65", "grey45"))
arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, angle=90, length=0.1)
arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, angle=90, length=0.1)
# Adds some numbers
text(x = xs, y = ms + ses + 0.05, labels = round(ms, 2))


modelbaseline<-c(0.987, 1.172, 0.780, 0.965)
modeltms<-c(0.987, 1.25, 0.780, 0.965)
modelencoding<-rep(1.28, 4) - 0.235

#          INFERRED INSTRUCTED
#CONCRETE    0.987      0.780
#SYMBOLIC    1.172      0.965


#           INFERRED INSTRUCTED
#CONCRETE    0.987      0.780
#SYMBOLIC    1.245      0.965

megafigure<-function() {
  oldmar <- par("mar")
  par(mar=c(2,4,3,1)+0.1)
  layout(matrix(c(1:3)))
  k1 <- subset(ds, ds$Stimulation == "Early")
  ms <- tapply(k1$EncodingTime, list(k1$Rule, k1$Instructed), mean)
  ses <- tapply(k1$EncodingTime, list(k1$Rule, k1$Instructed), se)
  xs <- barplot(ms, beside = T, legend=T, 
                args.legend=c(border=NULL, bty="n", x="top"),
                ylim=c(0, 1.7), main="(A) Encoding Times,\nAll Trials", border="white",
                xlab="Task Type", ylab="Respons Times (secs)",
                #col=c("grey65", "grey45", "grey65", "grey45")
                col=c("#22AA22", "yellow", "#22AA22", "yellow")
                #col=c("grey65", "grey45")
  )
  
  arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, length=0.05, angle=90)
  arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, length=0.05, angle=90)
  #text(x = xs, y = ms + ses + 0.1, labels = round(ms, 2))
  text(x = xs, y = 0.05, labels = round(ms, 3), adj=c(0.5,0))
  points(x = xs, y = modelencoding, pch=21, cex=2, col="blue", bg="#1111CC55")
  box(bty="o", lwd=2, col="blue")
  
  # Execution
  k1 <- subset(ds, ds$Stimulation == "Late" & ds$Site == "Vertex")
  ms <- tapply(k1$ResponseTime, list(k1$Rule, k1$Instructed), mean)
  ses <- tapply(k1$ResponseTime, list(k1$Rule, k1$Instructed), se)
  xs <- barplot(ms, beside = T, legend=F, 
                ylim=c(0, 1.7), main="(B) Response Times,\nTMS on Vertex (Control Trials)", border="white", 
                xlab="Task Type", ylab="Respons Times (secs)",
                #col=c("grey65", "grey45", "grey65", "grey45")
                col=c("#22AA22", "yellow", "#22AA22", "yellow")
                #col=c("grey65", "grey45")
  )
  arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, length=0.05, angle=90)
  arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, length=0.05, angle=90)
  #text(x = xs, y = ms + ses + 0.1, labels = round(ms, 3))
  text(x = xs, y = 0.05, labels = round(ms, 3), adj=c(0.5,0))
  points(x = xs, y = modelbaseline, pch=21, cex=2, col="blue", bg="#1111CC55")
  box(bty="o", lwd=2, col="red")
  
  # Execution, TMS
  k1 <- subset(ds, ds$Stimulation == "Late" & ds$Site == "PMd")
  ms <- tapply(k1$ResponseTime, list(k1$Rule, k1$Instructed), mean)
  ses <- tapply(k1$ResponseTime, list(k1$Rule, k1$Instructed), se)
  xs <- barplot(ms, beside = T, legend=F, 
                ylim=c(0, 1.7), main="(C) Response Times,\nTMS on PMd", border="white", 
                xlab="Task Type", ylab="Respons Times (secs)",
                #col=c("grey65", "red", "grey65", "grey45")
                col=c("#22AA22", "red", "#22AA22", "yellow")
                #col=c("grey65", "grey45")
  )
  arrows(x0=xs, x1=xs, y0=ms, y1 = ms + ses, length=0.05, angle=90)
  arrows(x0=xs, x1=xs, y0=ms, y1 = ms - ses, length=0.05, angle=90)
  #text(x = xs, y = ms + ses + 0.1, labels = round(ms, 2))
  text(x = xs, y = 0.05, labels = round(ms, 3), adj=c(0.5,0))
  points(x = xs, y = modeltms, pch=21, cex=2, col=c("blue", "orange", "blue", "blue"), 
         bg=c("#1111CC55", "#DD2222BB", "#1111CC55", "#1111CC55"))
  box(bty="o", col="red", lwd=2)

  # End
  par(mar=oldmar)
}

png("newFig.png", res=300, height = 5, width = 3, units="in")
megafigure()
dev.off()

## RMSE and CHISQ

rmse <- function(vec1, vec2) {
  sqrt(mean((vec1-vec2)**2))
}

# Encoding times
enc <- subset(ds, ds$Stimulation == "Early")
encms <- tapply(enc$EncodingTime, list(enc$Rule, enc$Instructed), mean)
encvar <- tapply(enc$EncodingTime, list(enc$Rule, enc$Instructed), var)
rmse(modelencoding, c(encms))

X2 <- sum(((c(encms) - modelencoding) ** 2) / (c(encvar) ** 2)) 
1 - pchisq(X2,3)

# Vertex Exec times
vex <-subset(ds, ds$Stimulation == "Late" & ds$Site == "Vertex")
basems <- tapply(vex$ResponseTime, list(vex$Rule, vex$Instructed), mean)
basevar <- tapply(vex$ResponseTime, list(vex$Rule, vex$Instructed), var)
rmse(modelbaseline, c(basems))

X2 <- sum(((c(basems) - modelbaseline) ** 2) / (c(basevar) ** 2)) 
1 - pchisq(X2,3)


# PMd Exec times
pex <-subset(ds, ds$Stimulation == "Late" & ds$Site == "PMd")
tmsms <- tapply(pex$ResponseTime, list(pex$Rule, pex$Instructed), mean)
tmsvar <- tapply(pex$ResponseTime, list(pex$Rule, pex$Instructed), var)
rmse(modeltms, ms)

X2 <- sum(((c(tmsms) - modeltms) ** 2) / (c(tmsvar) ** 2)) 
1 - pchisq(X2,3)

all_model <- c(modelencoding, modelbaseline, modeltms)
all_data <- c(c(encms), c(basems), c(tmsms))
all_var <- c(c(encvar), c(basevar), c(tmsvar))

X2 <- sum(((c(all_data) - all_model) ** 2) / (c(all_var) ** 2)) 
1 - pchisq(X2,3)
