library(ggplot2)
library(reshape2)
library(R.matlab)


pred <- readMat("~/Dropbox/MATLAB/Reverse Correlation/preds.mat")
pred <- as.data.frame(pred)
pred$Response <- as.factor(pred$Response)
levels(pred$Response) <- c('No target present', 'Target present')
pred$Target <- NA
pred$Target[pred$Participant==1|pred$Participant==5] <- 'x'
pred$Target[pred$Participant==2|pred$Participant==6] <- 'p'
pred$Target[pred$Participant==3|pred$Participant==7] <- 'pi'
pred$Target[pred$Participant==4|pred$Participant==8] <- 'xi'
pred$Target <- as.factor(pred$Target)

pred$RelevantPred <- NA
pred$RelevantPred[pred$Target=='x'] <- pred_xs$`prediction with x`
pred$RelevantPred[pred$Target=='p'] <- pred_ps$`prediction with +`
pred$RelevantPred[pred$Target=='pi'] <- pred_pis$`prediction with +i`
pred$RelevantPred[pred$Target=='xi'] <- pred_xis$`prediction with xi`

colnames(pred) <- c("Participant","Response","prediction with x","prediction with +","prediction with xi","prediction with +i")

pred_xs <- pred[which(pred$Participant==1|pred$Participant==5),]
pred_ps <- pred[which(pred$Participant==2|pred$Participant==6),]
pred_pis <- pred[which(pred$Participant==3|pred$Participant==7),]
pred_xis <- pred[which(pred$Participant==4|pred$Participant==8),]

#-------------------------- Generating the Means Data Frame --------------------------------

mean_pred_x <- matrix(c(mean(pred_xs$`prediction with x`[pred_xs$Response==1]), mean(pred_xs$`prediction with +`[pred_xs$Response==1]),mean(pred_xs$`prediction with +i`[pred_xs$Response==1]), mean(pred_xs$`prediction with xi`[pred_xs$Response==1])), ncol=4)
mean_pred_p <- matrix(c(mean(pred_ps$`prediction with x`[pred_ps$Response==1]), mean(pred_ps$`prediction with +`[pred_ps$Response==1]),mean(pred_ps$`prediction with +i`[pred_ps$Response==1]), mean(pred_ps$`prediction with xi`[pred_ps$Response==1])), ncol=4)
mean_pred_pis <- matrix(c(mean(pred_pis$`prediction with x`[pred_pis$Response==1]), mean(pred_pis$`prediction with +`[pred_pis$Response==1]),mean(pred_pis$`prediction with +i`[pred_pis$Response==1]), mean(pred_pis$`prediction with xi`[pred_pis$Response==1])), ncol=4)
mean_pred_xis <- matrix(c(mean(pred_xis$`prediction with x`[pred_xis$Response==1]), mean(pred_xis$`prediction with +`[pred_xis$Response==1]),mean(pred_xis$`prediction with +i`[pred_xis$Response==1]), mean(pred_xis$`prediction with xi`[pred_xis$Response==1])), ncol=4)

means <- rbind(mean_pred_x,mean_pred_p, mean_pred_pis,mean_pred_xis)
means <- t(means)
means <- data.frame(cbind(colnames(pred[3:6]), means))
colnames(means) <- c('Groups','x','+','xi','+i')

means.m <- melt(means,id.vars= c('Groups'))
means.m$value <- as.numeric(means.m$value)

#-------------------------- Generating the Standard Error Data Frame -----------------------

sd_pred_x <- matrix(c(sd(pred_xs$`prediction with x`[pred_xs$Response==1])/sqrt(length(pred_xs$`prediction with x`[pred_xs$Response==1])), sd(pred_xs$`prediction with +`[pred_xs$Response==1])/sqrt(length(pred_xs$`prediction with +`[pred_xs$Response==1])),sd(pred_xs$`prediction with +i`[pred_xs$Response==1])/sqrt(length(pred_xs$`prediction with +i`[pred_xs$Response==1])), sd(pred_xs$`prediction with xi`[pred_xs$Response==1]))/sqrt(length(pred_xs$`prediction with xi`[pred_xs$Response==1])), ncol=4)
sd_pred_p <- matrix(c(sd(pred_ps$`prediction with x`[pred_ps$Response==1])/sqrt(length(pred_ps$`prediction with x`[pred_ps$Response==1])), sd(pred_ps$`prediction with +`[pred_ps$Response==1])/sqrt(length(pred_ps$`prediction with +`[pred_ps$Response==1])),sd(pred_ps$`prediction with +i`[pred_ps$Response==1])/sqrt(length(pred_ps$`prediction with +i`[pred_ps$Response==1])), sd(pred_ps$`prediction with xi`[pred_ps$Response==1]))/sqrt(length(pred_ps$`prediction with xi`[pred_ps$Response==1])), ncol=4)
sd_pred_pis <- matrix(c(sd(pred_pis$`prediction with x`[pred_pis$Response==1])/sqrt(length(pred_pis$`prediction with x`[pred_pis$Response==1])), sd(pred_pis$`prediction with +`[pred_pis$Response==1])/sqrt(length(pred_pis$`prediction with +`[pred_pis$Response==1])),sd(pred_pis$`prediction with +i`[pred_pis$Response==1])/sqrt(length(pred_pis$`prediction with +i`[pred_pis$Response==1])), sd(pred_pis$`prediction with xi`[pred_pis$Response==1]))/sqrt(length(pred_pis$`prediction with xi`[pred_pis$Response==1])), ncol=4)
sd_pred_xis <- matrix(c(sd(pred_xis$`prediction with x`[pred_xis$Response==1])/sqrt(length(pred_xis$`prediction with x`[pred_xis$Response==1])), sd(pred_xis$`prediction with +`[pred_xis$Response==1])/sqrt(length(pred_xis$`prediction with +`[pred_xis$Response==1])),sd(pred_xis$`prediction with +i`[pred_xis$Response==1])/sqrt(length(pred_xis$`prediction with +i`[pred_xis$Response==1])), sd(pred_xis$`prediction with xi`[pred_xis$Response==1]))/sqrt(length(pred_xis$`prediction with xi`[pred_xis$Response==1])), ncol=4)

sds <- as.data.frame(rbind(sd_pred_x,sd_pred_p, sd_pred_pis,sd_pred_xis), row.names = c('x','+','+i','xi'))
colnames(sds) <- colnames(pred)[3:6]


ggplot(means.m, aes(x=Groups, y=value))+   
  geom_bar(aes(fill=variable), position = "dodge", stat="identity") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


#--------------------------------

for(i in unique(pred$Participant)){
  print(c(i,mean(pred$Response[pred$Participant==i])))
}


normdata <- as.data.frame(cbind(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01), mean = 0)))
normdata <- cbind(normdata,dnorm(seq(from=-3,to=3,by=0.01), mean = 0.2) )

normdata <- melt(normdata, id = "V1")

ggplot(normdata, aes(x=V1, y=value,colour=variable))+   
  geom_line() +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
  scale_color_manual(values=c("blue", "red"))


df <- data.frame(x = c(rnorm(10000, 0), rnorm(10000, 0.2)),
                 g = gl(2, 10000))

ggplot(df, aes(x, colour = g)) + 
  stat_ecdf() + 
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
  scale_color_manual(values=c("blue", "red")) + 
  scale_x_continuous(limits = c(-3, 3))

pdf("ECDF Image-wise Correlation.pdf", width = 8, height = 4.5)
ggplot(predwithCNN, aes(1-CNNoutput$RelevantOutput, colour = Response)) + 
  labs(x = 'Target-Stimulus Image-wise Correlation', y = 'Cumulative Density', colour = "Classification")+
  theme_bw()+
  stat_ecdf() + 
  theme(axis.line = element_line(color ='black')) + 
  scale_color_manual(values=c("blue", "red"))
dev.off()

RelevantES <- 


ggplot(data=predwithCNN, aes(predwithCNN$`prediction with x`)) +
  geom_histogram(binwidth=0.001) + 
  theme_bw() +
  labs(title="Distribution of stimuli with respect to target similarity") +
  labs(x="Correlation Between Target and Stimulus")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

gen_stats <- function(x){
  mean <- mean(x)
  sd <- sd(x)
  se <- se(x)
  
  return(c(mean,sd,se))
}

EStarg <- array(data = NA, dim=c(4,4,2,3), dimnames <- list(c('P-+','P-+i','P-x','P-xi'), c('predx','pred+', 'predxi', 'pred+i'), c('TargetAbsent','TargetPresent'), c('Mean', 'SD', 'SE')))

for(i in 1:dim(EStarg)[1]){
  for(j in 1:dim(EStarg)[2]){

    EStarg[i,j,,] <- aggregate(pred[pred$Target==levels(pred$Target)[j],i+2],
                               list(pred$Response[pred$Target==levels(pred$Target)[j]]),
                               gen_stats)$x
  }
}


test <- aggregate(predexp$Pred, list(predexp$SameTarget), mean)
test$se <- aggregate(predexp$Pred, list(predexp$SameTarget), se)$x
limits <- aes(ymax = test$x + test$se,
              ymin = test$x - test$se)

ggplot(data=test, aes(x=Group.1, 
                         y=x)) + 
  geom_bar(stat="identity", fill=c('blue','red')) +
  geom_errorbar(limits, width=0.25) + 
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x=element_blank())


bins <- pretty(predwithCNN$RelevantPred,n = 10)

freq <- c()

for(i in 1:(length(bins)-1)){
  
  ID <- which(predwithCNN$RelevantPred > bins[i] & predwithCNN$RelevantPred < bins[i+1])
  
  freq <- c(freq, mean(as.numeric(predwithCNN$Response)[ID]) - 1)
  
}

plot(bins[-1], freq, type='l')
lines(y=rep(0.5, times=2), x=c(min(bins)+0.5*min(bins), max(bins)+0.5*max(bins)),col='red')


bins <- pretty(CNNoutput$RelevantOutput,n = 10)

freq <- c()

for(i in 1:(length(bins)-1)){
  
  ID <- which(CNNoutput$RelevantOutput > bins[i] & CNNoutput$RelevantOutput < bins[i+1])
  
  freq <- c(freq, mean(as.numeric(predwithCNN$Response)[ID]) - 1)
  
}

plot(bins[-1], freq, type='l')
lines(y=rep(0.5, times=2), x=c(min(bins)+0.5*min(bins), max(bins)+0.5*max(bins)),col='red')


bins <- pretty(predwithCNN$RevelantCNN,n = 10)

freq <- c()

for(i in 1:(length(bins)-1)){
  
  ID <- which(predwithCNN$RevelantCNN > bins[i] & predwithCNN$RevelantCNN < bins[i+1])
  
  freq <- c(freq, mean(as.numeric(predwithCNN$Response)[ID]) - 1)
  
}

plot(bins[-1], freq, type='l')
lines(y=rep(0.5, times=2), x=c(min(bins)+0.5*min(bins), max(bins)+0.5*max(bins)),col='red')

bins <- pretty(c(0,CNNsigmoid$RelevantPred),n = 10)

freq <- c()
irrFreq <- c()

for(i in 1:(length(bins)-1)){
  
  ID <- which(CNNsigmoid$RelevantPred > bins[i] & CNNsigmoid$RelevantPred < bins[i+1])
  irrID <- which(CNNsigmoid$IrrelevantPred > bins[i] & CNNsigmoid$IrrelevantPred < bins[i+1])
  
  freq <- c(freq, mean(as.numeric(predwithCNN$Response)[ID]) - 1)
  irrFreq <- c(irrFreq, mean(as.numeric(predwithCNN$Response)[irrID])-1)
}

plot(bins[-length(bins)], freq, type='l', ylim=c(0,1), xlim = c(0, max(bins)))
lines(bins[-length(bins)], irrFreq, type='l', col='blue')
lines(y=rep(mean(as.numeric(predwithCNN$Response))-1, times=2), x=c(min(bins)+0.5*min(bins), max(bins)+0.5*max(bins)),col='red')
