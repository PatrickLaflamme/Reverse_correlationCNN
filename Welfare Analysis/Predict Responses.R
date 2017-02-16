library(rcicr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(psych)
library(jpeg)
library(imager)


welfaredata <- read.csv("Study1_ReverseCorrelationDataLongForm_codedmissingvalues.csv")

# Recode responses
# 1 = Positive stim
# 0 = Negative stim
welfaredata$response <- (welfaredata$response * 2) - 1

# Indicate repeated trials (pic > 400)
welfaredata <- subset(welfaredata, pic <= 400)
welfareCIs <- batchGenerateCI2IFC(welfaredata, 'SubjectID', 'pic', 'response', 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata')
welfareantiCIs <- batchGenerateCI2IFC(welfaredata, 'SubjectID', 'pic', 'response', 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', antiCI = T)
names(welfareantiCIs) <- paste0(names(welfareantiCIs), "_anti")

groupCIs <- list(
  welfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata'),
  antiwelfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', antiCI = T)
)

groupCIs <- autoscale(groupCIs, saveasjpegs = T)

p <- generateNoisePattern(img_size)
pb <- dplyr::progress_estimated(400)

stimuli <- matlab::zeros(img_size, img_size, n_trials)
for(i in 0:399){
  pb$tick()$print()
  stimuli[, , i+1] <- generateNoiseImage(t(stimuli_params[["pat3"]][i+1,]), p)
  
  stimulus <- ((stimuli[, , i+1] + 0.3)/0.6)
  combined <- (stimulus + base_faces[['pat3']])/2
  jpeg::writeJPEG(combined, paste(stimulus_path, paste(label, 
                                                       base_faces[["pat3"]], sprintf("%05d_ori.jpg", i), 
                                                       sep = "_"), sep = "/"), quality = 1)
  stimulus <- ((-stimuli[, , i+1] + 0.3)/0.6)
  stimulus <- (stimulus + base_faces[['pat3']])/2
  jpeg::writeJPEG(stimulus, paste(stimulus_path, paste(label, 
                                                       base_faces[["pat3"]], sprintf("%05d_inv.jpg", i), 
                                                       sep = "_"), sep = "/"), quality = 1)
}






CI <- matrix(load.image("./cis/ci_pat3.jpg"), nrow=512)
antiCI <- matrix(load.image("./cis/antici_pat3.jpg"), nrow=512)

poscor <- c()
negcor <- c()

pb <- dplyr::progress_estimated(400)

for(i in 0:399){
  
  pb$tick()$print()
  
  pos <- matrix(load.image(paste("MyGennedStims/kp1_0.994736842105263", sprintf("%05d_ori.jpg", i), sep='_')), nrow=512)
  
  neg <- matrix(load.image(paste("MyGennedStims/kp1_0.994736842105263", sprintf("%05d_inv.jpg", i), sep='_')), nrow=512)

  poscor <- c(poscor, cor(c(CI), c(pos)))
  
  negcor <- c(negcor, cor(c(CI), c(neg)))
}


cor(rep(poscor-negcor, times = 121)[welfaredata$response!=1997], welfaredata$response[welfaredata$response!=1997])

eff <- effsize::cohen.d(rep(poscor-negcor, times=123)[(welfaredata$response %in% c(-1,1))], as.factor(welfaredata$response[(welfaredata$response %in% c(-1,1))]))

Correlation_btw_CI_and_img <- welfaredata[(welfaredata$response %in% c(-1,1)),]
Correlation_btw_CI_and_img$response <- as.factor(Correlation_btw_CI_and_img$response)
levels(Correlation_btw_CI_and_img$response) <- c("no", "yes")
correlation <- rep(poscor, times = 123)[(welfaredata$response %in% c(-1,1))]
ggplot(Correlation_btw_CI_and_img, aes(correlation, colour = Correlation_btw_CI_and_img$response)) + stat_ecdf() + labs(list(title = paste("Effect Size:", eff$estimate, sep = ' '), x = "Correlation between Image and CI", y = "Proportion of Total", colour = "Response"))

bins <- pretty(poscor-negcor,n = 10)

corDifs <- poscor-negcor

freq <- c()

for(i in 1:(length(bins)-1)){
  
  ID <- which(corDifs>bins[i] & corDifs < bins[i+1])
  
  freq <- c(freq, mean(welfaredata$response[(welfaredata$pic %in% ID) & welfaredata$response!=999]))
  
}

plot(bins[-1], freq, type='l')
lines(y=rep(0.5, times=2), x=c(min(bins)+0.5*min(bins), max(bins)+0.5*max(bins)),col='red')


#---- Study 1 ---- 

welfaredata <- read.csv("Study1_ReverseCorrelationDataLongForm_codedmissingvalues.csv")

# Recode responses
# 1 = Positive stim
# 0 = Negative stim
welfaredata$response <- (welfaredata$response * 2) - 1

# Indicate repeated trials (pic > 400)
welfaredata <- subset(welfaredata, pic <= 400)

groupCIs <- list(
  welfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata'),
  antiwelfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', antiCI = T)
)

CI <- matrix(load.image("./cis/ci_pat3_1.jpg"), nrow=512)
antiCI <- matrix(load.image("./cis/antici_pat3_1.jpg"), nrow=512)

poscor <- c()
negcor <- c()

pb <- dplyr::progress_estimated(400)

for(i in 0:399){
  
  pb$tick()$print()
  
  pos <- matrix(load.image(paste("MyGennedStims/kp1_0.994736842105263", sprintf("%05d_ori.jpg", i), sep='_')), nrow=512)
  
  neg <- matrix(load.image(paste("MyGennedStims/kp1_0.994736842105263", sprintf("%05d_inv.jpg", i), sep='_')), nrow=512)
  
  poscor <- c(poscor, cor(c(CI), c(pos)))
  
  negcor <- c(negcor, cor(c(CI), c(neg)))
}

eff <- effsize::cohen.d(rep(poscor-negcor, times=121)[(welfaredata$response %in% c(-1,1))], as.factor(welfaredata$response[(welfaredata$response %in% c(-1,1))]))

Correlation_btw_CI_and_img <- welfaredata[(welfaredata$response %in% c(-1,1)),]
Correlation_btw_CI_and_img$response <- as.factor(Correlation_btw_CI_and_img$response)
levels(Correlation_btw_CI_and_img$response) <- c("no", "yes")
correlation <- rep(poscor-negcor, times = 121)[(welfaredata$response %in% c(-1,1))]
ggplot(Correlation_btw_CI_and_img, aes(correlation, colour = Correlation_btw_CI_and_img$response)) + stat_ecdf() + labs(list(title = paste("Effect Size:", eff$estimate, sep = ' '), x = "Correlation between Image and CI", y = "Proportion of Total", colour = "Response"))

#--- predictions

stimprams <- stimuli_params$pat3

welfaregroup <- group_by(filter(welfaredata, response %in% c(-1,1)), pic)

avImdiff <- welfaregroup$response %*% do.call(rbind, replicate(121, as.matrix(stimprams), simplify=FALSE))[welfaredata$response %in% c(-1,1),]
avImdiff <- avImdiff-min(avImdiff)
avImdiff <- avImdiff/max(avImdiff)
avImdiff <- avImdiff*2 -1

sinusioudCorsdiff <- cor(t(avImdiff), t(stimprams))

Pyes <- summarise(welfaregroup, mean = mean(response*0.5+0.5))

cor(t(sinusioudCorsdiff), Pyes$mean)

effsize::cohen.d(rep(t(sinusioudCors),times=121)[welfaredata$response!=1997], as.factor(welfaregroup$response))

cor(rep(t(sinusioudCors),times=121)[welfaredata$response!=1997], welfaregroup$response)

plot(t(sinusioudCorsdiff), Pyes$mean, ylim=c(0,1))
abline(lm(Pyes$mean~t(sinusioudCorsdiff)), col='blue')
abline(0.5,0, col='red')
title(main="Study1 - pred Welfare")


#---- Study 2 -----

welfaredata <- read.csv("Study2_ReverseCorrelations_Welfare_LongForm.csv")

# Recode responses
# 1 = Positive stim
# 0 = Negative stim
welfaredata$response <- (welfaredata$response * 2) - 1

# Indicate repeated trials (pic > 400)
welfaredata <- subset(welfaredata, pic <= 400)

groupCIs <- list(
  welfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata'),
  antiwelfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', antiCI = T)
)

CI <- matrix(load.image("./cis/ci_pat3_2a.jpg"), nrow=512)
antiCI <- matrix(load.image("./cis/antici_pat3_2a.jpg"), nrow=512)

poscor <- c()
negcor <- c()

pb <- dplyr::progress_estimated(400)

for(i in 0:399){
  
  pb$tick()$print()
  
  pos <- matrix(load.image(paste("MyGennedStims/kp1_0.994736842105263", sprintf("%05d_ori.jpg", i), sep='_')), nrow=512)
  
  neg <- matrix(load.image(paste("MyGennedStims/kp1_0.994736842105263", sprintf("%05d_inv.jpg", i), sep='_')), nrow=512)
  
  poscor <- c(poscor, cor(c(CI), c(pos)))
  
  negcor <- c(negcor, cor(c(CI), c(neg)))
}

eff <- effsize::cohen.d(rep(poscor-negcor, times=123)[(welfaredata$response %in% c(-1,1))], as.factor(welfaredata$response[(welfaredata$response %in% c(-1,1))]))

Correlation_btw_CI_and_img <- welfaredata[(welfaredata$response %in% c(-1,1)),]
Correlation_btw_CI_and_img$response <- as.factor(Correlation_btw_CI_and_img$response)
levels(Correlation_btw_CI_and_img$response) <- c("no", "yes")
correlation <- rep(poscor-negcor, times = 123)[(welfaredata$response %in% c(-1,1))]
ggplot(Correlation_btw_CI_and_img, aes(correlation, colour = Correlation_btw_CI_and_img$response)) + stat_ecdf() + labs(list(title = paste("Effect Size:", abs(eff$estimate), sep = ' '), x = "Correlation between Image and CI", y = "Proportion of Total", colour = "Response"))

#--- predictions

stimprams <- stimuli_params$pat3

avImdiff <- welfaregroup$response %*% do.call(rbind, replicate(123, as.matrix(stimprams), simplify=FALSE))[welfaredata$response %in% c(-1,1),]
avImdiff <- avImdiff-min(avImdiff)
avImdiff <- avImdiff/max(avImdiff)
avImdiff <- avImdiff*2 -1

sinusioudCorsdiff <- cor(t(avImdiff), t(stimprams))

welfaregroup <- group_by(filter(welfaredata, response %in% c(-1,1)), pic)

Pyes <- summarise(welfaregroup, mean = mean(response*0.5+0.5))

cor(t(sinusioudCorsdiff), Pyes$mean)


effsize::cohen.d(rep(t(sinusioudCors),times=121)[welfaredata$response!=1997], as.factor(welfaregroup$response))

cor(rep(t(sinusioudCors),times=121)[welfaredata$response!=1997], welfaregroup$response)

plot(t(sinusioudCorsdiff), Pyes$mean, ylim=c(0,1))
abline(lm(Pyes$mean~t(sinusioudCorsdiff)), col='blue')
abline(0.5,0, col='red')
title(main="Study2 - pred Welfare with study1 CI")


stimuli <- generateNoiseImage(t(avImdiff), p)

stimulus <- ((stimuli + 0.3)/0.6)
combined <- (stimulus + base_faces[['pat3']])/2

combined <- combined - min(combined)

combined <- combined/max(combined)
jpeg::writeJPEG(combined, paste( paste(label, 
                                       base_faces[["pat3"]], sprintf("%05d_ori.jpg", i), 
                                       sep = "_"), sep = "/"), quality = 1)
stimulus <- ((-1*stimuli + 0.3)/0.6)

combined <- (stimulus + base_faces[['pat3']])/2

combined <- combined - min(combined)

combined <- combined/max(combined)

jpeg::writeJPEG(combined, paste( paste(label, 
                                       base_faces[["pat3"]], sprintf("%05d_inv.jpg", i), 
                                       sep = "_"), sep = "/"), quality = 1)



#------ Study2b

welfaredata <- read.csv("Study2_ReverseCorrelations_NonWelfare_LongForm.csv")

# Recode responses
# 1 = Positive stim
# 0 = Negative stim
welfaredata$response <- (welfaredata$response * 2) - 1

# Indicate repeated trials (pic > 400)
welfaredata <- subset(welfaredata, pic <= 400)

groupCIs <- list(
  welfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata'),
  antiwelfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './4138635/R Materials for CIs/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', antiCI = T)
)

CI <- matrix(load.image("./cis/ci_pat3_2b.jpg"), nrow=512)
antiCI <- matrix(load.image("./cis/antici_pat3_2b.jpg"), nrow=512)

poscor <- c()
negcor <- c()

pb <- dplyr::progress_estimated(400)

for(i in 0:399){
  
  pb$tick()$print()
  
  pos <- matrix(load.image(paste("MyGennedStims/kp1_0.994736842105263", sprintf("%05d_ori.jpg", i), sep='_')), nrow=512)
  
  neg <- matrix(load.image(paste("MyGennedStims/kp1_0.994736842105263", sprintf("%05d_inv.jpg", i), sep='_')), nrow=512)
  
  poscor <- c(poscor, cor(c(CI), c(pos)))
  
  negcor <- c(negcor, cor(c(CI), c(neg)))
}

eff <- effsize::cohen.d(rep(poscor-negcor, times=115)[(welfaredata$response %in% c(-1,1))], as.factor(welfaredata$response[(welfaredata$response %in% c(-1,1))]))

Correlation_btw_CI_and_img <- welfaredata[(welfaredata$response %in% c(-1,1)),]
Correlation_btw_CI_and_img$response <- as.factor(Correlation_btw_CI_and_img$response)
levels(Correlation_btw_CI_and_img$response) <- c("no", "yes")
correlation <- rep(poscor-negcor, times = 115)[(welfaredata$response %in% c(-1,1))]
ggplot(Correlation_btw_CI_and_img, aes(correlation, colour = Correlation_btw_CI_and_img$response)) + stat_ecdf() + labs(list(title = paste("Effect Size:", abs(eff$estimate), sep = ' '), x = "Correlation between Image and CI", y = "Proportion of Total", colour = "Response"))



pb <- dplyr::progress_estimated(400)

stimprams <- stimuli_params$pat3

avImdiff <- welfaregroup$response %*% do.call(rbind, replicate(115, as.matrix(stimprams), simplify=FALSE))[welfaredata$response %in% c(-1,1),]
avImdiff <- avImdiff-min(avImdiff)
avImdiff <- avImdiff/max(avImdiff)
avImdiff <- avImdiff*2 -1

sinusioudCorsdiff <- cor(t(avImdiff), t(stimprams))

welfaregroup <- group_by(filter(welfaredata, response %in% c(-1,1)), pic)

Pyes <- summarise(welfaregroup, mean = mean(response*0.5+0.5))

cor(t(sinusioudCorsdiff), Pyes$mean)

effsize::cohen.d(rep(t(sinusioudCors),times=121)[welfaredata$response!=1997], as.factor(welfaregroup$response))

cor(rep(t(sinusioudCors),times=121)[welfaredata$response!=1997], welfaregroup$response)

plot(t(sinusioudCorsdiff), Pyes$mean, ylim=c(0,1))
abline(lm(Pyes$mean~t(sinusioudCorsdiff)), col='blue')
abline(0.5,0, col='red')
title(main="Study2 - pred Non-welfare with Study1 CI")

  stimuli <- generateNoiseImage(t(avImdiff), p)
  
  stimulus <- ((stimuli + 0.3)/0.6)
  combined <- (stimulus + base_faces[['pat3']])/2
  
  combined <- combined - min(combined)
  
  combined <- combined/max(combined)
  jpeg::writeJPEG(combined, paste( paste(label, 
                                                       base_faces[["pat3"]], sprintf("%05d_ori.jpg", i), 
                                                       sep = "_"), sep = "/"), quality = 1)
  stimulus <- ((-1*stimuli + 0.3)/0.6)
  
  combined <- (stimulus + base_faces[['pat3']])/2
  
  combined <- combined - min(combined)
  
  combined <- combined/max(combined)
  
  jpeg::writeJPEG(combined, paste( paste(label, 
                                  base_faces[["pat3"]], sprintf("%05d_inv.jpg", i), 
                                  sep = "_"), sep = "/"), quality = 1)

