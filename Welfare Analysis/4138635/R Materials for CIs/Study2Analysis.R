# ========================================================================
#
# The Relationship Between Mental Representations of Welfare Recipients and 
# Attitudes Toward Welfare
#
# Brown-Ianuzzi, Dotsch, Cooley, & Payne (in press)
#
# Script to compute Classification Images for Study 2
#
# ========================================================================
 
library(rcicr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(psych)
library(jpeg)

# Read welfare data (Study 2)
welfaredata <- read.csv('../Data/Dotsch_RevCorr_BtwnSubj_Welfare_OnlyRevCorr_Long.csv')
nonwelfaredata <- read.csv('../Data/Dotsch_RevCorr_BtwnSubj_NonWelfare_OnlyRevCorr_Long.csv')

# Recode responses
# 1 = Positive stim
# 0 = Negative stim
welfaredata$response <- (welfaredata$response * 2) - 1
nonwelfaredata$response <- (nonwelfaredata$response * 2) - 1

# Indicate repeated trials (pic > 400)
welfaredata$repetition <- "first"
welfaredata$repetition[welfaredata$pic > 400] <- "second"
nonwelfaredata$repetition <- "first"
nonwelfaredata$repetition[nonwelfaredata$pic > 400] <- "second"

# Recode repeated trials
welfaredata$pic[welfaredata$repetition == "second"] <- welfaredata$pic[welfaredata$repetition == 'second'] - 400
nonwelfaredata$pic[nonwelfaredata$repetition == 'second'] <- nonwelfaredata$pic[nonwelfaredata$repetition == 'second'] - 400

# Combine
welfaredata$target <- "welfare"
nonwelfaredata$target <- "nonwelfare"
combineddata <- merge(welfaredata, nonwelfaredata, all=T)

# Compute consistency
consistency <- combineddata %>% 
  filter(pic <= 50) %>%
  group_by(ID, target) %>%
  spread(repetition, response) %>%
  mutate(consistent=first==second) %>%
  summarize(consistency=sum(consistent) / 50)

ggplot(consistency, aes(target, consistency)) + 
  geom_violin()
 

# Remove repetitions and missing values
welfaredata <- welfaredata %>% filter(repetition=='first' & response %in% c(-1,1))
nonwelfaredata <- nonwelfaredata %>% filter(repetition=='first' & response %in% c(-1,1))

# Compute CIs with constant scaling (using autoscale)
welfareCIs <- batchGenerateCI2IFC(welfaredata, 'ID', 'pic', 'response', 'pat3', 'kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', label='welfare', saveasjpeg=F)
nonwelfareCIs <- batchGenerateCI2IFC(nonwelfaredata, 'ID', 'pic', 'response', 'pat3', 'kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', label='nonwelfare', saveasjpeg=F)

allCIs <- c(welfareCIs, nonwelfareCIs)
allCIs <- autoscale(allCIs, saveasjpegs=T)

# Group CIs
groupCIs <- list(
  welfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', 'kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', saveasjpeg=F),
  nonwelfare=generateCI2IFC(nonwelfaredata$pic, nonwelfaredata$response, 'pat3', 'kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', saveasjpeg=F)
)

groupCIs <- autoscale(groupCIs, saveasjpegs = T)

# Group AntiCIs

antigroupCIs <- list(
  antiwelfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', 'kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', saveasjpeg=F, antiCI = T),
  antinonwelfare=generateCI2IFC(nonwelfaredata$pic, nonwelfaredata$response, 'pat3', 'kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', saveasjpeg=F, antiCI = T)
)

antigroupCIs <- autoscale(antigroupCIs, saveasjpegs = T)

# Compute group correlations
cis <- data.frame(
  'welfare'=as.vector(groupCIs$welfare$ci),
  'nonwelfare'=as.vector(groupCIs$nonwelfare$ci),
  'antiwelfare'=as.vector(antigroupCIs$antiwelfare$ci),
  'antinonwelfare'=as.vector(antigroupCIs$antinonwelfare$ci)
)

c <- corr.test(cis)
print(c$ci)

# Computed masked correlations
mask <- apply(readJPEG('mask_fullface.jpg'), MARGIN = c(1,2), FUN=mean) == 1

masked.cis <- data.frame(
  'welfare'=as.vector(groupCIs$welfare$ci[mask]),
  'nonwelfare'=as.vector(groupCIs$nonwelfare$ci[mask]),
  'antiwelfare'=as.vector(antigroupCIs$antiwelfare$ci[mask]),
  'antinonwelfare'=as.vector(antigroupCIs$antinonwelfare$ci[mask])
)

c <- corr.test(masked.cis)
print(c$ci)

rm(allCIs, nonwelfareCIs, welfareCIs)
save.image('Study2.RData')

