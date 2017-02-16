# ========================================================================
#
# The Relationship Between Mental Representations of Welfare Recipients and 
# Attitudes Toward Welfare
#
# Brown-Ianuzzi, Dotsch, Cooley, & Payne (in press)
#
# Script to compute Classification Images for Study 1
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
welfaredata <- read.csv('../Data/Sample1_DATA_long.csv')

# Recode responses
# 1 = Positive stim
# 0 = Negative stim
welfaredata$response <- (welfaredata$response * -2) + 1

# Indicate repeated trials (pic > 400)
welfaredata <- subset(welfaredata, pic <= 400)
welfareCIs <- batchGenerateCI2IFC(welfaredata, 'SubjectID', 'pic', 'response', 'pat3', './Conversion from Python to R/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata')
welfareantiCIs <- batchGenerateCI2IFC(welfaredata, 'SubjectID', 'pic', 'response', 'pat3', './Conversion from Python to R/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', antiCI = T)
names(welfareantiCIs) <- paste0(names(welfareantiCIs), "_anti")

# Compute CIs with constant scaling (using autoscale)
allCIs <- c(welfareCIs, welfareantiCIs)
allCIs <- allCIs[!names(allCIs) %in% c('autoscaling.constant', 'autoscaling.constant_anti')]
allCIs <- autoscale(allCIs, saveasjpegs = T)

# Group CIs
groupCIs <- list(
  welfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './Conversion from Python to R/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata'),
  antiwelfare=generateCI2IFC(welfaredata$pic, welfaredata$response, 'pat3', './Conversion from Python to R/kp1_seed_NA_time_Jan_22_2015_12_24.Rdata', antiCI = T)
)

groupCIs <- autoscale(groupCIs, saveasjpegs = T)

rm(allCIs, welfareCIs, welfareantiCIs)
save.image('Study 1.Rdata')
