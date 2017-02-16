# ========================================================================
#
# The Relationship Between Mental Representations of Welfare Recipients and 
# Attitudes Toward Welfare
#
# Brown-Ianuzzi, Dotsch, Cooley, & Payne (in press)
#
# Script to compute masked correlations between Study 1 and 2 CIs
#
# ========================================================================

# Load data
load('../Experiment 1/Analysis/Study 1.Rdata')
cis.study1 <- groupCIs

load('../Experiment 2/Analysis/Study2.RData')
cis.study2 <- cis


# Computed masked correlations
mask <- apply(readJPEG('../Experiment 2/Analysis/mask_fullface.jpg'), MARGIN = c(1,2), FUN=mean) == 1

masked.cis <- data.frame(
  's1.welfare'=as.vector(cis.study1$welfare$ci[mask]),
  's1.antiwelfare'=as.vector(cis.study1$antiwelfare$ci[mask]),
  's2.welfare'=as.vector(cis.study2$welfare[mask]),
  's2.nonwelfare'=as.vector(cis.study2$nonwelfare[mask]),
  's2.antiwelfare'=as.vector(cis.study2$antiwelfare[mask]),
  's2.antinonwelfare'=as.vector(cis.study2$antinonwelfare[mask])
)

c <- corr.test(masked.cis)
print(c$ci)

rm(allCIs, nonwelfareCIs, welfareCIs)
save.image('Study2.RData')
