# Determining the probability distributions that best fit the time spent on each
# reef by individual sharks. The expectation is that the parameters, or the
# distributions themselves will differ among different species of shark.

#This script was written by Juan Sebastián Vargas in 2017

library(MASS)

# Load data frames
dat <- read.table("Distance-allSpp.txt", header = T, sep = '\t')
# Add an hour where time is 0, to prevent issues when fitting
dat$t_on_reef <- dat$t_on_reef+(dat$t_on_reef == 0)*1/24
bull <- dat[dat$sp=="Carcharhinus leucas",]
silvertip <- dat[dat$sp=="Carcharhinus albimarginatus",]
grayreef <- dat[dat$sp=="Carcharhinus amblyrhynchos",]

# Distance travelled ------------------------------------------------------

dGrayreef <- grayreef[grayreef$dist>0,]$dist
dSilvertip <- silvertip[silvertip$dist>0,]$dist
dBull <- bull[bull$dist>0,]$dist

# Find parameters of gamma distribution that best fit the data, using MASS package

fitDistBull <- fitdistr(dBull, "gamma")
DistShapeBull <- fitDistBull$estimate[1]
DistRateBull <- fitDistBull$estimate[2]
hist(dBull, col = 'gray', prob = T, main = 'Bull shark', xlab = "Distance of reef change (km)")
curve(dgamma(x, shape = DistShapeBull, rate = DistRateBull), col = 'red', add = T)

fitDistSilvertip <- fitdistr(dSilvertip, "gamma")
DistShapeSilvertip <- fitDistSilvertip$estimate[1]
DistRateSilvertip <- fitDistSilvertip$estimate[2]
hist(dSilvertip, col = 'gray', prob = T, main = 'Silvertip shark', xlab = 'Distance of reef change (km)')
curve(dgamma(x, shape = DistShapeSilvertip, rate = DistRateSilvertip), add = T, col = 'red')

fitDistGrayreef <- fitdistr(dGrayreef, "gamma")
DistShapeGrayreef <- fitDistGrayreef$estimate[1]
DistRateGrayreef <- fitDistGrayreef$estimate[2]
hist(dGrayreef, col = 'gray', prob = T, main = 'Gray reef shark', xlab = 'Distance of reef change')
curve(dgamma(x, shape = DistShapeGrayreef, rate = DistRateGrayreef), add = T, col = 'red')

