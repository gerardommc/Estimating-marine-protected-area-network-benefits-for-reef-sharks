# Determining the probability distributions that best fit the time spent on each
# reef by individual sharks. The expectation is that the parameters, or the
# distributions themselves will differ among different species of shark.

library(MASS)

# Load data frames
dat <- read.table("fittingDataAllSpp.txt", header = T, sep = '\t')
# Add an hour where time is 0, to prevent issues when fitting
dat$t_on_reef <- dat$t_on_reef+(dat$t_on_reef == 0)*1/24
bull <- dat[dat$sp=="Carcharhinus leucas",]
silvertip <- dat[dat$sp=="Carcharhinus albimarginatus",]
grayreef <- dat[dat$sp=="Carcharhinus amblyrhynchos",]

par(mfrow=c(3,1))

# Residency time ----------------------------------------------------------

tGrayreef <- grayreef$t_on_reef
tSilvertip <- silvertip$t_on_reef
tBull <- bull$t_on_reef

# Find parameters of gamma distribution that best fit the data, using MASS package
fitResBull <- fitdistr(tBull, "gamma")
ResShapeBull <- fitResBull$estimate[1]
ResRateBull <- fitResBull$estimate[2]
hist(tBull, col = 'gray', prob = T, main='Bull shark', xlab = "Reef visit duration (days)")
curve(dgamma(x, shape = ResShapeBull, rate = ResRateBull), col = 'red', add = T)

fitResSilvertip <- fitdistr(tSilvertip, "gamma")
ResShapeSilvertip <- fitResSilvertip$estimate[1]
ResRateSilvertip <- fitResSilvertip$estimate[2]
hist(tSilvertip, col = 'gray', prob = T, main = 'Silvertip shark', xlab = 'Reef visit duration (days)')
curve(dgamma(x, shape = ResShapeSilvertip, rate = ResRateSilvertip), col = 'red', add = T)

fitResGrayreef <- fitdistr(tGrayreef, "gamma")
ResShapeGrayreef <- fitResGrayreef$estimate[1]
ResRateGrayreef <- fitResGrayreef$estimate[2]
hist(tGrayreef, col = 'gray', prob = T, main = 'Gray Reef shark', xlab = 'Reef visit duration (days)')
curve(dgamma(x, shape = ResShapeGrayreef, rate = ResRateGrayreef), col = 'red', add = T)

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

