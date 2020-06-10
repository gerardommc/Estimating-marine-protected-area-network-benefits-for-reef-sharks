
### SILVERTIPS
Linf<-497.9
k<- 0.02
L0<-70.8
t0<- (1/k)*log((Linf-L0)/Linf)
Age <- 0:500
Lt <- Linf*(1-exp(-k*(Age-t0)))


# Instantaneous rate of fishing mortality if you want to test it. 
.F <- 0

# Note that it is age-indepdent as we don't have selectivity info
# for silvertips. My limited sample didn't have any 1 and 2 year olds so you could assume knife edge selectivity at age 3 if that is
# needed


# Chen and Watanabe mortality calculated from growth parameters
tM<--(1/k)*log(1-exp(k*t0))+t0  
a0<-1-exp(-k*(tM-t0))
a1<-k*exp(-k*(tM-t0))
a2<--0.5*k^2*exp(-k*(tM-t0))
CW <-NULL
for(i in 0:500){
  if(i <=tM){cw<-k/(1-exp(-k*(i-t0)))}
  else{cw<-k/(a0+a1*(i-tM)+a2*(i-tM)^2)}
  CW<-cbind(CW,cw)
}

Chen.Want<-as.vector(CW)
Charnov <- k*(Lt/Linf)^-1.5
# convert C&W mortality to annual survivorship
survivorship <- exp(-(Charnov+.F))
Then_hoenig <- 4.899*32^-0.916

# Convert C&W annual survivorship to probability of survivings to age x
Chen.Want.lx<-NULL
for(i in 3:length(survivorship)){
  Chen.Want.lx[1]<-1
  Chen.Want.lx[2]<-survivorship[1]
  Chen.Want.lx[i]<-survivorship[i]^(i-1)
}

# Shows that they can survive to > 300 years old which matches your results and my expectations given the growth parameters
plot(Chen.Want.lx[1:500], ylim = c(0,1), type ="l", col = "blue", lwd = 2)


##### -------------------------------------

# Instead lets use a chen and Yuan (2006) method which uses max age
max.age <- t0+(2.669/k) # gives a max age of 125 years which is still high. 
#However its worth seeing what proportion of the population reach this age

Chen.Yuan<-exp(1.46-1.01*log(max.age))
survivorship <- exp(-(rep(Chen.Yuan,500)+.F))


Chen.Yuan.lx<-NULL
for(i in 3:length(survivorship)){
  Chen.Yuan.lx[1]<-1
  Chen.Yuan.lx[2]<-survivorship[1]
  Chen.Yuan.lx[i]<-survivorship[i]^(i-1)
}

Chen.Yuan.lx[125] # Less than 2% make it to this age which  still gives a high longevity
plot(Chen.Yuan.lx[1:500], ylim = c(0,1),type ="l", col = "red", lwd = 2)


# So lets try the hoenig method (refined by Then et al 2011) using the same max age estimate
Then_hoenig <- 4.899*max.age^-0.916

survivorship <- exp(-(rep(Then_hoenig,500)+.F))


Then_hoenig.lx<-NULL
for(i in 3:length(survivorship)){
  Then_hoenig.lx[1]<-1
  Then_hoenig.lx[2]<-survivorship[1]
  Then_hoenig.lx[i]<-survivorship[i]^(i-1)
}

Then_hoenig.lx[125] # gives a very small proportion of the population surviving to this age

lines(Then_hoenig.lx[1:500], ylim = c(0,1),col = "blue", lwd = 2)

# However only 5% make it to age 53 which is sensible
which.max(Then_hoenig.lx < 0.05)
#50% make it to 13 years old
which.max(Then_hoenig.lx < 0.5)
# and 1% surviving beyond age 80 which is also sensible
which.max(Then_hoenig.lx < 0.01)

## Therefore I'd go with the Then/hoenig method.

####################################################################################################################
######################################################################################################################

### GREY REEF SHARKS ###

Linf <- 229.2 
k<- 0.05 # Bradley et al 2017
L0<-60 
t0<- (1/k)*log((Linf-L0)/Linf)
Age <- 0:500
Lt <- Linf*(1-exp(-k*(Age-t0)))

# Instantaneous rate of fishing mortality if you want to test it. 
.F <- 0

##### -------------------------------------

# Instead lets use a chen and Yuan (2006) method which uses max age
max.age <- t0+(2.669/k) # gives a max age of 47 years 

Chen.Yuan<-exp(1.46-1.01*log(max.age))
survivorship <- exp(-(rep(Chen.Yuan,500)+.F))

Chen.Yuan.lx<-NULL
for(i in 3:length(survivorship)){
  Chen.Yuan.lx[1]<-1
  Chen.Yuan.lx[2]<-survivorship[1]
  Chen.Yuan.lx[i]<-survivorship[i]^(i-1)
}

Chen.Yuan.lx[47] # Less than 2% make it to this age
plot(Chen.Yuan.lx[1:500], ylim = c(0,1),type ="l", col = "red", lwd = 2)

# So lets try the hoenig method (refined by Then et al 2011) using the same max age estimate
Then_hoenig <- 4.899*max.age^-0.916
survivorship <- exp(-(rep(Then_hoenig,500)+.F))

Then_hoenig.lx<-NULL
for(i in 3:length(survivorship)){
  Then_hoenig.lx[1]<-1
  Then_hoenig.lx[2]<-survivorship[1]
  Then_hoenig.lx[i]<-survivorship[i]^(i-1)
}

Then_hoenig.lx[47]*100 # gives a very small proportion of the population surviving to this age

lines(Then_hoenig.lx[1:500], ylim = c(0,1),col = "blue", lwd = 2)

curve(exp(-1/6 * x), 0, 500, add = T)

# However only 5% make it to age 22 which is sensible
which.max(Then_hoenig.lx < 0.05)
# 50% make it to age 5 years old
which.max(Then_hoenig.lx < 0.5)
# and 1% surviving beyond age 34 which is also sensible
which.max(Then_hoenig.lx < 0.01)

## Therefore I'd go with the Then/hoenig method.

####################################################################################################################
######################################################################################################################

### BULL SHARKS

Linf <- 357 
k <- 0.098
L0 <- 56.50 # Natanson et al 2014
t0 <- (1/k)*log((Linf-L0)/Linf)
Age <- 0:500
Lt <- Linf*(1-exp(-k*(Age-t0)))

# Instantaneous rate of fishing mortality if you want to test it. 
.F <- 0

##### -------------------------------------

# Instead lets use a chen and Yuan (2006) method which uses max age
max.age <- t0 + (2.669 / k) # gives a max age of 64 years 

Chen.Yuan<-exp(1.46-1.01*log(max.age))
survivorship <- exp(-(rep(Chen.Yuan,500)+.F))

Chen.Yuan.lx<-NULL
for(i in 3:length(survivorship)){
  Chen.Yuan.lx[1]<-1
  Chen.Yuan.lx[2]<-survivorship[1]
  Chen.Yuan.lx[i]<-survivorship[i]^(i-1)
}

Chen.Yuan.lx[25]*100 # Less than 2% make it to this age
plot(Chen.Yuan.lx[1:500], ylim = c(0,1),type ="l", col = "red", lwd = 2)

# So lets try the hoenig method (refined by Then et al 2011) using the same max age estimate
Then_hoenig <- 4.899*max.age^-0.916
survivorship <- exp(-(rep(Then_hoenig,500)+.F))

Then_hoenig.lx<-NULL
for(i in 3:length(survivorship)){
  Then_hoenig.lx[1]<-1
  Then_hoenig.lx[2]<-survivorship[1]
  Then_hoenig.lx[i]<-survivorship[i]^(i-1)
}

Then_hoenig.lx[25]*100 # gives a very small proportion of the population surviving to this age

lines(Then_hoenig.lx[1:500], ylim = c(0,1),col = "blue", lwd = 2)

curve(exp(-1/4 * x), 0, 500, add = T)

# However only 5% make it to age 13
which.max(Then_hoenig.lx < 0.05)
# 50% make it to age 4
which.max(Then_hoenig.lx < 0.5)
# and 1% surviving beyond age 20 
which.max(Then_hoenig.lx < 0.01)

## Therefore I'd go with the Then/hoenig method.
