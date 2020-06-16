source('shark.R')
source('simulate.R')
source('out.R')
source('protected.R')
source('unprotected.R')

rho <- read.csv("C-albi-residence-probs.csv")

set.seed(1234)

Nreefs <- 30
reefX <- runif(Nreefs, 0, 25)
reefY <- runif(Nreefs, 0, 25)
reefLoc <- data.frame(x = reefX, y = reefY)

reefDist <- as.matrix(dist(reefLoc))

reefSuitability <- rbeta(Nreefs, 0.03, 0.03)

protection = seq(0, 1, len = 11)
fishing = seq(0, 10, len = 11)

scenarios <- expand.grid(protection = protection, fishing = fishing)

rho.m <- na.omit(subset(rho, type == "move"))
rho.l <- na.omit(subset(rho, type == "leave"))
rho.b <- na.omit(subset(rho, type == "back"))

move.fun <- with(rho.m, approxfun(x = time, y = rho, method = "linear"))
leave.fun <- with(rho.l, approxfun(x = time, y = rho, method = "linear"))
back.fun <- with(rho.b, approxfun(x = time, y = rho, method = "linear"))

freq.move <- 0.2
freq.leave <- 0.8

library(doParallel)
registerDoParallel(cores = 8)

shark.population.1 <- foreach(i = 1:nrow(scenarios)) %dopar% {
      
      protReefs <- sample(1:Nreefs, scenarios$protection[i]*Nreefs, prob = reefSuitability, replace = F)
      
      reefStatus <-  rep('unprotected', Nreefs)
      reefStatus[protReefs] <- 'protected'
      
      s <- foreach(j = 1:1000) %do% {
                  
            initialPos <- sample(1:Nreefs, 1, replace = T)
 
            sharkStatus <- reefStatus[initialPos]  
            
                  s1 <- shark(list(
                        currentReef = initialPos, reefX = reefX, reefY = reefY, 
                        reefStatus = reefStatus, protReefs = protReefs, reefSuitability = reefSuitability,#reef attributes
                        move.fun = move.fun,
                        leave.fun = leave.fun,
                        back.fun = back.fun,
                        freq.move = freq.move,
                        freq.leave = freq.leave,
                        DistShape = 3.235529, DistRate = 0.2463993,
                        fishRate = 0.000359905, fishWeight = scenarios$fishing[i],#status attributes
                        deathRate = 1/(13 * 365),
                        sharkStatus = sharkStatus,
                        tAlive = 0,
                        tonReef = 0,#residence attributes
                        toffReef = 0,
                        Pos = numeric(),
                        reefDist = reefDist, #available habitat attributes
                        new.x = NA, new.y = NA,
                        day = 1)
                        )
                        
                  return(s1)
          }
      return(s)
}

saveRDS(shark.population.1, "C-alb-popn-1.rds")

sim.1 <- foreach(i = seq_along(shark.population.1)) %dopar% {
      scen <- foreach(j = seq_along(shark.population.1[[i]]), .combine = rbind) %do% {
            s <- shark.population.1[[i]][[j]]
            status <- s$sharkStatus
            while(s$sharkStatus != 'dead'){
                  s <- simulate(s)
                  status <- c(status, s$sharkStatus)
                  age <- s$day
            }
            p.prot <- length(which(status == 'protected'))/length(status)
            return(c(p.prot = p.prot, age = age, 
                     protection = scenarios[i,1], fishing = scenarios[i,2]))
      }
}

saveRDS(sim.1, "C-alb-sim-popn-1.rds")

rm(shark.population.1)
rm(sim.1)

gc(reset = T)

shark.population.2 <- foreach(i = 1:nrow(scenarios)) %dopar% {
      
      if(scenarios$protection[i] < 0.7777778){ suit <- 1 - reefSuitability }else{ suit <- 1 / reefSuitability }
      protReefs <- sample(1:Nreefs, scenarios$protection[i]*Nreefs, prob = suit, replace = F)
      
      reefStatus <-  rep('unprotected', Nreefs)
      reefStatus[protReefs] <- 'protected'
      
      s <- foreach(j = 1:1000) %do% {
            
            initialPos <- sample(1:Nreefs, 1, replace = T)
            
            id.move <- sample(1:3000, 1)
            id.leave <- sample(1:3000, 1)
            id.back <- sample(1:3000, 1)
            
            sharkStatus <- reefStatus[initialPos]  
            
            s1 <- shark(list(
                  currentReef = initialPos, reefX = reefX, reefY = reefY, 
                  reefStatus = reefStatus, protReefs = protReefs, reefSuitability = reefSuitability,#reef attributes
                  move.fun = move.fun,
                  leave.fun = leave.fun,
                  back.fun = back.fun,
                  freq.move = freq.move,
                  freq.leave = freq.leave,
                  DistShape = 3.235529, DistRate = 0.2463993,
                  fishRate = 0.000359905, fishWeight = scenarios$fishing[i],#status attributes
                  deathRate = 1/(13 * 365),
                  sharkStatus = sharkStatus,
                  tAlive = 0,
                  tonReef = 0,#residence attributes
                  toffReef = 0,
                  Pos = numeric(),
                  reefDist = reefDist, #available habitat attributes
                  new.x = NA, new.y = NA,
                  day = 1)
            )
            
            return(s1)
      }
      return(s)
}

saveRDS(shark.population.2, "C-alb-popn-2.rds")

sim.2 <- foreach(i = seq_along(shark.population.2)) %dopar% {
      scen <- foreach(j = seq_along(shark.population.2[[i]]), .combine = rbind) %do% {
            s <- shark.population.2[[i]][[j]]
            status <- s$sharkStatus
            while(s$sharkStatus != 'dead'){
                  s <- simulate(s)
                  status <- c(status, s$sharkStatus)
                  age <- s$day
            }
            p.prot <- length(which(status == 'protected'))/length(status)
            return(c(p.prot = p.prot, age = age, 
                     protection = scenarios[i,1], fishing = scenarios[i,2]))
      }
}

saveRDS(sim.2, "C-alb-sim-popn-2.rds")
