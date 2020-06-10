source('shark.R')
source('simulate.R')
source('out.R')
source('protected.R')
source('unprotected.R')

set.seed(1234)

Nreefs <- 30
reefX <- runif(Nreefs, 0, 25)
reefY <- runif(Nreefs, 0, 25)
reefLoc <- data.frame(x = reefX, y = reefY)

reefDist <- as.matrix(dist(reefLoc))

reefSuitability <- rbeta(Nreefs, 0.03, 0.03)

initialPos <- sample(1:Nreefs, 1, replace = T)

protection = seq(0, 1, len = 10)
fishing = seq(0.1, 10, len = 10)

scenarios <- expand.grid(protection = protection, fishing = fishing)

library(doParallel)
registerDoParallel(cores = 8)

move <- read.csv('Ca-move-samples.csv')
reef <- read.csv('Ca-back-samples.csv')
out <- read.csv('Ca-leave-samples.csv')

shark.population.1 <- foreach(i = 1:nrow(scenarios)) %dopar% {
      
      protReefs <- sample(1:Nreefs, scenarios$protection[i]*Nreefs, prob = reefSuitability, replace = F)
      
      reefStatus <-  rep('unprotected', Nreefs)
      reefStatus[protReefs] <- 'protected'
      
      s <- foreach(j = 1:1000) %do% {
            
            initialPos <- sample(1:Nreefs, 1, replace = T)
            
            id.move <- sample(1:3000, 1)
            id.reef <- sample(1:3000, 1)
            id.out <- sample(1:3000, 1)
            
            sharkStatus <- reefStatus[initialPos]  
            
            s1 <- shark(list(
                  currentReef = initialPos, reefX = reefX, reefY = reefY, 
                  reefStatus = reefStatus, protReefs = protReefs, reefSuitability = reefSuitability,#reef attributes
                  a.move = move$a[id.move], b.move = move$b[id.move], c.move = NA,
                  a.reef = reef$a[id.reef], b.reef = reef$b[id.reef], c.reef = reef$c[id.reef],
                  a.out = out$a[id.out], b.out = out$b[id.out], c.out = NA,
                  DistShape = 4.31353, DistRate = 0.3077521, ###Modificar estos parámetros de acuerdo con la especie
                  fishRate = 0.000359905, fishWeight = scenarios$fishing[i],#status attributes
                  deathRate = 1/(6 * 365),
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

saveRDS(shark.population.1, "C-a-popn-1.rds")

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

saveRDS(sim.1, "C-a-sim-popn-1.rds")

rm(shark.population.1)
rm(sim.1)

gc(reset = T)

#protecting less suitable reefs

shark.population.2 <- foreach(i = 1:nrow(scenarios)) %dopar% {
      
      if(scenarios$protection[i] < 0.7777778){ suit <- 1 - reefSuitability }else{ suit <- 1 / reefSuitability }
      protReefs <- sample(1:Nreefs, scenarios$protection[i]*Nreefs, prob = suit, replace = F)
      
      reefStatus <-  rep('unprotected', Nreefs)
      reefStatus[protReefs] <- 'protected'
      
      s <- foreach(j = 1:1000) %do% {
            
            initialPos <- sample(1:Nreefs, 1, replace = T)
            
            id.move <- sample(1:3000, 1)
            id.reef <- sample(1:3000, 1)
            id.out <- sample(1:3000, 1)
            
            sharkStatus <- reefStatus[initialPos]  
            
            s1 <- shark(list(
                  currentReef = initialPos, reefX = reefX, reefY = reefY, 
                  reefStatus = reefStatus, protReefs = protReefs, reefSuitability = reefSuitability,#reef attributes
                  a.move = move$a[id.move], b.move = move$b[id.move], c.move = NA,
                  a.reef = reef$a[id.reef], b.reef = reef$b[id.reef], c.reef = reef$c[id.reef],
                  a.out = out$a[id.out], b.out = out$b[id.out], c.out = NA,
                  DistShape = 4.31353, DistRate = 0.3077521,
                  fishRate = 0.000359905, fishWeight = scenarios$fishing[i],#status attributes
                  deathRate = 1/(6*365),
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

saveRDS(shark.population.2, "C-a-popn-2.rds")

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

saveRDS(sim.2, "C-a-sim-popn-2.rds")

