#The function for sharks out of reefs
simulate.out = function(x){
      
      with(x, {
      
      p.back = back.fun(toffReef)
      p.back = ifelse(is.infinite(p.back), 0, p.back)
      p.back = ifelse(is.na(p.back), runif(1), p.back)      

      p.stay.out = 1 - p.back
      
      p.fish =  fishRate * fishWeight
      
      p.die = p.fish + deathRate
      
      stay.back.or.fish = rmultinom(1,1, c(p.stay.out, p.back, p.die))
      
      if(as.logical(stay.back.or.fish)[3]){ #First we evaluate if it dies
            
            currentDay = day + 1
            sharkStatus = 'dead'
            
            #Return statement here
            
            return(shark(list(currentReef = NA, reefX = NA, reefY = NA, reefStatus = NA, protReefs = NA, reefSuitability = reefSuitability,#reef attributes
                              move.fun = NA,
                              leave.fun = NA,
                              back.fun = NA,
                              freq.move = freq.move,
                              freq.leave = freq.leave,
                              DistShape = NA, DistRate = NA,
                              fishRate = NA, fishWeight = fishWeight, #status attributes
                              deathRate = deathRate, 
                              sharkStatus = sharkStatus,
                              tAlive = tAlive,
                              tonReef = NA,#residence attributes
                              toffReef = NA,
                              Pos = Pos,
                              reefDist = NA, #available habitat attributes
                              new.x = NA, new.y = NA,
                              day = currentDay)))
            
      } else { #Otherwise it lives
            if(as.logical(stay.back.or.fish)[1]){ #If the shark stays out
                  
                  travel.dist = rgamma(1, shape = DistShape, rate = DistRate) #traveling distance
                  angle = runif(1, 0, 2*pi) #direction of the travel, assumed a uniform distribution of turning angles
                  delta.x = cos(angle) * travel.dist # new coordinates
                  delta.y = sin(angle) * travel.dist
                  new.x = new.x + delta.x
                  new.y = new.y + delta.y
                  tAlive = tAlive + 1
                  toffReef = toffReef + 1  #Update time out of reef
                  currentDay = day + 1
                  Pos[currentDay] = 0

                  #Return statement
                  
                  return(shark(list(currentReef = currentReef, reefX = reefX, reefY = reefY, 
                               reefStatus = reefStatus, protReefs = protReefs, reefSuitability = reefSuitability,#reef attributes
                               move.fun = move.fun,
                               leave.fun = leave.fun,
                               back.fun = back.fun,
                               freq.move = freq.move,
                               freq.leave = freq.leave,
                               DistShape = DistShape, DistRate = DistRate,
                               fishRate = fishRate, fishWeight = fishWeight, #status attributes
                               deathRate = deathRate,
                               sharkStatus = sharkStatus,
                               tAlive = tAlive,
                               tonReef = tonReef,#residence attributes
                               toffReef = toffReef,
                               Pos = Pos,
                               reefDist = reefDist, #available habitat attributes
                               new.x = new.x, new.y = new.y,
                               day = currentDay)))

            } else { #Otherwise we find a new reef
                  
                  dist.to.reefs = sqrt((new.x - reefX)^2 + (new.y - reefY)^2) #Begin calculating the distance from current position to all reefs
                  reachProb = pgamma(dist.to.reefs, shape = DistShape, rate = DistRate) * reefSuitability # Calculate probability of going to other reefs
                  newReef = sample(1:Nreefs, 1, prob = reachProb) # Select a reef randomly, weighing with the probabilities of reaching
                  newSharkStatus = reefStatus[newReef]
                  currentDay = day + 1
                  Pos[currentDay] = newReef # update Position
                  tonReef = 1 # update time on reef
                  toffReef = 0
                  tAlive = tAlive + 1
                  
                  #return statement
                  
                  return(shark(list(currentReef = newReef, reefX = reefX, reefY = reefY, 
                               reefStatus = reefStatus, protReefs = protReefs, reefSuitability = reefSuitability,#reef attributes
                               move.fun = move.fun,
                               leave.fun = leave.fun,
                               back.fun = back.fun,
                               freq.move = freq.move,
                               freq.leave = freq.leave,
                               DistShape = DistShape, DistRate = DistRate,
                               fishRate = fishRate, fishWeight = fishWeight, #status attributes
                               deathRate = deathRate,
                               sharkStatus = newSharkStatus,
                               tAlive = tAlive,
                               tonReef = tonReef,#residence attributes
                               toffReef = toffReef,
                               Pos = Pos,
                               reefDist = reefDist, #available habitat attributes
                               new.x = new.x, new.y = new.y,
                               day = currentDay)))
                  
            } 
            
      }
      }) #Closing the with
}
