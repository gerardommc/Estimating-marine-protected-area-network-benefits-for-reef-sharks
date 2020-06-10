#This function will determine what happens when a shak visits a protected reef
simulate.protected = function(x){
      
      with(x, {
      
      p.move = move.fun(tonReef) * freq.move
      p.move = ifelse(is.infinite(p.move), 0, p.move)
      p.move = ifelse(is.na(p.move), runif(1), p.move)      
      
      p.out = leave.fun(tonReef) * freq.leave
      p.out = ifelse(is.infinite(p.out), 0, p.out)
      p.out = ifelse(is.na(p.out), runif(1), p.out)
      
      p.stay = 1 - mean(c(p.move, p.out))

      p.die = deathRate
      
      stay.move.or.die = rmultinom(1, 1, c(p.stay, p.move, p.out, p.die))
      
      if(as.logical(stay.move.or.die)[4]){
            sharkStatus = 'dead' #update status that will be passed out to object class
            currentDay = day + 1
            #Return statement here, an updated copy of 'shark' will be returned with a new class according to the status
            
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
            
      } else { if(as.logical(stay.move.or.die)[1]){ #If the shark stays, these are the subsequent events
                  
                  tAlive = tAlive+1
                  currentDay = day + 1
                  Pos[currentDay] = currentReef
                  tonReef = tonReef + 1
                  
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
                  
            } else {# Otherwise it'll move
                  if(as.logical(stay.move.or.die)[2]){ #This is the conditional for going to anoter reef
                        
                        reachProb = pgamma(reefDist[currentReef,], shape = DistShape, rate = DistRate) * reefSuitability #determine the probability of reaching each reef
                        newReef = sample(1:Nreefs, 1, prob = reachProb)# Select a reef randomly, weighing by the probability of reaching each reef
                        newSharkStatus = reefStatus[newReef]
                        currentDay = day + 1
                        Pos[currentDay] = newReef  #Update the position
                        tonReef = 1 # update time on reef
                        tAlive = tAlive + 1 #add 1 day alive
                        
                        #Return statement here
                        
                        return(shark(list(currentReef = newReef, reefX = reefX, reefY = reefY, 
                                     reefStatus = reefStatus, protReefs = protReefs, reefSuitability = reefSuitability,#reef attributes
                                     move.fun = move.fun,
                                     leave.fun = leave.fun,
                                     back.fun = back.fun,
                                     freq.move = freq.move,
                                     freq.leave = freq.leave,
                                     DistShape = DistShape, DistRate = DistRate,
                                     fishRate = fishRate, fishWeight = fishWeight,#status attributes
                                     deathRate = deathRate,
                                     sharkStatus = newSharkStatus,
                                     tAlive = tAlive,
                                     tonReef = tonReef,#residence attributes
                                     toffReef = toffReef,
                                     Pos = Pos,
                                     reefDist = reefDist, #available habitat attributes
                                     new.x = new.x, new.y = new.y,
                                     day = currentDay)))
                        
                  } else { #The alternative for going out
                        
                        travel.dist = rgamma(1, shape = DistShape, rate = DistRate) #traveling distance
                        angle = runif(1, 0, 2*pi) #direction of the travel
                        delta.x = cos(angle) * travel.dist # Travel distance in X and Y scales
                        delta.y = sin(angle) * travel.dist
                        new.x = reefX[currentReef] + delta.x #New X position
                        new.y = reefY[currentReef] + delta.y #New y position
                        currentDay = day + 1
                        currentReef = 0
                        Pos[currentDay] = currentReef
                        sharkStatus = 'out'
                        tonReef = 0;
                        toffReef = 1 #Add 1 day to the time out of reefs
                        tAlive = tAlive+1 #add 1 day alive
                        
                        #Return statement here
                        
                        return(shark(list(currentReef = currentReef, reefX = reefX, reefY = reefY, 
                                     reefStatus = reefStatus, protReefs = protReefs, reefSuitability = reefSuitability,#reef attributes
                                     move.fun = move.fun,
                                     leave.fun = leave.fun,
                                     back.fun = back.fun,
                                     freq.move = freq.move,
                                     freq.leave = freq.leave,
                                     DistShape = DistShape, DistRate = DistRate,
                                     fishRate = fishRate, fishWeight = fishWeight,#status attributes
                                     deathRate = deathRate,
                                     sharkStatus = sharkStatus,
                                     tAlive = tAlive,
                                     tonReef = tonReef,#residence attributes
                                     toffReef = toffReef,
                                     Pos = Pos,
                                     reefDist = reefDist, #available habitat attributes
                                     new.x = new.x, new.y = new.y,
                                     day = currentDay)))
                        
                  }
            }
      }
      }) #Closing the with environment
}
