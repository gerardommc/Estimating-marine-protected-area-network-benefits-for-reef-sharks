shark <- function(x = list(currentReef = numeric(), reefX = numeric(), reefY = numeric(), protReefs = numeric(),
                           reefSuitability = 1,#reef attributes
                           move.fun = NA,
                           leave.fun = NA,
                           back.fun = NA,
                           freq.move = NA,
                           freq.leave = NA,
                  DistShape = numeric(), DistRate = numeric(),
                  fishRate = numeric(), fishWeight = 1, #status attributes
                  deathRate = numeric(),
                  sharkStatus = character(),
                  tAlive = numeric(),
                  tonReef = numeric(),#residence attributes
                  toffReef = numeric(),
                  Pos = numeric(),
                  reefDist = matrix(),#available habitat attributes
                  new.x = numeric(), new.y = numeric(), #out or reef position attributes
                  day = numeric())#simulation runing length
){
    s <- with(x, 
              list(
                    currentReef = currentReef, reefX = reefX, reefY = reefY, reefStatus = reefStatus, protReefs = protReefs,
                    reefSuitability = reefSuitability,#reef attributes
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
                    day = day
                    )
              ) 
    class(s) <- c('shark', x$sharkStatus)
    return(s)
}
