setwd("~/Dropbox/Simulation model MS/2017-9-1_Base de datos residencia")

library(foreach)

files <- lapply(list.files('by reef freq', pattern = 'csv', full.names = T), function(x){read.csv(x, stringsAsFactors = F)})

db <- files[[1]]; db$tag <- as.factor(db$tag)
inds.cl <- foreach(i = seq_along(levels(db$tag))) %do% {
  ind <- subset(db, tag == levels(tag)[i]) #Splitting data into individuals
  ind$reef[is.na(ind$reef)] <- 'None'
  return(ind)
}

db <- files[[2]]; db$tag <- as.factor(db$tag)
inds.ca <- foreach(i = seq_along(levels(db$tag))) %do% {
  ind <- subset(db, tag == levels(tag)[i]) #Splitting data into individuals
  ind$reef[is.na(ind$reef)] <- 'None'
  return(ind)
}

db <- files[[3]]; db$tag <- as.factor(db$tag)
inds.calb <- foreach(i = seq_along(levels(db$tag))) %do% {
  ind <- subset(db, tag == levels(tag)[i]) #Splitting data into individuals
  ind$reef[is.na(ind$reef)] <- 'None'
  return(ind)
}
  
###########
##Extracting residence times in and out of the reefs
## With this process we'll extract the frequency of leaving the residence reef to go to another reef
## and then the time to leave the reef, and the time they remain out of the reef

inds.cl <- lapply(inds.cl, function(x){
      type <- foreach(i = 1:(nrow(x)-1), .combine = c) %do% {
            if(x$reef[i] == 'None'){
                  'back.reef'
            } else {
                  if(x$reef[i+1] == 'None'){
                        'leave.reef'
                  } else {
                        'move.reef' 
                  }
            }
      }
      type[nrow(x)] <- NA
      x$move.type <- type
      return(x)
})
 
inds.ca <- lapply(inds.ca, function(x){
      type <- foreach(i = 1:(nrow(x)-1), .combine = c) %do% {
            if(x$reef[i] == 'None'){
                  'back.reef'
            } else {
                  if(x$reef[i+1] == 'None'){
                        'leave.reef'
                  } else {
                        'move.reef' 
                  }
            }
      }
      type[nrow(x)] <- NA
      x$move.type <- type
      return(x)
})

inds.calb <- lapply(inds.calb, function(x){
      type <- foreach(i = 1:(nrow(x)-1), .combine = c) %do% {
            if(x$reef[i] == 'None'){
                  'back.reef'
            } else {
                  if(x$reef[i+1] == 'None'){
                        'leave.reef'
                  } else {
                        'move.reef' 
                  }
            }
      }
      type[nrow(x)] <- NA
      x$move.type <- type
      return(x)
})

##################
##Splitting data into residence times

cl.back.reef <- lapply(inds.cl, function(x){subset(x, move.type == 'back.reef')})
cl.leave.reef <- lapply(inds.cl, function(x){subset(x, move.type == 'leave.reef')})
cl.move.reef <- lapply(inds.cl, function(x){subset(x, move.type == 'move.reef')})

ca.back.reef <- lapply(inds.ca, function(x){subset(x, move.type == 'back.reef')})
ca.leave.reef <- lapply(inds.ca, function(x){subset(x, move.type == 'leave.reef')})
ca.move.reef <- lapply(inds.ca, function(x){subset(x, move.type == 'move.reef')})

calb.back.reef <- lapply(inds.calb, function(x){subset(x, move.type == 'back.reef')})
calb.leave.reef <- lapply(inds.calb, function(x){subset(x, move.type == 'leave.reef')})
calb.move.reef <- lapply(inds.calb, function(x){subset(x, move.type == 'move.reef')})

saveRDS(list(cl.back.reef, cl.leave.reef, cl.move.reef,
             ca.back.reef, ca.leave.reef, ca.move.reef,
             calb.back.reef, calb.leave.reef, calb.move.reef), 'by reef freq/All residency data.rds')


for(i in seq_along(cl.back.reef)) write.csv(cl.back.reef[[i]], paste0('by reef freq/Cl/back to reef/Cl-tag-',cl.back.reef[[i]]$tag[1], '.csv'))
for(i in seq_along(cl.leave.reef)) write.csv(cl.leave.reef[[i]], paste0('by reef freq/Cl/leave reef/Cl-tag-',cl.leave.reef[[i]]$tag[1], '.csv'))
for(i in seq_along(cl.move.reef)) write.csv(cl.move.reef[[i]], paste0('by reef freq/Cl/move reef/Cl-tag-',cl.move.reef[[i]]$tag[1], '.csv'))

for(i in seq_along(ca.back.reef)) write.csv(ca.back.reef[[i]], paste0('by reef freq/Ca/back to reef/Ca-tag-',ca.back.reef[[i]]$tag[1], '.csv'))
for(i in seq_along(ca.leave.reef)) write.csv(ca.leave.reef[[i]], paste0('by reef freq/Ca/leave reef/Ca-tag-',ca.leave.reef[[i]]$tag[1], '.csv'))
for(i in seq_along(ca.move.reef)) write.csv(ca.move.reef[[i]], paste0('by reef freq/Ca/move reef/Ca-tag-',ca.move.reef[[i]]$tag[1], '.csv'))

for(i in seq_along(calb.back.reef)) write.csv(calb.back.reef[[i]], paste0('by reef freq/Calb/back to reef/Calb-tag-',calb.back.reef[[i]]$tag[1], '.csv'))
for(i in seq_along(calb.leave.reef)) write.csv(calb.leave.reef[[i]], paste0('by reef freq/Calb/leave reef/Calb-tag-',calb.leave.reef[[i]]$tag[1], '.csv'))
for(i in seq_along(calb.move.reef)) write.csv(calb.move.reef[[i]], paste0('by reef freq/Calb/move reef/Calb-tag-',calb.move.reef[[i]]$tag[1], '.csv'))


