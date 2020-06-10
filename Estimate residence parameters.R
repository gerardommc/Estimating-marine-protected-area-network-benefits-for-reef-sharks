library(survival)
library(rlist)
library(data.table)

#Carcharhinus amblyrhynchos

#First reading the data

#Moving to another reef
ca.mov <- lapply(list.files('Residence times data/C amblyrhynchos/move reef/', 'csv', full.names = T), read.csv)
ca.mov <- ca.mov[sapply(ca.mov, nrow) > 0]
#Going out of reefs
ca.leave <- lapply(list.files('Residence times data/C amblyrhynchos/leave reef/', 'csv', full.names = T), read.csv)
ca.leave <- ca.leave[sapply(ca.leave, nrow) > 0]
#Moving back to reefs
ca.back <- lapply(list.files('Residence times data/C amblyrhynchos/back to reef/', 'csv', full.names = T), read.csv)
ca.back <- ca.back[sapply(ca.back, nrow) > 0]

#Formatting it as a Surv object

ca.mov.df <- data.frame(rbindlist(ca.mov))
ca.mov.df$event <- 1
ca.leave.df <- data.frame(rbindlist(ca.leave))
ca.leave.df$event <- 1
ca.back.df <- data.frame(rbindlist(ca.back))
ca.back.df$event <- 1

ca.mov.times <- with(ca.mov.df, Surv(acum, event, type = "right"))
ca.leave.times <- with(ca.leave.df, Surv(acum, event, type = "right")) 
ca.back.times <- with(ca.back.df, Surv(acum, event, type = "right"))

#Doing the regression

ca.mov.m <- survfit(ca.mov.times ~ 1)
ca.leave.m <- survfit(ca.leave.times ~ 1)
ca.back.m <- survfit(ca.back.times~1)

ca.mov.surv <- data.frame(list.cbind(summary(ca.mov.m)[c("time", "surv", "lower", "upper")]))
ca.leave.surv <- data.frame(list.cbind(summary(ca.leave.m)[c("time", "surv", "lower", "upper")]))
ca.back.surv <- data.frame(list.cbind(summary(ca.back.m)[c("time", "surv", "lower", "upper")]))

#Extracting the probability of surviving up to that point in time
ca.mov.surv$rho <- with(ca.mov.surv, - log(surv)/time)
ca.mov.surv$rho.up <- with(ca.mov.surv, - log(upper)/time)
ca.mov.surv$rho.lo <- with(ca.mov.surv, - log(lower)/time)
ca.mov.surv$type <- "Move"

ca.leave.surv$rho <- with(ca.leave.surv, -log(surv)/time)
ca.leave.surv$rho.up <- with(ca.leave.surv, -log(upper)/time)
ca.leave.surv$rho.lo <- with(ca.leave.surv, -log(lower)/time)
ca.leave.surv$type <- "Leave"

ca.back.surv$rho <- with(ca.back.surv, -log(surv)/time)
ca.back.surv$rho.up <- with(ca.back.surv, -log(upper)/time)
ca.back.surv$rho.lo <- with(ca.back.surv, -log(lower)/time)
ca.back.surv$type <- "Back"


#############################
#####Carcharhinus leucas#####

cl.mov <- lapply(list.files('Residence times data/C leucas/move reef/', 'csv', full.names = T), read.csv)
cl.mov <- cl.mov[sapply(cl.mov, nrow) > 0]
#Going out of reefs
cl.leave <- lapply(list.files('Residence times data/C leucas/leave reef/', 'csv', full.names = T), read.csv)
cl.leave <- cl.leave[sapply(cl.leave, nrow) > 0]
#Moving back to reefs
cl.back <- lapply(list.files('Residence times data/C leucas/back to reef/', 'csv', full.names = T), read.csv)
cl.back <- cl.back[sapply(cl.back, nrow) > 0]

#Formatting it as a Surv object

cl.mov.df <- data.frame(rbindlist(cl.mov))
cl.mov.df$event <- 1
cl.leave.df <- data.frame(rbindlist(cl.leave))
cl.leave.df$event <- 1
cl.back.df <- data.frame(rbindlist(cl.back))
cl.back.df$event <- 1

cl.mov.times <- with(cl.mov.df, Surv(acum, event, type = "right"))
cl.leave.times <- with(cl.leave.df, Surv(acum, event, type = "right"))
cl.back.times <- with(cl.back.df, Surv(acum, event, type = "right"))

#Doing the regression

cl.mov.m <- survfit(cl.mov.times ~ 1)
cl.leave.m <- survfit(cl.leave.times ~ 1)
cl.back.m <- survfit(cl.back.times~1)

cl.mov.surv <- data.frame(list.cbind(summary(cl.mov.m)[c("time", "surv", "lower", "upper")]))
cl.leave.surv <- data.frame(list.cbind(summary(cl.leave.m)[c("time", "surv", "lower", "upper")]))
cl.back.surv <- data.frame(list.cbind(summary(cl.back.m)[c("time", "surv", "lower", "upper")]))

#Extracting the probability of surviving up to that point in time
cl.mov.surv$rho <- with(cl.mov.surv, -log(surv)/time)
cl.mov.surv$rho.up <- with(cl.mov.surv, -log(upper)/time)
cl.mov.surv$rho.lo <- with(cl.mov.surv, -log(lower)/time)
cl.mov.surv$type <- "Move"

cl.leave.surv$rho <- with(cl.leave.surv, -log(surv)/time)
cl.leave.surv$rho.up <- with(cl.leave.surv, -log(upper)/time)
cl.leave.surv$rho.lo <- with(cl.leave.surv, -log(lower)/time)
cl.leave.surv$type <- "Leave"


cl.back.surv$rho <- with(cl.back.surv, -log(surv)/time)
cl.back.surv$rho.up <- with(cl.back.surv, -log(upper)/time)
cl.back.surv$rho.lo <- with(cl.back.surv, -log(lower)/time)
cl.back.surv$type <- "Back"

#####################################
#####Carcharhinus albimarginatus#####

calb.mov <- lapply(list.files('Residence times data/C albimarginatus/move reef/', 'csv', full.names = T), read.csv)
calb.mov <- calb.mov[sapply(calb.mov, nrow) > 0]
#Going out of reefs
calb.leave <- lapply(list.files('Residence times data/C albimarginatus/leave reef/', 'csv', full.names = T), read.csv)
calb.leave <- calb.leave[sapply(calb.leave, nrow) > 0]
#Moving back to reefs
calb.back <- lapply(list.files('Residence times data/C albimarginatus/back to reef/', 'csv', full.names = T), read.csv)
calb.back <- calb.back[sapply(calb.back, nrow) > 0]

#Formatting it as a Surv object

calb.mov.df <- data.frame(rbindlist(calb.mov))
calb.mov.df$event <- 1
calb.leave.df <- data.frame(rbindlist(calb.leave))
calb.leave.df$event <- 1
calb.back.df <- data.frame(rbindlist(calb.back))
calb.back.df$event <- 1

calb.mov.times <- with(calb.mov.df, Surv(acum, event, type = "right"))
calb.leave.times <- with(calb.leave.df, Surv(acum, event, type = "right"))
calb.back.times <- with(calb.back.df, Surv(acum, event, type = "right"))

#Doing the regression

calb.mov.m <- survfit(calb.mov.times ~ 1)
calb.leave.m <- survfit(calb.leave.times ~ 1)
calb.back.m <- survfit(calb.back.times~1)

calb.mov.surv <- data.frame(list.cbind(summary(calb.mov.m)[c("time", "surv", "lower", "upper")]))
calb.leave.surv <- data.frame(list.cbind(summary(calb.leave.m)[c("time", "surv", "lower", "upper")]))
calb.back.surv <- data.frame(list.cbind(summary(calb.back.m)[c("time", "surv", "lower", "upper")]))

#Extracting the probability of surviving up to that point in time
calb.mov.surv$rho <- with(calb.mov.surv, -log(surv)/time)
calb.mov.surv$rho.up <- with(calb.mov.surv, -log(upper)/time)
calb.mov.surv$rho.lo <- with(calb.mov.surv, -log(lower)/time)
calb.mov.surv$type <- "Move"

calb.leave.surv$rho <- with(calb.leave.surv, -log(surv)/time)
calb.leave.surv$rho.up <- with(calb.leave.surv, -log(upper)/time)
calb.leave.surv$rho.lo <- with(calb.leave.surv, -log(lower)/time)
calb.leave.surv$type <- "Leave"

calb.back.surv$rho <- with(calb.back.surv, -log(surv)/time)
calb.back.surv$rho.up <- with(calb.back.surv, -log(upper)/time)
calb.back.surv$rho.lo <- with(calb.back.surv, -log(lower)/time)
calb.back.surv$type <- "Back"

### Plots of the probabilities

library(ggplot2)

ca.surv <- rbind(ca.mov.surv, ca.leave.surv, ca.back.surv)
ca.surv$species <- "C. amblyrhynchos"

cl.surv <- rbind(cl.mov.surv, cl.leave.surv, cl.back.surv)
cl.surv$species <- "C. leucas"

calb.surv <- rbind(calb.mov.surv, calb.leave.surv, calb.back.surv)
calb.surv$species <- "C. albimarginatus"

shark.surv <- rbind(ca.surv, cl.surv, calb.surv)
shark.surv <- na.omit(shark.surv)

png("Rho-time.png", width = 1200, height = 500)
ggplot(shark.surv) + geom_line(aes(x = log10(time), y = rho, colour = species), size = 1) +
      geom_ribbon(aes(x = log10(time), ymin = rho.up, ymax = rho.lo, fill = species), alpha = 0.3) +
      facet_wrap(facets = "type")+
      theme_minimal() +
      labs(x = expression(log[10](time)), y = expression(italic(rho(t))),
           colour = "Species", fill = "Species") +
      theme(axis.text = element_text(size = 16),
            title = element_text(size = 24),
            legend.key.height = unit(12, units = "mm"),
            legend.key.width = unit(12, units = "mm"), 
            legend.text =  element_text(size = 18, face = "italic"),
            strip.text.x = element_text(size = 24))
dev.off()
      
png("Curves.png", width = 1200, height = 500)
ggplot(shark.surv) + geom_line(aes(x = log10(time), y = surv, colour = species), size = 1) +
      geom_ribbon(aes(x = log10(time), ymin = lower, ymax = upper, fill = species), alpha = 0.3) +
      facet_wrap(facets = "type") +
      labs(colour = "Species", fill = "Species", x = expression(log[10](time)), y = "Proportion")+
      theme_minimal() +
      theme(axis.text = element_text(size = 16),
            title = element_text(size = 24),
            legend.key.height = unit(12, units = "mm"),
            legend.key.width = unit(12, units = "mm"), 
            legend.text =  element_text(size = 18, face = "italic"),
            strip.text.x = element_text(size = 24))
dev.off()

write.csv(ca.surv, "C-ambly-residence-probs.csv")
write.csv(cl.surv, "C-leucas-residence-probs.csv")
write.csv(calb.surv, "C-albi-residence-probs.csv")
