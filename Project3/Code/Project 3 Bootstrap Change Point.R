#=============================================================#
# Project 3
# Bootstrap change points
# Rachel Johnson
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(multcomp)
library(dplyr)
source("C:/Repositories/bios6623-johnsra3/Project3/Code/Functions- Change Points and Bootstrap.R")

setwd("C:/Users/johnsra3/Documents/School/AdvancedData")
blockr <- read.csv("blockrOutcome.csv", header = T)
animals <- read.csv("AnimalsOutcome.csv", header = T)
logmem1 <- read.csv("LogMem1Outcome.csv", header = T)
logmem2 <- read.csv("LogMem2Outcome.csv", header = T)


#=============================================================#
# Bootstrap for animals- ONLY THIS OUTCOME!!!
#=============================================================#

vars <- c("id", "gender", "SES", "age", "animals", "demind", "timeb4dem")
animals <- animals[, colnames(animals) %in% vars]
colnames(animals)[which(colnames(animals) == "animals")] <- "y"

niter <- 1000
bootstraps_animal <- matrix(NA, ncol = 7, nrow = niter)
cps <- seq(from = -6, to = 2, by = 0.1)
for (j in 1:niter){
  bootstraps_animal[j, ] <- boot.function(ids = animals$id, dat = animals, cps = cps)
  print(j)
}


write.csv(bootstraps_animal, "Bootstraps_animals.csv")

#=============================================================#
# Bootstrap for blockR
#=============================================================#

colnames(blockr)[which(colnames(blockr) == "blockR")] <- "y"

niter <- 1000
bootstraps <- matrix(NA, ncol = 7, nrow = niter)
cps <- seq(from = -15.1, to = -0.1, by = 0.1)
for (j in 1:niter){
  bootstraps[j, ] <- boot.function(ids = blockr$id, dat = blockr, cps = cps)
  print(j)
}

write.csv(bootstraps, "Bootstraps_blockR.csv")

hist(bootstraps[,1], xlab="Change Point", main='Bootstrap Distribution of Change Points', breaks = 30)
mean(bootstraps[,1])
sd(bootstraps[,1])
quantile(bootstraps[,1], c(0.025, 0.975))
#whoa--these seem kind of weird and the bootstrapped mean is -4.22, wh/ is a little far off



#=============================================================#
# Bootstrap for logmem1
#=============================================================#

vars <- c("id", "gender", "SES", "age", "logmemI", "demind", "timeb4dem")
logmem1 <- logmem1[, colnames(logmem1) %in% vars]
colnames(logmem1)[which(colnames(logmem1) == "logmemI")] <- "y"

niter <- 1000
bootstraps_logmem1 <- matrix(NA, ncol = 7, nrow = niter)
cps <- seq(from = -15.1, to = -0.1, by = 0.1)
for (j in 1:niter){
  bootstraps_logmem1[j, ] <- boot.function(ids = logmem1$id, dat = logmem1, cps = cps)
  print(j)
}

write.csv(bootstraps_logmem1, "Bootstraps_logmem1.csv")


#=============================================================#
# Bootstrap for logmem2
#=============================================================#

vars <- c("id", "gender", "SES", "age", "logmemII", "demind", "timeb4dem")
logmem2 <- logmem2[, colnames(logmem2) %in% vars]
colnames(logmem2)[which(colnames(logmem2) == "logmemII")] <- "y"

niter <- 1000
bootstraps_logmem2 <- matrix(NA, ncol = 7, nrow = niter)
cps <- seq(from = -15.1, to = -0.1, by = 0.1)
for (j in 1:niter){
  bootstraps_logmem2[j, ] <- boot.function(ids = logmem2$id, dat = logmem2, cps = cps)
  print(j)
}
  
write.csv(bootstraps_logmem2, "Bootstraps_logmem2.csv")
