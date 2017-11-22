#==============================================================#
# Project 3
# Work with bootstrap results
# Rachel Johnson
#==============================================================#

#==============================================================#
# Import data
#==============================================================#

setwd("C:/Users/johnsra3/Documents/School/AdvancedData")
blockr_bs <- read.csv("Bootstraps_blockR.csv", header = T)
blockr_bs <- blockr_bs[, -1]
animals_bs <- read.csv("Bootstraps_animals.csv", header = T)
animals_bs <- animals_bs[, -1]
logmem1_bs <- read.csv("Bootstraps_logmem1.csv", header = T)
logmem1_bs <- logmem1_bs[, -1]
logmem2_bs <- read.csv("Bootstraps_logmem2.csv", header = T)
logmem2_bs <- logmem2_bs[, -1]

#==============================================================#
# Explore bootstrap results
#==============================================================#

mean(blockr_bs[, 1])
sd(blockr_bs[, 1])
quantile(blockr_bs[, 1], c(0.025, 0.975))

mean(animals_bs[, 1])
sd(animals_bs[, 1])
quantile(animals_bs[, 1], c(0.025, 0.975))

mean(logmem1_bs[, 1])
sd(logmem1_bs[, 1])
quantile(logmem1_bs[, 1], c(0.025, 0.975))

mean(logmem2_bs[, 1])
sd(logmem2_bs[, 1])
quantile(logmem2_bs[, 1], c(0.025, 0.975))


#==============================================================#
# Table of bootstrap results 
#==============================================================#

bstab <- as.data.frame(matrix(data = NA, nrow = 4, ncol = 4))
colnames(bstab) <- c("Outcome", "Change Point (likelihood)", "Change point (bootstrap)",
                     "Bootstrapped 95% CI")

bstab[1:4, 1] <- c("Block R", "Animals", "LogMem1", "LogMem2")
bstab[1:4, 2] <- c(-3.9, -3.9, -2.8, -3.0)

bstab[1, 3] <- round(mean(blockr_bs[, 1]), 2)  
bstab[2, 3] <- round(mean(animals_bs[, 1]), 2)  
bstab[3, 3] <- round(mean(logmem1_bs[, 1]), 2)  
bstab[4, 3] <- round(mean(logmem2_bs[, 1]), 2)  

bstab[1, 4] <- paste(paste(round(quantile(blockr_bs[, 1], 0.025), 2), ",", sep = ""),
                     round(quantile(blockr_bs[, 1], 0.975), 2))
bstab[2, 4] <- paste(paste(round(quantile(animals_bs[, 1], 0.025), 2), ",", sep = ""),
                     round(quantile(animals_bs[, 1], 0.975), 2))
bstab[3, 4] <- paste(paste(round(quantile(logmem1_bs[, 1], 0.025), 2), ",", sep = ""),
                     round(quantile(logmem1_bs[, 1], 0.975), 2))
bstab[4, 4] <- paste(paste(round(quantile(logmem2_bs[, 1], 0.025), 2), ",", sep = ""),
                     round(quantile(logmem2_bs[, 1], 0.975), 2))
bstab

write.csv(bstab, "C:/Repositories/bios6623-johnsra3/Project3/Reports/BootstrapSummaryTable.csv")
