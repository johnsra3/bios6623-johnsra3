#=============================================================#
# Project 3
# Import, explore, clean data
# Rachel Johnson
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(dplyr)
library(ggplot2)
mci <- read.csv("C:/Users/johnsra3/Documents/School/AdvancedData/Project3Data.csv", header = T)


#=============================================================#
# Remove rows that are completely NA for outcomes
#=============================================================#

allmissing <- mci[is.na(mci$blockR) == T & is.na(mci$animals) == T & is.na(mci$logmemI) == T & is.na(mci$logmemII) == T, ]
mci <- mci[!rownames(mci) %in% rownames(allmissing), ]


#=============================================================#
# Remove individuals who don't have at least 3 time points-
# DO THIS FOR EACH OUTCOME!
#=============================================================#

#Get each outcome into a separate data set
blockr <- mci[, c(1, 2, 3, 4, 5, 6, 10, 11)]
animals <- mci[, c(1, 2, 3, 4, 5, 7, 10, 11)]
logmem1 <- mci[, c(1, 2, 3, 4, 5, 8, 10, 11)]
logmem2 <- mci[, c(1, 2, 3, 4, 5, 9, 10, 11)]

#Remove missing obs for each outcome
blockr <- blockr[is.na(blockr$blockR) == F, ]
animals <- animals[is.na(animals$animals) == F, ]
logmem1 <- logmem1[is.na(logmem1$logmemI) == F, ]
logmem2 <- logmem2[is.na(logmem2$logmemII) == F, ]

#Tables of frequencies for each ID
blockr_rows <- as.data.frame(table(blockr$id))
animals_rows <- as.data.frame(table(animals$id))
logmem1_rows <- as.data.frame(table(logmem1$id))
logmem2_rows <- as.data.frame(table(logmem2$id))

colnames(blockr_rows) <- c("id", "num_obs")
colnames(animals_rows) <- c("id", "num_obs")
colnames(logmem1_rows) <- c("id", "num_obs")
colnames(logmem2_rows) <- c("id", "num_obs")

blockr <- merge(blockr, blockr_rows, by = "id")
animals <- merge(animals, animals_rows, by = "id")
logmem1 <- merge(logmem1, logmem1_rows, by = "id")
logmem2 <- merge(logmem2, logmem2_rows, by = "id")


#=============================================================#
# Explore data- blockR
#=============================================================#

summary(blockr$blockR)
hist(blockr$blockR)

summary(animals$animals)
hist(animals$animals)

summary(logmem1$logmemI)
hist(logmem1$logmemI)

summary(logmem2$logmemII)
hist(logmem2$logmemII)


#=============================================================#
# Spaghetti plots- exploration of trajectory
#=============================================================#

#blockr
ggplot(data = blockr, aes(x = age, y = blockR, group = demind, col = as.factor(demind))) +
  geom_line()

#animals
ggplot(data = animals, aes(x = age, y = animals, group = demind, col = as.factor(demind))) +
  geom_line()

#logmem1
ggplot(data = logmem1, aes(x = age, y = logmemI, group = demind, col = as.factor(demind))) +
  geom_line()

#logmem2
ggplot(data = logmem2, aes(x = age, y = logmemII, group = demind, col = as.factor(demind))) +
  geom_line()


#=============================================================#
# Write csvs of outcome data sets
#=============================================================#

setwd("~/School/AdvancedData")
write.csv(blockr, "BlockROutcome.csv")
write.csv(animals, "AnimalsOutcome.csv")
write.csv(logmem1, "LogMem1Outcome.csv")
write.csv(logmem2, "LogMem2Outcome.csv")
