#=============================================================#
# Project 3
# Import, explore, clean data
# Rachel Johnson
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(dplyr)
library(plyr)
library(tidyr)

mci <- read.csv("C:/Users/johnsra3/Documents/School/AdvancedData/Project3Data.csv", header = T)


#=============================================================#
# Add number of visits column 
#=============================================================#

freq <- as.data.frame(table(mci$id))
colnames(freq) <- c("id", "numobs")
mci <- merge(mci, freq, by = "id")


#=============================================================#
# Length of follow-up column 
#=============================================================#

test <- ddply(mci, .(id), function(x) x[c(1, nrow(x)), ])
test <- test[, c(which(colnames(test) == "id"),
                 which(colnames(test) == "age"))]
test$whichvisit <- rep(c(1, 2), times = nrow(test)/2)
test_wide <- spread(test, whichvisit, age)
test_wide$followup <- test_wide$`2` - test_wide$`1`

mci <- merge(test_wide, mci, by = "id")


#=============================================================#
# Create new variable stdized around minimum age
#=============================================================#

mci$age_59 <- mci$age - 59


#=============================================================#
# Remove obs missing all outcomes, then
# remove individuals who don't have at least 3 time points-
# DO THIS FOR EACH OUTCOME!
#=============================================================#

allmissing <- mci[is.na(mci$blockR) == T & is.na(mci$animals) == T & is.na(mci$logmemI) == T & is.na(mci$logmemII) == T, ]
mci <- mci[!rownames(mci) %in% rownames(allmissing), ]

#Get each outcome into a separate data set
blockr <- mci[, c(1, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16)]
animals <- mci[, c(1, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16)]
logmem1 <- mci[, c(1, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16)]
logmem2 <- mci[, c(1, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16)]

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

#Only include indivs w/ 3+ visits
blockr <- blockr[blockr$num_obs > 2, ]
animals <- animals[animals$num_obs > 2, ]
logmem1 <- logmem1[logmem1$num_obs > 2, ]
logmem2 <- logmem2[logmem2$num_obs > 2, ]

#Add variable for time b/t age and onset age
blockr$timeb4dem <- ifelse(blockr$demind == 1,
                           blockr$age - blockr$ageonset, 0)
animals$timeb4dem <- ifelse(animals$demind == 1,
                            animals$age - animals$ageonset, 0)
logmem1$timeb4dem <- ifelse(logmem1$demind == 1,
                            logmem1$age - logmem1$ageonset, 0)
logmem2$timeb4dem <- ifelse(logmem2$demind == 1,
                            logmem2$age - logmem2$ageonset, 0)

#=============================================================#
# Write csvs of outcome data sets
#=============================================================#

setwd("~/School/AdvancedData")
write.csv(mci, "MCICleaned.csv")
write.csv(blockr, "BlockROutcome.csv")
write.csv(animals, "AnimalsOutcome.csv")
write.csv(logmem1, "LogMem1Outcome.csv")
write.csv(logmem2, "LogMem2Outcome.csv")
