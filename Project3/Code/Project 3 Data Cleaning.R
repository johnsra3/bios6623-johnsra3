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
library(gridExtra)
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
# Remove missing data rows
#=============================================================#

allmissing <- mci[is.na(mci$blockR) == T & is.na(mci$animals) == T & is.na(mci$logmemI) == T & is.na(mci$logmemII) == T, ]
mci <- mci[!rownames(mci) %in% rownames(allmissing), ]


#=============================================================#
# Remove individuals who don't have at least 3 time points-
# DO THIS FOR EACH OUTCOME!
#=============================================================#

#Get each outcome into a separate data set
blockr <- mci[, c(1, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15)]
animals <- mci[, c(1, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15)]
logmem1 <- mci[, c(1, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15)]
logmem2 <- mci[, c(1, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15)]

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


#=============================================================#
# Explore outcome data
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
p1 <-ggplot(data = blockr, aes(x = age, y = blockR, group = id, col = as.factor(demind))) +
  geom_line() +
  theme_classic() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_discrete("Developed dementia/MCI") +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Block design score") +
  ggtitle("BlockR")



#animals
p2 <- ggplot(data = animals, aes(x = age, y = animals, group = id, col = as.factor(demind))) +
  geom_line() +
  theme_classic() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_discrete("Developed dementia/MCI") +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Animal category fluency score") +
  ggtitle("Animals")

#logmem1
p3 <- ggplot(data = logmem1, aes(x = age, y = logmemI, group = id, col = as.factor(demind))) +
  geom_line() +
  theme_classic() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_discrete("Developed dementia/MCI") +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Logical memory score 1") +
  ggtitle("LogMemI")

#logmem2
p4 <- ggplot(data = logmem2, aes(x = age, y = logmemII, group = id, col = as.factor(demind))) +
  geom_line() +
  theme_classic() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_discrete("Developed dementia/MCI") +scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Logical memory score 2") +
  ggtitle("LogMemII")

grid.arrange(p1, p2, p3, p4, ncol = 2)


#=============================================================#
# Write csvs of outcome data sets
#=============================================================#

setwd("~/School/AdvancedData")
write.csv(mci, "MCICleaned.csv")
write.csv(blockr, "BlockROutcome.csv")
write.csv(animals, "AnimalsOutcome.csv")
write.csv(logmem1, "LogMem1Outcome.csv")
write.csv(logmem2, "LogMem2Outcome.csv")
