#=============================================================#
# Project 3
# Demographics table
# Rachel Johnson
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(tableone)
#ONLY INCLUDE ANIMALS OUTCOME- T/F INCLUDE TABLE BASED ON THESE INDIVDIUALS
animals <- read.csv("C:/Users/johnsra3/Documents/School/AdvancedData/AnimalsOutcome.csv", header = T)

#only include baseline observations for this table
animals <- animals[order(animals$id), ]
animals <- animals[!duplicated(animals$id), ]


#=============================================================#
# Format vars for table
#=============================================================#

#Need to make following vars factors: gender, demind
animals$gender <- factor(animals$gender, levels = c("1", "2"), labels = c("Male", "Female"))
animals$demind <- factor(animals$demind, levels = c("0", "1"))


#=============================================================#
# Select vars and create table
#=============================================================#

tabvars <- c("numobs", "followup", "gender", "SES", "age", "blockR", 
             "animals", "logmemI", "logmemII")
tab1 <- CreateTableOne(vars = tabvars, strata = "demind", data = animals, test = F)
tab1print <- as.data.frame(print(tab1, showAllLevels = T))

tab1_nostrat <- CreateTableOne(vars = tabvars, data = animals, test = F)
tab1nostratprint <- as.data.frame(print(tab1_nostrat, showAllLevels = T))

tab <- cbind.data.frame(tab1nostratprint, tab1print)
tab <- tab[, -3]
colnames(tab) <- c("", "Overall", "Did not develop dementia/MCI during study",
                   "Developed dementia/MCI during study")

# setwd("C:/Repositories/bios6623-johnsra3/Project3/Reports")
# write.csv(tab, "Table1Demographics.csv")
