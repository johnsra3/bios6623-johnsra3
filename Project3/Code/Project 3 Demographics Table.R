#=============================================================#
# Project 3
# Demographics table
# Rachel Johnson
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(tableone)

setwd("~/School/AdvancedData")
mci <- read.csv("C:/Users/johnsra3/Documents/School/AdvancedData/Project3Data.csv", header = T)


#=============================================================#
# Get baseline observations for each person, remove cdr
#=============================================================#

mci <- mci[order(mci$id), ]
mci <- mci[!duplicated(mci$id), ]
mci <- mci[, -which(colnames(mci) == "cdr")]


#=============================================================#
# Format vars for table
#=============================================================#

#Need to make following vars factors: gender, demind
mci$gender <- factor(mci$gender, levels = c("1", "2"), labels = c("Male", "Female"))
mci$demind <- factor(mci$demind, levels = c("0", "1"))

#=============================================================#
# Select vars and create table
#=============================================================#

tabvars <- colnames(mci)[which(colnames(mci) == "gender"):which(colnames(mci) == "logmemII")]
tab1 <- CreateTableOne(vars = tabvars, strata = "demind", data = mci, test = F)
tab1print <- as.data.frame(print(tab1, showAllLevels = T))

tab1_nostrat <- CreateTableOne(vars = tabvars, data = mci, test = F)
tab1nostratprint <- as.data.frame(print(tab1_nostrat, showAllLevels = T))

tab <- cbind.data.frame(tab1nostratprint, tab1print)
tab <- tab[, -3]
colnames(tab) <- c("", "Overall", "Did not develop dementia/MCI during study",
                   "Developed dementia/MCI during study")

setwd("C:/Repositories/bios6623-johnsra3/Project3/Reports")
write.csv(tab, "Table1Demographics.csv")
