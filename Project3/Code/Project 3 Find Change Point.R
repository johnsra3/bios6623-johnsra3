#=============================================================#
# Project 3
# Find change point for each outcome
# Rachel Johnson
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(nlme)
source("C:/Repositories/bios6623-johnsra3/Project3/Code/Functions- Change Points and Bootstrap.R")

setwd("C:/Users/johnsra3/Documents/School/Adv/ancedData")
blockr <- read.csv("blockrOutcome.csv", header = T)
animals <- read.csv("AnimalsOutcome.csv", header = T)
logmem1 <- read.csv("LogMem1Outcome.csv", header = T)
logmem2 <- read.csv("LogMem2Outcome.csv", header = T)


#=============================================================#
# Change point for blockr
#=============================================================#

fivenum(blockr$timeb4dem)

patid <- as.character(blockr$id)
demind <- as.factor(blockr$demind)
timeb4dem <- blockr$timeb4dem
age <- blockr$age_59
ses <- blockr$SES
gender <- factor(blockr$gender, levels = c("1", "2"))
y <- blockr$blockR

cps <- seq(from = -15.1, to = -0.1, by = 0.1)


#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
summary(cp.model$model)
coeff <- cp.model$model$coefficients$fixed

blockr_cp <- cp.model$cp
#cp = -3.9-- This seems to match up fairly well to what's online!


#=============================================================#
# Change point for animals
#=============================================================#

fivenum(animals$timeb4dem)

patid <- as.character(animals$id)
demind <- as.factor(animals$demind)
timeb4dem <- animals$timeb4dem
age <- animals$age_59
ses <- animals$SES
gender <- factor(animals$gender, levels = c("1", "2"))
y <- animals$animals

#Sequence of change points to consider
cps <- seq(from = -12.4, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

animals_cp <- cp.model$cp
#cp = -3.9


#=============================================================#
# Change point for logmem1
#=============================================================#

fivenum(logmem1$timeb4dem)

patid <- as.character(logmem1$id)
demind <- as.factor(logmem1$demind)
timeb4dem <- logmem1$timeb4dem
age <- logmem1$age_59
ses <- logmem1$SES
gender <- factor(logmem1$gender, levels = c("1", "2"))
y <- logmem1$logmemI

cps <- seq(from = -15.1, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

logmem1_cp <- cp.model$cp
#cp = -2.8


#=============================================================#
# Change point for logmem2
#=============================================================#

fivenum(logmem2$timeb4dem)

patid <- as.character(logmem2$id)
demind <- as.factor(logmem2$demind)
timeb4dem <- logmem2$timeb4dem
age <- logmem2$age_59
ses <- logmem2$SES
gender <- factor(logmem2$gender, levels = c("1", "2"))
y <- logmem2$logmemII

#Sequence of change points to consider
cps <- seq(from = -15.1, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

logmem2_cp <- cp.model$cp
#cp = -3.0


#=============================================================#
# Prepare each data set for modeling in SAS w/ cp included
#=============================================================#

#Each model will include: age_59, demind, age_59*demind,
  #SES, gender, max(age - ageonset + cp, 0)

blockr$timemax <- blockr$timeb4dem - blockr_cp
animals$timemax <- animals$timeb4dem - animals_cp 
logmem1$timemax <- logmem1$timeb4dem - logmem1_cp
logmem2$timemax <- logmem2$timeb4dem - logmem2_cp 

blockr$timecp <- ifelse(blockr$timemax < 0, 0, blockr$timemax)
animals$timecp <- ifelse(animals$timemax < 0, 0, animals$timemax)
logmem1$timecp <- ifelse(logmem1$timemax < 0, 0, logmem1$timemax)
logmem2$timecp <- ifelse(logmem2$timemax < 0, 0, logmem2$timemax)

blockr[is.na(blockr)] <- ""
animals[is.na(animals)] <- ""
logmem1[is.na(logmem1)] <- ""
logmem2[is.na(logmem2)] <- ""

setwd("C:/Users/johnsra3/Documents/School/AdvancedData")
write.csv(blockr, "BlockRModeling.csv")
write.csv(animals, "AnimalsModeling.csv")
write.csv(logmem1, "LogMem1Modeling.csv")
write.csv(logmem2, "LogMem2Modeling.csv")
