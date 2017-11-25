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

setwd("C:/Users/johnsra3/Documents/School/AdvancedData")
#blockr <- read.csv("blockrOutcome.csv", header = T)
animals <- read.csv("AnimalsOutcome.csv", header = T)
#logmem1 <- read.csv("LogMem1Outcome.csv", header = T)
#logmem2 <- read.csv("LogMem2Outcome.csv", header = T)


#=============================================================#
# Run animals change point for only -6 to 2
#=============================================================#

cps <- seq(from = -6, to = 2, by = 0.1)

animals$gender <- factor(animals$gender)
animals$demind <- factor(animals$demind)
animals$int <- as.numeric(as.character(animals$demind)) * animals$age_59

#Run the function on the dataset
cp.model <- cp.search_and_fit(id = animals$id, timeb4dem =animals$timeb4dem, age = animals$age_59, 
                              demind = animals$demind, ses = animals$SES, gender = animals$gender, 
                              y = animals$animals, cps = cps, int = animals$int)
summary(cp.model$model)
animals_cp <- cp.model$cp
#cp = -3.9


#=============================================================#
# Prepare data set for modeling w/ cp included
#=============================================================#

#Each model will include: age_59, demind, age_59*demind,
  #SES, gender, max(age - ageonset + cp, 0)

animals$timemax <- animals$timeb4dem - animals_cp 
animals$timecp <- ifelse(animals$timemax < 0, 0, animals$timemax)
animals[is.na(animals)] <- ""


#=============================================================#
# Run mixed model 
#=============================================================#

animals.model <- lme(animals ~ age_59 + demind + age_59*demind + timemax + SES + gender, random = ~1|id, 
                     correlation = corCAR1(form = ~age_59), method = "REML", data = animals)
mmres <- as.data.frame(summary(animals.model)$coeff$fixed)

setwd("C:/Repositories/bios6623-johnsra3/Project3/Reports")
write.csv(mmres, "MixedModelResults.csv")


#=============================================================#
# Bootstrap change point
#=============================================================#

id <- animals$id
timeb4dem <- animals$timeb4dem
age_59 <- animals$age_59
demind <- animals$demind
ses <- animals$SES
gender <- animals$gender
y <- animals$animals
cps <- seq(from = -6, to = 2, by = 0.1)
int <- as.numeric(as.character(animals$demind)) * animals$age_59
dat <- cbind.data.frame(id, timeb4dem, age_59, demind, ses, gender, y, int)

niter <- 10
bootstraps <- matrix(NA, ncol = 10, nrow = niter)
for (j in 1:niter){
  bootstraps[j, ] <- boot.function(ids = id, dat = dat, cps = cps)
  print(j)
}

#setwd("C:/Repositories/bios6623-johnsra3/Project3/Reports")
#write.csv(bootstraps, "BootstrapWithAllEstimates.csv")