#=============================================================#
# Project 3
# Find change point for each outcome
# Rachel Johnson
#=============================================================#

#Note: Not sure that I've done change point stuff correctly, 
  #but have attempted it for each outcome


#=============================================================#
# Import data
#=============================================================#

library(nlme)

setwd("C:/Users/johnsra3/Documents/School/AdvancedData")
blockr <- read.csv("blockrOutcome.csv", header = T)
animals <- read.csv("AnimalsOutcome.csv", header = T)
logmem1 <- read.csv("LogMem1Outcome.csv", header = T)
logmem2 <- read.csv("LogMem2Outcome.csv", header = T)


#=============================================================#
# Select only people w/ dementia for each outcome, then find
#   diff in time b/t age @ visit and diag age
#=============================================================#

blockr$timeb4dem <- ifelse(blockr$demind == 1 & blockr$ageonset - blockr$age >= 0,
                           blockr$age - blockr$ageonset, 0)
animals$timeb4dem <- ifelse(animals$demind == 1 & animals$ageonset - animals$age >= 0,
                           animals$age - animals$ageonset, 0)
logmem1$timeb4dem <- ifelse(logmem1$demind == 1 & logmem1$ageonset - logmem1$age >= 0,
                           logmem1$age - logmem1$ageonset, 0)
logmem2$timeb4dem <- ifelse(logmem2$demind == 1 & logmem2$ageonset - logmem2$age >= 0,
                           logmem2$age - logmem2$ageonset, 0)


#=============================================================#
# Change point function
#=============================================================#

#Create a function to search for change point and fit final change point model
cp.search_and_fit<-function(patid, t1, age, ses, gender, y, cps){
  
  #Place to store likelihoods from the CP search
  ll<-data.frame(changepoint = rep(NA, length(cps)), ll = rep(NA, length(cps)))
  
  #Search for the CP
  for (i in 1:length(cps)){
    cp <- cps[i]
    t2 <- ifelse(t1 > cp, t1 - cp, 0)
    cp.model <- lme(y ~ t1 + t2 + age + ses + gender, random = ~1|patid, method = 'ML')
    ll[i, ] <- c(cp, logLik(cp.model))
  }
  
  #Plot the likelihood
  plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (years)', ylab='Log Likelihood')
  
  #Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  print(cp)
  
  #Fit the final model
  t2 <- ifelse(t1 > cp, t1 - cp, 0)
  cp.model <- lme(y~t1+t2, random=~1|patid)
  return(list(cp=cp, model=cp.model))

}


#=============================================================#
# Change point for blockR
#=============================================================#

fivenum(blockr$timeb4dem)

patid <- as.character(blockr$id)
t1 <- blockr$timeb4dem
age <- blockr$age
ses <- blockr$SES
gender <- factor(blockr$gender, levels = c("1", "2"))
y <- blockr$blockR

#Sequence of change points to consider
cps <- seq(from = -15.1, to = -0.1, by = 0.1)


#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, t1, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

#t2 = -3.66766-- This seems to match up fairly well to what's online!


#=============================================================#
# Change point for animals
#=============================================================#

fivenum(animals$timeb4dem)

patid <- as.character(animals$id)
t1 <- animals$timeb4dem
age <- animals$age
ses <- animals$SES
gender <- factor(animals$gender, levels = c("1", "2"))
y <- animals$animals

#Sequence of change points to consider
cps <- seq(from = -12.4, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, t1, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

#t2 = -2.229631, hmm seems a little lower

#=============================================================#
# Change point for logmem1
#=============================================================#

fivenum(logmem1$timeb4dem)

patid <- as.character(logmem1$id)
t1 <- logmem1$timeb4dem
age <- logmem1$age
ses <- logmem1$SES
gender <- factor(logmem1$gender, levels = c("1", "2"))
y <- logmem1$logmemI

#Sequence of change points to consider
cps <- seq(from = -15.1, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, t1, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

#t2 = -1.82367879, hmm seems a little lower


#=============================================================#
# Change point for logmem2
#=============================================================#

fivenum(logmem2$timeb4dem)

patid <- as.character(logmem2$id)
t1 <- logmem2$timeb4dem
age <- logmem2$age
ses <- logmem2$SES
gender <- factor(logmem2$gender, levels = c("1", "2"))
y <- logmem2$logmemII

#Sequence of change points to consider
cps <- seq(from = -15.1, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, t1, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

#t2 = -1.6992988