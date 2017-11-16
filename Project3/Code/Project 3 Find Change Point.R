#=============================================================#
# Project 3
# Find change point for each outcome
# Rachel Johnson
#=============================================================#

#Note: Not sure that I've done change point stuff correctly, 
  #but have attempted it for each outcome
#Notes from Nichole: need to have these models the exact same
  #as the model that we're going to fit--make it match paper


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
# Change point function
#=============================================================#

#Create a function to search for change point and fit final change point model
cp.search_and_fit<-function(patid, timeb4dem, age, ses, gender, y, cps){
  
  #Place to store likelihoods from the CP search
  ll<-data.frame(changepoint = rep(NA, length(cps)), ll = rep(NA, length(cps)))
  
  #Search for the CP
  for (i in 1:length(cps)){
    cp <- cps[i]
    timemax <- ifelse(timeb4dem > cp, timeb4dem - cp, 0)
    cp.model <- lme(y ~ age + demind + age*demind + timemax + ses + gender, random = ~1|patid, method = 'ML')
    ll[i, ] <- c(cp, logLik(cp.model))
  }
  
  #Plot the likelihood
  plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (years)', ylab='Log Likelihood')
  
  #Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  print(cp)
  
  #Fit the final model
  timemax <- ifelse(timeb4dem > cp, timeb4dem - cp, 0)
  cp.model <- lme(y~ age + demind + age*demind + timemax + ses + gender, random=~1|patid)
  return(list(cp=cp, model=cp.model))

}


#=============================================================#
# Change point for blockR---updated but getting errors!!
#   HAVEN'T ATTEMPTED THIS YET IN OTHER OUTCOMES
#=============================================================#

fivenum(blockr$timeb4dem)

patid <- as.character(blockr$id)
demind <- blockr$demind
timeb4dem <- blockr$timeb4dem
age <- blockr$age
ses <- blockr$SES
gender <- factor(blockr$gender, levels = c("1", "2"))
y <- blockr$blockR

cps <- seq(from = -15.1, to = -0.1, by = 0.1)


#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
summary(cp.model$model)
coeff <- cp.model$model$coefficients$fixed

#cp = -3.9-- This seems to match up fairly well to what's online!


#=============================================================#
# Change point for animals
#=============================================================#

fivenum(animals$timeb4dem)

patid <- as.character(animals$id)
demind <- animals$demind
timeb4dem <- animals$timeb4dem
age <- animals$age
ses <- animals$SES
gender <- factor(animals$gender, levels = c("1", "2"))
y <- animals$animals

#Sequence of change points to consider
cps <- seq(from = -12.4, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

#cp = -3.4


#=============================================================#
# Change point for logmem1
#=============================================================#

fivenum(logmem1$timeb4dem)



#Sequence of change points to consider
cps <- seq(from = -15.1, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

#cp = -3.1


#=============================================================#
# Change point for logmem2
#=============================================================#

fivenum(logmem2$timeb4dem)


#Sequence of change points to consider
cps <- seq(from = -15.1, to = -0.1, by = 0.1)

#Run the function on the dataset
cp.model <- cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
summary(cp.model$model)
(coeff <- cp.model$model$coefficients$fixed)

#cp = -3.9