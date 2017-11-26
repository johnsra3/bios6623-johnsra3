#=============================================================#
# Project 3
# Functions for change points and bootstraps
# Rachel Johnson
#=============================================================#

library(nlme)

#=============================================================#
# Change point function-- note: specific to animals data set
#=============================================================#

#Create a function to search for change point and fit final change point model
cp.search_and_fit<-function(id, timeb4dem, age_59, demind, ses, gender, y, cps, int){
  
  #Place to store likelihoods from the CP search
  ll<-data.frame(changepoint = rep(NA, length(cps)), ll = rep(NA, length(cps)))
  
  #Search for the CP
  for (i in 1:length(cps)){
    cp <- cps[i]
    timemax <- ifelse(timeb4dem > cp, timeb4dem - cp, 0)
    cp.model <- lme(y ~ age_59 + demind + int + timemax + 
                      ses + gender, random = ~1|id,
                    correlation = corCAR1(form = ~age_59), method = "REML")
    ll[i, ] <- c(cp, logLik(cp.model))
  }
  
  #Plot the likelihood
  plot(ll$changepoint, ll$ll, type='l', xlab='Change Point (years)', ylab='Log Likelihood')
  
  #Find the max
  cp<-ll[which(ll$ll==max(ll$ll)),'changepoint']
  print(cp)
  
  #Fit the final model
  timemax <- ifelse(timeb4dem > cp, timeb4dem - cp, 0)
  cp.model <- lme(y ~ age_59 + demind + int + timemax + ses + gender, random = ~1|id, 
                  correlation = corCAR1(form = ~age_59), method = "REML")
  return(list(cp=cp, model=cp.model))
  
}


#=============================================================#
# Bootstrap function for changepoint CIs
#=============================================================#

boot.function <- function(ids, dat, cps){
  
  #Sample subjects randomly w/ replacemnt
  ids.u <- unique(ids)
  boot.subjects <- sample(ids.u, length(ids.u), replace = T)
  
  #Grab the data for each of the chosen subjects
  boot.dat <- NULL
  for (i in 1:length(ids.u)){ 
    temp <- cbind(i, dat[ids==boot.subjects[i],])
    boot.dat <- rbind(boot.dat, temp)
  }
  
  #Repeat the analysis on the bootstrap sample
  boot.model <- cp.search_and_fit(id = boot.dat$id, timeb4dem = boot.dat$timeb4dem, age = boot.dat$age_59, 
                                  demind = boot.dat$demind, ses = boot.dat$ses, gender = boot.dat$gender, 
                                  y = boot.dat$y, int = boot.dat$int, cps = cps)
  
  #Save the estimates and CP's
  mod1 <- glht(boot.model$model, matrix(c(0, 0, 1, 1, 0, 0, 0), nrow = 1))
  mod2 <- glht(boot.model$model, matrix(c(0, 0, 1, 0, 0, 0, 0), nrow = 1))
  
  boot.rslt <- c(boot.model$cp,
                 confint(mod1)$confint[1],
                 confint(mod2)$confint[1],
                 boot.model$model$coefficient$fixed[1],
                 boot.model$model$coefficient$fixed[2],
                 boot.model$model$coefficient$fixed[3],
                 boot.model$model$coefficient$fixed[4],
                 boot.model$model$coefficient$fixed[5],
                 boot.model$model$coefficient$fixed[6],
                 boot.model$model$coefficient$fixed[7])
  names(boot.rslt)<-c("Changepoint", "Slope1", "Slope2", "Intercept",
                      "Age_59", "Dementia = 1", "Change-point", "SES", "Gender = 2",
                      "Age_59*Dementia = 1")
  
  return(boot.rslt)
}

























