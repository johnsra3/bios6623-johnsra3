#=============================================================#
# Project 3
# Functions for change points and bootstraps
# Rachel Johnson
#=============================================================#

#=============================================================#
# Change point function
#=============================================================#

#Create a function to search for change point and fit final change point model
cp.search_and_fit<-function(patid, timeb4dem, age, demind, ses, gender, y, cps){
  
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
# Bootstrap function for changepoint CIs
#=============================================================#

boot.function<-function(ids, dat, cps){
  
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
  boot.model <- cp.search_and_fit(patid = boot.dat$id, timeb4dem = boot.dat$timeb4dem, age = boot.dat$age, 
                                  demind = boot.dat$demind, ses = boot.dat$SES, gender = boot.dat$gender, 
                                  y = boot.dat$y, cps)
  
  #Save the estimates and CP's
  mod1 <- glht(boot.model$model, matrix(c(0, 0, 1, 1, 0, 0, 0), nrow = 1))
  mod2 <- glht(boot.model$model, matrix(c(0, 0, 1, 0, 0, 0, 0), nrow = 1))
  
  boot.rslt <- c(boot.model$cp, 
                 confint(mod1)$confint[1], confint(mod1)$confint[2], confint(mod1)$confint[3],
                 confint(mod2)$confint[1], confint(mod2)$confint[2], confint(mod2)$confint[3]) 
  names(boot.rslt)<-c("Changepoint", "Slope1", "Slope1 Lower CI", "Slope1 Upper CI",
                      "Slope2", "Slope2 Lower CI", "Slope2 Upper CI") 
  
  return(boot.rslt)
}