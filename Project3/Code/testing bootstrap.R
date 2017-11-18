
library(multcomp)

#Test example
ids <- blockr$id
dat <- blockr

patid <- as.character(blockr$id)
demind <- as.factor(blockr$demind)
timeb4dem <- blockr$timeb4dem
age <- blockr$age_59
ses <- blockr$SES
gender <- factor(blockr$gender, levels = c("1", "2"))
y <- blockr$blockR

cps <- seq(from = -15.1, to = -0.1, by = 0.1)


#Run the function on the dataset
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
  boot.model <-cp.search_and_fit(patid, timeb4dem, age, ses, gender, y, cps)
  
  #Save the estimates and CP's
  boot.rslt <- c(boot.model$cp, 
               confint(glht(boot.model$model, matrix(c(0, 0, 1, 1, 0, 0, 0), nrow = 1))$confint[1]), #slope after change point
               confint(glht(boot.model$model, matrix(c(0, 0, 1, 0, 0, 0, 0), nrow = 1))$confint[1])) #slope before change point
  names(boot.rslt)<-c("Changepoint", "Slope1", "Slope2")
  
  return(boot.rslt)
}


niter <- 50

bootstraps <- matrix(NA, ncol = 7, nrow = 1000)
for (j in 1:niter){
  bootstraps[j, ] <- boot.function(ids = blockr$id, dat = blockr, cps = cps)
  print(i)
}
#GETTING ERRORS FROM THIS!!
# Error in UseMethod("vcov") : 
  #no applicable method for 'vcov' applied to an object of class "NULL"


#Step 5:Inspect the bootstrap distributions

#Change Point
hist(bootstraps[,1], xlab="Change Point", main='Bootstrap Distribution of Change Points')
lines(c(cp,cp), c(0,1000), col='red')
lines(c(mean(bootstraps[,1]),mean(bootstraps[,1])), c(0,1000))

mean(bootstraps[,1])
sd(bootstraps[,1])
quantile(bootstraps[,1], c(0.025, 0.975))

#Change in VAS at 3 months
hist(bootstraps[,2], xlab="3 Month Change in VAS", main='Bootstrap Estimates of 3 Month VAS Change')
lines(c(-3.3010,-3.3010), c(0,1000), col='red')
lines(c(mean(bootstraps[,2]),mean(bootstraps[,2])), c(0,1000))

mean(bootstraps[,2])
sd(bootstraps[,2])
quantile(bootstraps[,2], c(0.025, 0.975))
