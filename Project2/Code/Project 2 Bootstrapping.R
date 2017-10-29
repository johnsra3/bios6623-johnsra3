#==========================================================#
# Project 2
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data 
#==========================================================#

setwd("~/School/AdvancedData")
vadata <- read.csv("VadataCleaned.csv", header = T)


#==========================================================#
# Divide into different data sets, create factor levels
#==========================================================#

rec <- vadata[vadata$sixmonth == 39, ]
old <- vadata[vadata$sixmonth != 39, ]

vadata$asa <- factor(vadata$asa, levels = c("3 or less", "4 or greater"))

#==========================================================#
# Create data set for complete cases to use in bootstrap
#==========================================================#

comp <- vadata[, c(which(colnames(vadata) == "hospcode"),
                   which(colnames(vadata) == "sixmonth"),
                   which(colnames(vadata) == "proced"),
                   which(colnames(vadata) == "asa_indic"),
                   which(colnames(vadata) == "bmi"),
                   which(colnames(vadata) == "death30"))]
comp <- comp[complete.cases(comp), ]

comp_rec <- comp[comp$sixmonth == 39, ]


#==========================================================#
# Write loop to bootstrap
#==========================================================#

#Need following steps (1-5 in loop, 6 separate):
  # 1. Sample from w/i total population w/ replacement in complete cases
  # 2. Run logistic regression w/ this resampled population
  # 3. Extract fitted values and exponentiate them 
  # 4. Calculate p_fitted for each individual in the resampled data set w/ inv.logit
  # 5. Get an average p_fitted for each hospital w/ aggregate
  # 6. Find distribution of p_fitted for each hosp, w/ 2.5% and 97.5% pieces (boot.ci)

#Place to store p_fits
num_iter <- 10000
boot.stats <- matrix(data = NA, ncol = 43, nrow = num_iter)
colnames(boot.stats) <- c(seq(from = 1, to = 29, by = 1), seq(from = 31, to = 44, by = 1))


for(i in 1:num_iter){ 
  
  set.seed(i)
  
  boot.samps <- sample(nrow(comp), replace = T)
  boot.dat <- comp[boot.samps, ]
  
  boot.model <- glm(death30 ~ proced + asa_indic + bmi, data = boot.dat, family = binomial(link = "logit"))
  coeff <- summary(boot.model)$coefficients
  
  xb <- coeff[1] + coeff[2]*comp$proced + coeff[3]*comp$asa_indic + coeff[4]*comp$bmi
  comp$pfit <- inv.logit(xb)
  comp_pd39 <- comp[comp$sixmonth == 39, ]
  
  boot.stats[i, ] <- round(aggregate(comp_pd39$pfit, list(comp_pd39$hospcode), mean)[, 2] * 100, 2)
  
  print(i)

}

setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
write.csv(boot.stats, "BootstrapResults_raw.csv")


