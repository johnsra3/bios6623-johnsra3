#==========================================================#
# Project 2 Logistic Regressions
# Purpose: Run logistic regressions, create results tables 
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data
#==========================================================#

library(boot)

setwd("~/School/AdvancedData")
vadata <- read.csv("VadataCleaned.csv", header = T)

#==========================================================#
# Divide into different data sets, create factor levels
#==========================================================#

rec <- vadata[vadata$sixmonth == 39, ]
old <- vadata[vadata$sixmonth != 39, ]

vadata$asa <- factor(vadata$asa, levels = c("3 or less", "4 or greater"))


#==========================================================#
# Logistic regressions
#==========================================================#

#With albumin
model1 <- glm(death30 ~ proced + asa + bmi + albumin, data = vadata,
              family = binomial(link = "logit"))
summary(model1)
coeff1 <- summary(model1)$coefficients

#Without albumin
model2 <- glm(death30 ~ proced + asa + bmi, data = vadata,
              family = binomial(link = "logit"))
summary(model2)
coeff2 <- summary(model2)$coefficients

#Decisions do not change-- albumin can be excluded, but still report results
  #p-vals change some, but not in decision; estimates only change a little
  #all estimate directions are the same
  #albumin is not a significant predictor


#==========================================================#
# Make tables for logistic regression results
#==========================================================#

#Model 1- with albumin
mod1tab <- matrix(data = NA, nrow = 4, ncol = 4)
colnames(mod1tab) <- c("Covariate", "Estimate", "95% CI", "p-value")

mod1tab[1:4, 1] <- c("Procedure (reference = valve surgery)", "ASA (reference = 3 or less)", "BMI", "Albumin")

e <- exp(1)
mod1tab[1, 2] <- round(e^coeff1[2, 1], 3)
mod1tab[2, 2] <- round(e^coeff1[3, 1], 3)   
mod1tab[3, 2] <- round(e^coeff1[4, 1], 3)
mod1tab[4, 2] <- round(e^coeff1[5, 1], 3)

mod1tab[1, 3] <- paste(paste(round(e^(coeff1[2, 1] - 1.96*coeff1[2, 2]), 3), ",", sep = ""), 
                       round(e^(coeff1[2, 1] + 1.96*coeff1[2, 2]), 3)) 
mod1tab[2, 3] <- paste(paste(round(e^(coeff1[3, 1] - 1.96*coeff1[3, 2]), 3), ",", sep = ""), 
                       round(e^(coeff1[3, 1] + 1.96*coeff1[3, 2]), 3)) 
mod1tab[3, 3] <- paste(paste(round(e^(coeff1[4, 1] - 1.96*coeff1[4, 2]), 3), ",", sep = ""), 
                       round(e^(coeff1[4, 1] + 1.96*coeff1[4, 2]), 3)) 
mod1tab[4, 3] <- paste(paste(round(e^(coeff1[5, 1] - 1.96*coeff1[5, 2]), 3), ",", sep = ""), 
                       round(e^(coeff1[5, 1] + 1.96*coeff1[5, 2]), 3)) 

mod1tab[1, 4] <- round(coeff1[2, 4], 3)
mod1tab[2, 4] <- round(coeff1[3, 4], 3)
mod1tab[2, 4] <- "<0.001"
mod1tab[3, 4] <- round(coeff1[4, 4], 3)
mod1tab[4, 4] <- round(coeff1[5, 4], 3)

#Model 2- without albumin
mod2tab <- matrix(data = NA, nrow = 3, ncol = 4)
colnames(mod2tab) <- c("Covariate", "Estimate", "95% CI", "p-value")

mod2tab[1:3, 1] <- c("Procedure (reference = valve surgery)", "ASA (reference = 3 or less)", "BMI")

e <- exp(1)
mod2tab[1, 2] <- round(e^coeff2[2, 1], 3)
mod2tab[2, 2] <- round(e^coeff2[3, 1], 3)   
mod2tab[3, 2] <- round(e^coeff2[4, 1], 3)

mod2tab[1, 3] <- paste(paste(round(e^(coeff2[2, 1] - 1.96*coeff2[2, 2]), 3), ",", sep = ""), 
                       round(e^(coeff2[2, 1] + 1.96*coeff2[2, 2]), 3)) 
mod2tab[2, 3] <- paste(paste(round(e^(coeff2[3, 1] - 1.96*coeff2[3, 2]), 3), ",", sep = ""), 
                       round(e^(coeff2[3, 1] + 1.96*coeff2[3, 2]), 3)) 
mod2tab[3, 3] <- paste(paste(round(e^(coeff2[4, 1] - 1.96*coeff2[4, 2]), 3), ",", sep = ""), 
                       round(e^(coeff2[4, 1] + 1.96*coeff2[4, 2]), 3)) 

mod2tab[1, 4] <- round(coeff2[2, 4], 3)
mod2tab[2, 4] <- round(coeff2[3, 4], 3)
mod2tab[2, 4] <- "<0.001"
mod2tab[3, 4] <- round(coeff2[4, 4], 3)

# setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
# write.csv(mod1tab, "ModelWithAlbuminResults.csv")
# write.csv(mod2tab, "ModelWithoutAlbuminResults.csv")

#==========================================================#
# Predicted values to individuals' odds to indivs' pred p
#==========================================================#

#Need a complete case data for BMI, procedure, ASA
comp <- vadata[, c(which(colnames(vadata) == "hospcode"),
                   which(colnames(vadata) == "sixmonth"),
                   which(colnames(vadata) == "proced"),
                   which(colnames(vadata) == "asa_indic"),
                   which(colnames(vadata) == "bmi"),
                   which(colnames(vadata) == "death30"))]
comp <- comp[complete.cases(comp), ]

comp_rec <- comp[comp$sixmonth == 39, ]

#Extract fitted values from summary of model2
(int <- summary(model2)$coefficients[1, 1])
(pred_proced <- summary(model2)$coefficients[2, 1])
(pred_asa <- summary(model2)$coefficients[3, 1])
(pred_bmi <- summary(model2)$coefficients[4, 1])

#Create XB column in comp_rec (last 6 month period only)
comp_rec$XB <- int + pred_proced*comp_rec$proced + pred_asa*comp_rec$asa_indic + pred_bmi*comp_rec$bmi

#Create predicted p column in comp
comp_rec$pred_p <- inv.logit(comp_rec$XB) 

#==========================================================#
# Write csv of comp_rec w/ predicted values by hosp
#==========================================================#

setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
write.csv(comp_rec, "CompleteCasesPd39Predicted.csv")
