#==========================================================#
# Project 2 Missing Data
# Purpose: Explore missing data related to variable albumin
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data
#==========================================================#

library(dplyr)
setwd("~/School/AdvancedData")
vadata <- read.csv("VadataCleaned.csv", header = T)

#==========================================================#
# Look at missing data for ALBUMIN 
#==========================================================#

#Look at covariates between missing and non-missing for ALBUMIN
alb_miss <- vadata[is.na(vadata$albumin) == T, ]
alb_pres <- vadata[is.na(vadata$albumin) == F, ]

table(alb_miss$hospcode)
table(alb_pres$hospcode)
#very similar

table(alb_miss$proced)
table(alb_pres$proced)
#very similar

table(alb_miss$asa)
table(alb_pres$asa)
#very similar

summary(alb_miss$height)
summary(alb_pres$height)
#very similar

summary(alb_miss$weight)
summary(alb_pres$weight)
#very similar

summary(alb_miss$bmi) #some v. v. low measures, but are correct
summary(alb_pres$bmi) #some v. v. low measures, but are correct
#very similar

table(alb_miss$death30)
table(alb_pres$death30)
#very similar


#No missingness is explained by covs, either MCAR or MNAR 


#==========================================================#
# Compare outcome data for those w/ complete cases & those
# w/o complete cases for log. regr. 
#==========================================================#

comp <- vadata[, c(1, 2, 3, 4, 8, 10, 11)]
comp <- comp[complete.cases(comp), ]
comp_39 <- comp[comp$sixmonth == 39, ]

excl <- vadata[, c(1, 2, 3, 4, 8, 10, 11)]
excl <- excl[!excl$X %in% comp$X, ]
excl_39 <- excl[excl$sixmonth == 39, ]

table(comp$death30)
table(excl$death30)

summary(comp$death30)
summary(excl$death30)

summary(comp_39$death30)
summary(excl_39$death30)

vadata$complete <- ifelse(vadata$X %in% comp$X, 1, 0)
pd39 <- vadata[vadata$sixmonth == 39, ]
chisq.test(pd39$complete, pd39$death30) #significantly different!--bias issue

#There is a difference b/t death rates. Note this in discussion/limitations and state that
  #expected values may be too low. 
#Whether looking at period 39 or not... need to figure out which one to report. 
#Need to note this bias

#==========================================================#
# Determine if bias from BMI or proced missingness
#==========================================================#

missing_bmi <- excl[is.na(excl$bmi) == T, ]
missing_bmi39 <- missing_bmi[missing_bmi$sixmonth == 39, ]
missing_proced <- excl[is.na(excl$proced) == T, ]
missing_proced39 <- missing_proced[missing_proced$sixmonth == 39, ]

summary(missing_bmi$death30)
summary(missing_proced$death30)

summary(missing_bmi39$death30)
summary(missing_proced39$death30)
