#==========================================================#
# Project 2
# Purpose: Explore missing data related to variable albumin
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data
#==========================================================#

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