#==========================================================#
# Project 2
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data and load libraries
#==========================================================#

library(haven)

setwd("~/School/AdvancedData")
vadata <- read_sas("~/School/AdvancedData/vadata2.sas7bdat")


#==========================================================#
# Divide into different data sets 
#==========================================================#

rec <- vadata[vadata$sixmonth == 39, ]
old <- vadata[vadata$sixmonth != 39, ]


#==========================================================#
# Explore data
#==========================================================#

names(vadata)
summary(vadata)
#missing data: <2% missing in proced, asa, weight, height, BMI
  #largest missing data issue: albumin (missing 13241, ~50%)

#Look at covariates between missing and non-missing
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

summary(alb_miss$weight)
summary(alb_pres$weight)
#very similar

summary(alb_miss$height)
summary(alb_pres$height)
#very similar

summary(alb_miss$bmi) #some impossible measures! need to fix!
summary(alb_pres$bmi) #some impossible measures! need to fix!
#very similar

summary(alb_miss$death30)
summary(alb_pres$death30)
#very similar