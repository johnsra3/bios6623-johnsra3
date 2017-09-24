#=============================================================#
# Project 1
# Code by: Rachel Johnson
# Started: 09/20/2017
# Last updated: 09/23/2017
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(dplyr)

setwd("~/School/AdvancedData")
hiv_initial <- read.csv("hiv_6623_final_nodots.csv", header = T)

hiv0 <- hiv_initial[hiv_initial$years == 0, ] #715 people at baseline
hiv2 <- hiv_initial[hiv_initial$years == 2, ] #506 people at 2 yr mark 


#=============================================================#
# Clean data
#=============================================================#

#Remove people who don't have data at 2 years
hiv0 <- hiv0[hiv0$newid %in% hiv2$newid, ]

hiv0 <- hiv0[, c(which(colnames(hiv0) == "newid"),
                 which(colnames(hiv0) == "AGG_MENT"),
                 which(colnames(hiv0) == "AGG_PHYS"),
                 which(colnames(hiv0) == "LEU3N"),
                 which(colnames(hiv0) == "hard_drugs"),
                 which(colnames(hiv0) == "age"),
                 which(colnames(hiv0) == "BMI"),
                 which(colnames(hiv0) == "RACE"),
                 which(colnames(hiv0) == "HASHV"),
                 which(colnames(hiv0) == "DKGRP"),
                 which(colnames(hiv0) == "SMOKE"),
                 which(colnames(hiv0) == "income"),
                 which(colnames(hiv0) == "EDUCBAS"))] 

hiv2 <- hiv2[, c(which(colnames(hiv2) == "newid"),
                 which(colnames(hiv2) == "ADH"))]

# 2. In previous analyses, we have adjusted for the following variables:

#Need to categorize race, alcohol use, smoke, income, education, ART adherence
#Race- change to NHW (non-hispanic white) vs. Other
table(hiv0$RACE)
hiv0$RACE_cat <- hiv0$RACE  
hiv0$RACE_cat <- ifelse(hiv0$RACE_cat == 1, "Non-White Hispanic", "Other")

#Alcohol
table(hiv0$DKGRP)
hiv0$drink_cat <- hiv0$DKGRP
hiv0$drink_cat <- ifelse(hiv0$DKGRP == 3, "> 13 drinks per week", "13 or fewer drinks per week")

#Smoking
table(hiv0$SMOKE)
hiv0$smoke_cat <- hiv0$SMOKE
hiv0$smoke_cat <- ifelse(hiv0$smoke_cat == 3, "Current", "Never/former")

#Income 
table(hiv0$income)
hiv0$income_cat <- hiv0$income
hiv0$income_cat <- if(hiv0$income_cat == 1){
  hiv0$income_cat <- "< $10,000"
} else {
  if(hiv$income_cat)
}
#figure out how to categorize this :) 


# Baseline income level - < $10,000, $10,000 - $40,000, >$40,000
# Education - >HS vs. HS or less
# ART Adherence, >95% vs. <95%


#Format categorical variables for table

#=============================================================#
# Explore data- use baseline only (hiv0 set)
#=============================================================#

names(hiv)

#Primary predictor- hard_drugs
table(hiv0$hard_drugs)

#Outcomes- use baseline, then 2 year, then create difference variables
#Viral load
summary(hiv0$VLOAD)
boxplot(hiv0$VLOAD, horizontal = T)
#Check on plausible numbers--some very high outliers

#CD4
summary(hiv0$LEU3N)
boxplot(hiv0$LEU3N, horizontal = T)

#AGG_MENT
summary(hiv0$AGG_MENT)

