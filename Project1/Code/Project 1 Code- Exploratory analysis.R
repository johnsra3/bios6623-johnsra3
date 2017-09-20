#=============================================================#
# Project 1
# Code by: Rachel Johnson
# Started: 09/20/2017
# Last updated: 09/20/2017
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

setwd("~/School/AdvancedData")
hiv <- read.csv("hiv_6623_final_nodots.csv", header = T)

hiv0 <- hiv[hiv$years == 0, ]
hiv2 <- hiv[hiv$years == 2, ]

hiv <- rbind.data.frame(hiv0, hiv2)

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


#=============================================================#
# Clean data
#=============================================================#