#=================================================================#
# Project 0
# Purpose: Analyze data to determine if dental gel improves gum health measurements
# Rachel Johnson
# Code started: 08/30/2017
# Last updated: 08/30/2017
#=================================================================#

#=================================================================#
# Import and clean data
#=================================================================#

setwd("~/School/AdvancedData")
gums <- read.csv("Project0_dental_data.csv", header = T)

#=================================================================#
# Explore all variables indiv. (even though randomized trial)
#=================================================================#

names(gums)

table(gums$id) #no repeat IDs
table(gums$trtgroup) #evenly spread; 26 in each group
table(gums$trtgroup) #not an even number of men and women
table(gums$sex, gums$trtgroup) #however there are approx. equal in each group
table(gums$race) #definitely uneven numbers

table(gums$age)
gums$age <- as.numeric(gums$age)
summary(gums$age)

table(gums$smoker, gums$trtgroup)
summary(gums$sites)


#=================================================================#
# Explore univ relationships b/t covs & outcome 1: attach base/attach 1 year
#=================================================================#

