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

##################
#Remove people who don't have data at 2 years
hiv0 <- hiv0[hiv0$newid %in% hiv2$newid, ]

hiv0 <- hiv0[, c(which(colnames(hiv0) == "newid"),
                 which(colnames(hiv0) == "AGG_MENT"),
                 which(colnames(hiv0) == "AGG_PHYS"),
                 which(colnames(hiv0) == "LEU3N"),
                 which(colnames(hiv0) == "VLOAD"),
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
                 which(colnames(hiv2) == "ADH"),
                 which(colnames(hiv2) == "AGG_MENT"),
                 which(colnames(hiv2) == "AGG_PHYS"),
                 which(colnames(hiv2) == "LEU3N"),
                 which(colnames(hiv2) == "VLOAD"))]


###################
#Need to categorize race, alcohol use, smoke, income, education, ART adherence
#Race- change to NHW (non-hispanic white) vs. Other
table(hiv0$RACE)
hiv0$RACE_cat <- hiv0$RACE  
hiv0$RACE_cat <- ifelse(hiv0$RACE_cat == 1, "Non-Hispanic White", "Other")
table(hiv0$RACE_cat)

#Alcohol
table(hiv0$DKGRP)
hiv0$drink_cat <- hiv0$DKGRP
hiv0$drink_cat <- ifelse(hiv0$DKGRP == 3, "> 13 drinks per week", "13 or fewer drinks per week")
table(hiv0$drink_cat)

#Smoking
table(hiv0$SMOKE)
hiv0$smoke_cat <- hiv0$SMOKE
hiv0$smoke_cat <- ifelse(hiv0$smoke_cat == 3, "Current", "Never/former")
table(hiv0$smoke_cat)

#Income 
table(hiv0$income)
hiv0$income_cat <- hiv0$income
hiv0$income_cat[hiv0$income_cat == 9] <- NA #remove those who chose not to answer
hiv0$income_cat <- as.character(hiv0$income_cat)
hiv0$income_cat[hiv0$income_cat == "1"] <- "< $10,000"
hiv0$income_cat[hiv0$income_cat == "2"] <- "$10,000 - $40,000"
hiv0$income_cat[hiv0$income_cat == "3"] <- "$10,000 - $40,000"
hiv0$income_cat[hiv0$income_cat == "4"] <- "$10,000 - $40,000"
hiv0$income_cat[hiv0$income_cat == "5"] <- "> $40,000"
hiv0$income_cat[hiv0$income_cat == "6"] <- "> $40,000"
hiv0$income_cat[hiv0$income_cat == "7"] <- "> $40,000"

#Education
table(hiv0$EDUCBAS)
hiv0$educ_cat <- hiv0$EDUCBAS
hiv0$educ_cat <- ifelse(hiv0$educ_cat > 3, ">HS", "HS or less")
table(hiv0$educ_cat)

# ART Adherence, >95% vs. <95%
table(hiv2$ADH)
hiv2$adh_cat <- hiv2$ADH
hiv2$adh_cat <- ifelse(hiv2$adh_cat < 3, ">95%", "<95%")
table(hiv2$adh_cat)


####################
#Format categorical variables for table
names(hiv0)

#hard_drugs
table(hiv0$hard_drugs)
hiv0$hard_drugs <- factor(hiv0$hard_drugs, levels = c("0", "1"),
                          labels = c("No", "Yes"))

#RACE_cat
table(hiv0$RACE_cat)
hiv0$RACE_cat <- factor(hiv0$RACE_cat, levels = c("Non-Hispanic White", "Other"))

#drink_cat
table(hiv0$drink_cat)
hiv0$drink_cat <- factor(hiv0$drink_cat, levels = c("13 or fewer drinks per week", "> 13 drinks per week"))

#smoke_cat
table(hiv0$smoke_cat)
hiv0$smoke_cat <- factor(hiv0$smoke_cat, levels = c("Never/former", "Current"))

#income_cat
table(hiv0$income_cat)
hiv0$income_cat <- factor(hiv0$income_cat, levels = c("< $10,000", "$10,000 - $40,000", "> $40,000"))

#educ_cat
table(hiv0$educ_cat)
hiv0$educ_cat <- factor(hiv0$educ_cat, levels = c("HS or less", ">HS"))

#adh_cat (at 2 years)
table(hiv2$adh_cat)
hiv2$adh_cat <- factor(hiv2$adh_cat, levels = c("<95%", ">95%"))


#=============================================================#
# Merge datasets and create difference variables
#=============================================================#

####Merge
names(hiv0)
names(hiv2)

hiv0_merge <- hiv0[, c(which(colnames(hiv0) == "newid"),
                       which(colnames(hiv0) == "AGG_MENT"),
                       which(colnames(hiv0) == "AGG_PHYS"),
                       which(colnames(hiv0) == "LEU3N"),
                       which(colnames(hiv0) == "VLOAD"),
                       which(colnames(hiv0) == "hard_drugs"),
                       which(colnames(hiv0) == "age"),
                       which(colnames(hiv0) == "BMI"),
                       which(colnames(hiv0) == "RACE_cat"),
                       which(colnames(hiv0) == "drink_cat"),
                       which(colnames(hiv0) == "smoke_cat"),
                       which(colnames(hiv0) == "income_cat"),
                       which(colnames(hiv0) == "educ_cat"))]

colnames(hiv2)[3:7] <- c("AGG_MENT_2yr", "AGG_PHYS_2yr", "LEU3N_2yr",
                         "VLOAD_2yr", "adh_cat_2yr")

hiv <- merge(hiv0_merge, hiv2, by = "newid")

###########
# Create difference variables

hiv$diff_vload <- hiv$VLOAD_2yr - hiv$VLOAD
hiv$diff_leu3n <- hiv$LEU3N_2yr - hiv$LEU3N
hiv$diff_aggment <- hiv$AGG_MENT_2yr - hiv$AGG_MENT
hiv$diff_aggphys <- hiv$AGG_PHYS_2yr - hiv$AGG_PHYS

##########
# Make data set w/ no missing outcomes for each outocme
vload <- hiv
summary(vload$diff_vload) #only 19 missing
vload <- vload[is.na(vload$diff_vload) == FALSE, ]
  
leu3n <- hiv
summary(leu3n$diff_leu3n) #only 19 missing
leu3n <- leu3n[is.na(leu3n$diff_leu3n) == FALSE, ]

aggment <- hiv
summary(aggment$diff_aggment) #only 7 missing
aggment <- aggment[is.na(aggment$diff_aggment) == FALSE, ]

aggphys <- hiv
summary(aggphys$diff_aggphys)
aggphys <- aggphys[is.na(aggphys$diff_aggphys) == FALSE, ]


#=============================================================#
# Summarize rest of cont. variables to get rid of NA coding
#=============================================================#

summary(hiv$BMI)
hiv$BMI[hiv$BMI == -1] <- NA
hiv$BMI[hiv$BMI == 999] <- NA
#still have a maximum of 514.25-- changing to NA for data entry
hiv$BMI[hiv$BMI > 500] <- NA
#all seem probable, even if some are fairly low or high
boxplot(hiv$BMI, horizontal = TRUE)

summary(hiv$age)
#all seem probable


#=============================================================#
# Create table 1: demographics
#=============================================================#

#need rows for age (1), bmi (1), hard_drugs (3), race_cat (3),
  #drink_cat (3), smoke_cat (3), income_cat (4), educ_cat (3),
  #ADH (3), baseline vload (1), baseline leu3n (1),
  #baseline aggment (1), baseline aggphys (1)
demtab <- matrix(data = NA, nrow = 25, ncol = 2)

demtab[1, 1] <- "Age at baseline"
demtab[1, 2] <- paste(round(mean(hiv$age), 2), "±", round(sd(hiv$age), 2))

demtab[2, 1] <- "BMI at baseline"
demtab[2, 2] <- paste(round(mean(hiv$BMI, na.rm = T), 2), "±", round(sd(hiv$BMI, na.rm = T), 2))

demtab[3, 1] <- "Hard drug use at baseline"
demtab[4:5, 1] <- levels(hiv$hard_drugs)
demtab[4, 2] <- paste(nrow(hiv[hiv$hard_drugs == "No", ]), paste("(", round(nrow(hiv[hiv$hard_drugs == "No", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[5, 2] <- paste(nrow(hiv[hiv$hard_drugs == "Yes", ]), paste("(", round(nrow(hiv[hiv$hard_drugs == "Yes", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 

demtab[6, 1] <- "Alcohol use at baseline"
demtab[7:8, 1] <- levels(hiv$drink_cat)
demtab[7, 2] <- paste(nrow(hiv[hiv$drink_cat == "13 or fewer drinks per week", ]), 
                      paste("(", round(nrow(hiv[hiv$drink_cat == "13 or fewer drinks per week", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[8, 2] <- paste(nrow(hiv[hiv$drink_cat == "> 13 drinks per week", ]), 
                      paste("(", round(nrow(hiv[hiv$drink_cat == "> 13 drinks per week", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 

demtab[9, 1] <- "Smoking status at baseline"
demtab[10:11, 1] <- levels(hiv$smoke_cat)
demtab[10, 2] <- paste(nrow(hiv[hiv$smoke_cat == "Never/former", ]), 
                      paste("(", round(nrow(hiv[hiv$smoke_cat == "Never/former", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[11, 2] <- paste(nrow(hiv[hiv$smoke_cat == "Current", ]), 
                      paste("(", round(nrow(hiv[hiv$smoke_cat == "Current", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 

demtab[12, 1] <- "Income level at baseline"
demtab[13:15, 1] <- levels(hiv$income_cat)
inc <- cbind.data.frame(hiv$income_cat, hiv$newid)
inc <- inc[is.na(inc$`hiv$income_cat`) == FALSE, ]
colnames(inc) <- c("income_cat", "newid")
demtab[13, 2] <- paste(nrow(inc[inc$income_cat == "< $10,000", ]), 
                       paste("(", round(nrow(inc[inc$income_cat == "< $10,000", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[14, 2] <- paste(nrow(inc[inc$income_cat == "$10,000 - $40,000", ]), 
                       paste("(", round(nrow(inc[inc$income_cat == "$10,000 - $40,000", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[15, 2] <- paste(nrow(inc[inc$income_cat == "> $40,000", ]), 
                       paste("(", round(nrow(inc[inc$income_cat == "> $40,000", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 

demtab[16, 1] <- "Education at baseline"
demtab[17:18, 1] <- levels(hiv$educ_cat)
demtab[17, 2] <- paste(nrow(hiv[hiv$educ_cat == "HS or less", ]), 
                       paste("(", round(nrow(hiv[hiv$educ_cat == "HS or less", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[18, 2] <- paste(nrow(hiv[hiv$educ_cat == ">HS", ]), 
                       paste("(", round(nrow(hiv[hiv$educ_cat == ">HS", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 

demtab[19, 1] <- "Adherence at 2 years"
demtab[20:21, 1] <- levels(hiv$adh_cat_2yr)
demtab[20, 2] <- paste(nrow(hiv[hiv$adh_cat_2yr == "<95%", ]), 
                       paste("(", round(nrow(hiv[hiv$adh_cat_2yr == "<95%", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[21, 2] <-paste(nrow(hiv[hiv$adh_cat_2yr == ">95%", ]), 
                      paste("(", round(nrow(hiv[hiv$adh_cat_2yr == ">95%", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 

demtab[22, 1] <- "Baseline viral load"
demtab[22, 2] <- paste(round(mean(vload$VLOAD), 2), "±", round(sd(vload$VLOAD), 2))
  
demtab[23, 1] <- "Baseline CD4+ count"
demtab[23, 2] <- paste(round(mean(leu3n$LEU3N), 2), "±", round(sd(leu3n$LEU3N), 2))

demtab[24, 1] <- "Baseline AGG_MENT score"
demtab[24, 2] <- paste(round(mean(aggment$AGG_MENT), 2), "±", round(sd(aggment$AGG_MENT), 2))

demtab[25, 1] <- "Baselin AGG_PHYS score"
demtab[25, 2] <- paste(round(mean(aggphys$AGG_PHYS), 2), "±", round(sd(aggphys$AGG_PHYS), 2))


#=============================================================#
# Create table 2: outcomes
#=============================================================#

outtab <- matrix(data = NA, nrow = 4, ncol = 2)

outtab[1, 1] <- "Difference in VLOAD"
outtab[1, 2] <- paste(round(mean(hiv$diff_vload, na.rm = T), 2), 
                      "±", round(sd(hiv$diff_vload, na.rm = T), 2))

outtab[2, 1] <- "Difference in CD4+ count"
outtab[2, 2] <- paste(round(mean(hiv$diff_leu3n, na.rm = T), 2), 
                      "±", round(sd(hiv$diff_leu3n, na.rm = T), 2))

outtab[3, 1] <- "Difference in AGG_MENT score"
outtab[3, 2] <- paste(round(mean(hiv$diff_aggment, na.rm = T), 2), "±", 
                      round(sd(hiv$diff_aggment, na.rm = T), 2))

outtab[4, 1] <- "Difference in AGG_PHYS score"
outtab[4, 2] <- paste(round(mean(hiv$diff_aggphys, na.rm = T), 2), "±", 
                      round(sd(hiv$diff_aggphys, na.rm = T), 2))