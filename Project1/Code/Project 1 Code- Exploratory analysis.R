#=============================================================#
# Project 1
# Code by: Rachel Johnson
# Started: 09/20/2017
# Last updated: 09/25/2017
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
extra <- hiv

###########
# Create difference variables

hiv$diff_leu3n <- hiv$LEU3N_2yr - hiv$LEU3N
hiv$diff_aggment <- hiv$AGG_MENT_2yr - hiv$AGG_MENT
hiv$diff_aggphys <- hiv$AGG_PHYS_2yr - hiv$AGG_PHYS

#Log10 transform vload and then find difference
hiv$logvload <- log10(hiv$VLOAD)
hiv$logvload_2yr <- log10(hiv$VLOAD_2yr)
boxplot(hiv$logvload, horizontal = TRUE)
boxplot(hiv$logvload_2yr, horizontal = TRUE)

hiv$diff_logvload <- hiv$logvload_2yr/hiv$logvload

##########
test <- hiv
table(test$hard_drugs) #467 no, 39 yes
test <- test[is.na(test$diff_aggment) == FALSE, ]
test <- test[is.na(test$diff_aggphys) == FALSE, ]
test <- test[is.na(test$diff_leu3n) == FALSE, ]
test <- test[is.na(test$diff_logvload) == FALSE, ]
table(test$hard_drugs) #439 no, 39 yes-- doesn't affect # of ppl who yes hard drugs
hiv <- test


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

############
# Make dataset for drugyes and drugno
hivyes <- hiv[hiv$hard_drugs == "Yes", ]
hivno <- hiv[hiv$hard_drugs == "No", ]

#need rows for age (1), bmi (1), hard_drugs (3), race_cat (3),
  #drink_cat (3), smoke_cat (3), income_cat (4), educ_cat (3),
  #ADH (3), baseline vload (1), baseline leu3n (1),
  #baseline aggment (1), baseline aggphys (1)
demtab <- matrix(data = NA, nrow = 22, ncol = 4)
colnames(demtab) <- c("", "Total", "Hard drugs = Yes", "Hard drugs = No")

demtab[1, 1] <- "Age at baseline"
demtab[1, 2] <- paste(round(mean(hiv$age), 2), "±", round(sd(hiv$age), 2))
demtab[1, 3] <- paste(round(mean(hivyes$age), 2), "±", round(sd(hivyes$age), 2))
demtab[1, 4] <- paste(round(mean(hivno$age), 2), "±", round(sd(hivno$age), 2))

demtab[2, 1] <- "BMI at baseline"
demtab[2, 2] <- paste(round(mean(hiv$BMI, na.rm = T), 2), "±", round(sd(hiv$BMI, na.rm = T), 2))
demtab[2, 3] <- paste(round(mean(hivyes$BMI, na.rm = T), 2), "±", round(sd(hivyes$BMI, na.rm = T), 2))
demtab[2, 4] <- paste(round(mean(hivno$BMI, na.rm = T), 2), "±", round(sd(hivno$BMI, na.rm = T), 2))

demtab[3, 1] <- "Alcohol use at baseline"
demtab[4:5, 1] <- levels(hiv$drink_cat)
demtab[4, 2] <- paste(nrow(hiv[hiv$drink_cat == "13 or fewer drinks per week", ]), 
                      paste("(", round(nrow(hiv[hiv$drink_cat == "13 or fewer drinks per week", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[5, 2] <- paste(nrow(hiv[hiv$drink_cat == "> 13 drinks per week", ]), 
                      paste("(", round(nrow(hiv[hiv$drink_cat == "> 13 drinks per week", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[4, 3] <- paste(nrow(hivyes[hivyes$drink_cat == "13 or fewer drinks per week", ]), 
                      paste("(", round(nrow(hivyes[hivyes$drink_cat == "13 or fewer drinks per week", ])/nrow(hivyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[5, 3] <- paste(nrow(hivyes[hivyes$drink_cat == "> 13 drinks per week", ]), 
                      paste("(", round(nrow(hivyes[hivyes$drink_cat == "> 13 drinks per week", ])/nrow(hivyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[4, 4] <- paste(nrow(hivno[hivno$drink_cat == "13 or fewer drinks per week", ]), 
                      paste("(", round(nrow(hivno[hivno$drink_cat == "13 or fewer drinks per week", ])/nrow(hivno) * 100, 2), ")", sep = ""), sep = " ") 
demtab[5, 4] <- paste(nrow(hivno[hivno$drink_cat == "> 13 drinks per week", ]), 
                      paste("(", round(nrow(hivno[hivno$drink_cat == "> 13 drinks per week", ])/nrow(hivno) * 100, 2), ")", sep = ""), sep = " ") 

demtab[6, 1] <- "Smoking status at baseline"
demtab[7:8, 1] <- levels(hiv$smoke_cat)
demtab[7, 2] <- paste(nrow(hiv[hiv$smoke_cat == "Never/former", ]), 
                      paste("(", round(nrow(hiv[hiv$smoke_cat == "Never/former", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[8, 2] <- paste(nrow(hiv[hiv$smoke_cat == "Current", ]), 
                      paste("(", round(nrow(hiv[hiv$smoke_cat == "Current", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[7, 3] <- paste(nrow(hivyes[hivyes$smoke_cat == "Never/former", ]), 
                      paste("(", round(nrow(hivyes[hivyes$smoke_cat == "Never/former", ])/nrow(hivyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[8, 3] <- paste(nrow(hivyes[hivyes$smoke_cat == "Current", ]), 
                      paste("(", round(nrow(hivyes[hivyes$smoke_cat == "Current", ])/nrow(hivyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[7, 4] <- paste(nrow(hivno[hivno$smoke_cat == "Never/former", ]), 
                      paste("(", round(nrow(hivno[hivno$smoke_cat == "Never/former", ])/nrow(hivno) * 100, 2), ")", sep = ""), sep = " ") 
demtab[8, 4] <- paste(nrow(hivno[hivno$smoke_cat == "Current", ]), 
                      paste("(", round(nrow(hivno[hivno$smoke_cat == "Current", ])/nrow(hivno) * 100, 2), ")", sep = ""), sep = " ") 

demtab[9, 1] <- "Income level at baseline"
demtab[10:12, 1] <- levels(hiv$income_cat)
inc <- cbind.data.frame(hiv$income_cat, hiv$newid, hiv$hard_drugs)
inc <- inc[is.na(inc$`hiv$income_cat`) == FALSE, ]
colnames(inc) <- c("income_cat", "newid", "hard_drugs")
incyes <- inc[inc$hard_drugs == "Yes", ]
incno <- inc[inc$hard_drugs == "No", ]

demtab[10, 2] <- paste(nrow(inc[inc$income_cat == "< $10,000", ]), 
                       paste("(", round(nrow(inc[inc$income_cat == "< $10,000", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[11, 2] <- paste(nrow(inc[inc$income_cat == "$10,000 - $40,000", ]), 
                       paste("(", round(nrow(inc[inc$income_cat == "$10,000 - $40,000", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[12, 2] <- paste(nrow(inc[inc$income_cat == "> $40,000", ]), 
                       paste("(", round(nrow(inc[inc$income_cat == "> $40,000", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[10, 3] <- paste(nrow(incyes[incyes$income_cat == "< $10,000", ]), 
                       paste("(", round(nrow(incyes[incyes$income_cat == "< $10,000", ])/nrow(incyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[11, 3] <- paste(nrow(incyes[incyes$income_cat == "$10,000 - $40,000", ]), 
                       paste("(", round(nrow(incyes[incyes$income_cat == "$10,000 - $40,000", ])/nrow(incyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[12, 3] <- paste(nrow(incyes[incyes$income_cat == "> $40,000", ]), 
                       paste("(", round(nrow(incyes[incyes$income_cat == "> $40,000", ])/nrow(incyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[10, 4] <- paste(nrow(incno[incno$income_cat == "< $10,000", ]), 
                     paste("(", round(nrow(incno[incno$income_cat == "< $10,000", ])/nrow(incno) * 100, 2), ")", sep = ""), sep = " ") 
demtab[11, 4] <- paste(nrow(incno[incno$income_cat == "$10,000 - $40,000", ]), 
                       paste("(", round(nrow(incno[incno$income_cat == "$10,000 - $40,000", ])/nrow(incno) * 100, 2), ")", sep = ""), sep = " ") 
demtab[12, 4] <- paste(nrow(incno[incno$income_cat == "> $40,000", ]), 
                       paste("(", round(nrow(incno[incno$income_cat == "> $40,000", ])/nrow(incno) * 100, 2), ")", sep = ""), sep = " ") 

demtab[13, 1] <- "Education at baseline"
demtab[14:15, 1] <- levels(hiv$educ_cat)
demtab[14, 2] <- paste(nrow(hiv[hiv$educ_cat == "HS or less", ]), 
                       paste("(", round(nrow(hiv[hiv$educ_cat == "HS or less", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[15, 2] <- paste(nrow(hiv[hiv$educ_cat == ">HS", ]), 
                       paste("(", round(nrow(hiv[hiv$educ_cat == ">HS", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[14, 3] <- paste(nrow(hivyes[hivyes$educ_cat == "HS or less", ]), 
                       paste("(", round(nrow(hivyes[hivyes$educ_cat == "HS or less", ])/nrow(hivyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[15, 3] <- paste(nrow(hivyes[hivyes$educ_cat == ">HS", ]), 
                       paste("(", round(nrow(hivyes[hivyes$educ_cat == ">HS", ])/nrow(hivyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[14, 4] <- paste(nrow(hivno[hivno$educ_cat == "HS or less", ]), 
                       paste("(", round(nrow(hivno[hivno$educ_cat == "HS or less", ])/nrow(hivno) * 100, 2), ")", sep = ""), sep = " ") 
demtab[15, 4] <- paste(nrow(hivno[hivno$educ_cat == ">HS", ]), 
                       paste("(", round(nrow(hivno[hivno$educ_cat == ">HS", ])/nrow(hivno) * 100, 2), ")", sep = ""), sep = " ") 

demtab[16, 1] <- "Adherence at 2 years"
demtab[17:18, 1] <- levels(hiv$adh_cat_2yr)
demtab[17, 2] <- paste(nrow(hiv[hiv$adh_cat_2yr == "<95%", ]), 
                       paste("(", round(nrow(hiv[hiv$adh_cat_2yr == "<95%", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[18, 2] <-paste(nrow(hiv[hiv$adh_cat_2yr == ">95%", ]), 
                      paste("(", round(nrow(hiv[hiv$adh_cat_2yr == ">95%", ])/nrow(hiv) * 100, 2), ")", sep = ""), sep = " ") 
demtab[17, 3] <- paste(nrow(hivyes[hivyes$adh_cat_2yr == "<95%", ]), 
                       paste("(", round(nrow(hivyes[hivyes$adh_cat_2yr == "<95%", ])/nrow(hivyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[18, 3] <-paste(nrow(hivyes[hivyes$adh_cat_2yr == ">95%", ]), 
                      paste("(", round(nrow(hivyes[hivyes$adh_cat_2yr == ">95%", ])/nrow(hivyes) * 100, 2), ")", sep = ""), sep = " ") 
demtab[17, 4] <- paste(nrow(hivno[hivno$adh_cat_2yr == "<95%", ]), 
                       paste("(", round(nrow(hivno[hivno$adh_cat_2yr == "<95%", ])/nrow(hivno) * 100, 2), ")", sep = ""), sep = " ") 
demtab[18, 4] <- paste(nrow(hivno[hivno$adh_cat_2yr == ">95%", ]), 
                      paste("(", round(nrow(hivno[hivno$adh_cat_2yr == ">95%", ])/nrow(hivno) * 100, 2), ")", sep = ""), sep = " ") 

demtab[19, 1] <- "Baseline log10 viral load"
demtab[19, 2] <- paste(round(mean(hiv$logvload), 2), "±", round(sd(hiv$logvload), 2))
demtab[19, 3] <- paste(round(mean(hivyes$logvload), 2), "±", round(sd(hivyes$logvload), 2))
demtab[19, 4] <- paste(round(mean(hivno$logvload), 2), "±", round(sd(hivno$logvload), 2))

demtab[20, 1] <- "Baseline CD4+ count"
demtab[20, 2] <- paste(round(mean(hiv$LEU3N), 2), "±", round(sd(hiv$LEU3N), 2))
demtab[20, 3] <- paste(round(mean(hivyes$LEU3N), 2), "±", round(sd(hivyes$LEU3N), 2))
demtab[20, 4] <- paste(round(mean(hivno$LEU3N), 2), "±", round(sd(hivno$LEU3N), 2))

demtab[21, 1] <- "Baseline SF36 MCS score"
demtab[21, 2] <- paste(round(mean(hiv$AGG_MENT), 2), "±", round(sd(hiv$AGG_MENT), 2))
demtab[21, 3] <- paste(round(mean(hivyes$AGG_MENT), 2), "±", round(sd(hivyes$AGG_MENT), 2))
demtab[21, 4] <- paste(round(mean(hivno$AGG_MENT), 2), "±", round(sd(hivno$AGG_MENT), 2))

demtab[22, 1] <- "Baseline SF36 PCS score"
demtab[22, 2] <- paste(round(mean(hiv$AGG_PHYS), 2), "±", round(sd(hiv$AGG_PHYS), 2))
demtab[22, 3] <- paste(round(mean(hivyes$AGG_PHYS), 2), "±", round(sd(hivyes$AGG_PHYS), 2))
demtab[22, 4] <- paste(round(mean(hivno$AGG_PHYS), 2), "±", round(sd(hivno$AGG_PHYS), 2))


#setwd("C:/Repositories/bios6623-johnsra3/Project1/Reports")
#write.csv(demtab, "DemographicsTable09272017.csv")


#=============================================================#
# Create table 2: outcomes
#=============================================================#

outtab <- matrix(data = NA, nrow = 4, ncol = 4)
colnames(outtab) <- c("", "Total", "Hard drugs = Yes", "Hard drugs = No")

outtab[1, 1] <- "Difference in log10 viral load"
outtab[1, 2] <- paste(round(mean(hiv$diff_logvload, na.rm = T), 2), 
                      "±", round(sd(hiv$diff_logvload, na.rm = T), 2))
outtab[1, 3] <- paste(round(mean(hivyes$diff_logvload, na.rm = T), 2), 
                      "±", round(sd(hivyes$diff_logvload, na.rm = T), 2))
outtab[1, 4] <- paste(round(mean(hivno$diff_logvload, na.rm = T), 2), 
                      "±", round(sd(hivno$diff_logvload, na.rm = T), 2))

outtab[2, 1] <- "Difference in CD4+ count"
outtab[2, 2] <- paste(round(mean(hiv$diff_leu3n, na.rm = T), 2), 
                      "±", round(sd(hiv$diff_leu3n, na.rm = T), 2))
outtab[2, 3] <- paste(round(mean(hivyes$diff_leu3n, na.rm = T), 2), 
                      "±", round(sd(hivyes$diff_leu3n, na.rm = T), 2))
outtab[2, 4] <- paste(round(mean(hivno$diff_leu3n, na.rm = T), 2), 
                      "±", round(sd(hivno$diff_leu3n, na.rm = T), 2))

outtab[3, 1] <- "Difference in SF36 MCS score"
outtab[3, 2] <- paste(round(mean(hiv$diff_aggment, na.rm = T), 2), "±", 
                      round(sd(hiv$diff_aggment, na.rm = T), 2))
outtab[3, 3] <- paste(round(mean(hivyes$diff_aggment, na.rm = T), 2), "±", 
                      round(sd(hivyes$diff_aggment, na.rm = T), 2))
outtab[3, 4] <- paste(round(mean(hivno$diff_aggment, na.rm = T), 2), "±", 
                      round(sd(hivno$diff_aggment, na.rm = T), 2))

outtab[4, 1] <- "Difference in SF36 PCS score"
outtab[4, 2] <- paste(round(mean(hiv$diff_aggphys, na.rm = T), 2), "±", 
                      round(sd(hiv$diff_aggphys, na.rm = T), 2))
outtab[4, 3] <- paste(round(mean(hivyes$diff_aggphys, na.rm = T), 2), "±", 
                      round(sd(hivyes$diff_aggphys, na.rm = T), 2))
outtab[4, 4] <- paste(round(mean(hivno$diff_aggphys, na.rm = T), 2), "±", 
                      round(sd(hivno$diff_aggphys, na.rm = T), 2))

setwd("C:/Repositories/bios6623-johnsra3/Project1/Reports")
write.csv(outtab, "DifferenceOutcomesTable09272017.csv")


#=============================================================#
# Create graphs for outcome differences
#=============================================================#

par(mfrow = c(2, 2))
boxplot(hiv$diff_logvload ~ hiv$hard_drugs, ylab = "Difference in Viral Load",
        main = "Difference in log10 Viral Load \nby Hard Drug Use")
boxplot(hiv$diff_leu3n ~ hiv$hard_drugs, ylab = "Difference in CD4+ Count",
        main = "Difference in CD4+ Count \nby Hard Drug Use")
boxplot(hiv$diff_aggment ~ hiv$hard_drugs, ylab = "Difference in SF36 MCS Score",
        main = "Difference in SF36 MCS Score \nby Hard Drug Use")
boxplot(hiv$diff_aggphys ~ hiv$hard_drugs, ylab = "Difference in SF36 PCS Score",
        main = "Difference in SF36 PCS Score \nby Hard Drug Use")


#=============================================================#
# Make dataset of indicator variables to import into SAS
#=============================================================#

names(extra)
names(hiv)
indic <- cbind.data.frame(extra, hiv[, 20:25])
names(indic)

indic <- indic[, -c(which(colnames(indic) == "AGG_MENT_2yr"),
                    which(colnames(indic) == "AGG_PHYS_2yr"),
                    which(colnames(indic) == "LEU3N_2yr"),
                    which(colnames(indic) == "VLOAD_2yr"),
                    which(colnames(indic) == "ADH"))]
names(indic)

summary(indic$hard_drugs)
indic$harddrugsY <- NA
indic$harddrugsY <- ifelse(indic$hard_drugs == "Yes", 1, 0)
indic <- indic[, -which(colnames(indic) == "hard_drugs")]

indic$raceNHW <- NA
indic$raceNHW <- ifelse(indic$RACE_cat == "Non-Hispanic White", 1, 0)
indic <- indic[, -which(colnames(indic) == "RACE_cat")]

indic$drink13plus <- NA
indic$drink13plus <- ifelse(indic$drink_cat == "> 13 drinks per week", 1, 0)
indic <- indic[, -which(colnames(indic) == "drink_cat")]

indic$smokecurrent <- NA
indic$smokecurrent <- ifelse(indic$smoke_cat == "Current", 1, 0)
indic <- indic[, -which(colnames(indic) == "smoke_cat")]

indic$incomehigh <- NA
indic$incomehigh <- ifelse(indic$income_cat == "> $40,000", 1, 0)
indic$incomemed <- NA 
indic$incomemed <- ifelse(indic$income_cat == "$10,000 - $40,000", 1, 0)
indic <- indic[, -which(colnames(indic) == "income_cat")]

indic$educHSmore <- NA
indic$educHSmore <- ifelse(indic$educ_cat == ">HS", 1, 0)
indic <- indic[, -which(colnames(indic) == "educ_cat")]

indic$adhhigh <- NA
indic$adhhigh <- ifelse(indic$adh_cat_2yr == ">95%", 1, 0)
indic <- indic[, -which(colnames(indic) == "adh_cat_2yr")]

setwd("~/School/AdvancedData")
write.csv(indic, "IndicatorHIVdata09252017.csv")
