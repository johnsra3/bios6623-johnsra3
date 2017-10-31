#==========================================================#
# Project 2 Data Cleaning
# Purpose: Clean data, save cleaned .csv file
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data and load libraries
#==========================================================#

library(haven)

setwd("~/School/AdvancedData")
vadata <- read_sas("~/School/AdvancedData/vadata2.sas7bdat")


#==========================================================#
# Divide into different data sets (will use later)
#==========================================================#

rec <- vadata[vadata$sixmonth == 39, ]
old <- vadata[vadata$sixmonth != 39, ]

#==========================================================#
# Look at BMI issues in most recent period
#==========================================================#

plot((rec$weight)/(rec$height^2) * 703, rec$bmi)
abline(1, 1)

hosp1_5 <- rec[rec$hospcode >= 1 & rec$hospcode < 6, ]
plot((hosp1_5$weight)/(hosp1_5$height^2) * 703, hosp1_5$bmi)
abline(1, 1)
#All 5 of these hosp code BMIs are wrong (1 extraneous)

hosp6_10 <- rec[rec$hospcode > 5 & rec$hospcode < 11, ]
plot((hosp6_10$weight)/(hosp6_10$height^2) * 703, hosp6_10$bmi)
abline(1, 1)
#All 5 of these hosp code BMIs are wrong (0 extraneous)

hosp11_15 <- rec[rec$hospcode > 10 & rec$hospcode < 16, ]
plot((hosp11_15$weight)/(hosp11_15$height^2) * 703, hosp11_15$bmi)
abline(1, 1)
#All 5 of these hosp code BMIs are wrong (0 extraneous)

hosp16 <- rec[rec$hospcode == 16, ]
plot((hosp16$weight)/(hosp16$height^2) * 703, hosp16$bmi)
abline(1, 1)
#Wrong BMI still :(


#==========================================================#
# Cleaning: fix BMI calculation issues
#==========================================================#

bmifix <- rec[rec$hospcode <= 16, ] 
#All issues w/ weight and not height
bmifix$weight <- bmifix$weight * 2.2
bmifix$bmi <- bmifix$weight/(bmifix$height^2) * 703
plot((bmifix$weight)/(bmifix$height^2) * 703, bmifix$bmi)
#One BMI is too low

#Hospital codes 17-44
hosp17plus <- rec[rec$hospcode > 16, ]
plot((hosp17plus$weight)/(hosp17plus$height^2) * 703, hosp17plus$bmi)
abline(1, 1)
#All okay for calculation, 4 extraneous need to be fixed

hosp17plus$bmi_check <- (hosp17plus$weight)/(hosp17plus$height^2) * 703
hosp17plus$diff <- round(hosp17plus$bmi - hosp17plus$bmi_check, 3)
plot(hosp17plus$bmi, hosp17plus$bmi_check)
diffs <- hosp17plus[hosp17plus$diff != 0 & is.na(hosp17plus$diff) == F, ]
hosp17plus$bmi <- hosp17plus$bmi_check
hosp17plus <- hosp17plus[, -c(which(colnames(hosp17plus) == "bmi_check"),
                              which(colnames(hosp17plus) == "diff"))]

#Re-bind hospital code divided data
rec_new <- rbind.data.frame(bmifix, hosp17plus)
plot((rec_new$weight)/(rec_new$height^2) * 703, rec_new$bmi)
abline(1, 1)
#All good now!

#Re-bind old and new data
vadata <- rbind.data.frame(rec_new, old)


#==========================================================#
# Cleaning: remove proced = 2, 
#==========================================================#

proc2 <- which(vadata$proced == 2)
vadata <- vadata[-proc2, ]

#==========================================================#
# Cleaning: collapse ASA categories
#==========================================================#

table(vadata$asa)
#some small categories--see if deaths in all groups
table(vadata$death30, vadata$asa)
#no deaths in asa=1; acc. to investigator, categ. 1/2/3 vs. 4/5
#note: 664 are missing

#Create indicator variable
vadata$asa_indic <- ifelse(vadata$asa >= 4 & is.na(vadata$asa) == F, 1, 0)

#Collapse categories
vadata$asa <- ifelse(vadata$asa <= 3, "3 or less", vadata$asa)
vadata$asa <- ifelse(vadata$asa >=4, "4 or greater", vadata$asa)
vadata$asa <- factor(vadata$asa, levels = c("3 or less", "4 or greater"))

#==========================================================#
# Write csv of cleaned data
#==========================================================#

setwd("~/School/AdvancedData")
write.csv(vadata, "VadataCleaned.csv")
