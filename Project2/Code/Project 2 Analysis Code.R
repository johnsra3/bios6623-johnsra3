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
# Fix BMI calculation issues
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
# Fix proced data
#==========================================================#

vadata$proced[vadata$proced == 2] <- NA

#==========================================================#
# Explore other data
#==========================================================#

names(vadata)
summary(vadata)
#missing data: <2% missing in proced, asa, weight, height, BMI
  #largest missing data issue: albumin (missing 13241, ~50%)

#Hosp code
table(vadata$hospcode)
length(unique(vadata$hospcode))

#Sixmonth
table(vadata$sixmonth)
length(unique(vadata$sixmonth))

#Proced
table(vadata$proced)

#ASA
table(vadata$asa)

#Weight- won't model, but look at numbers
summary(vadata$weight)
hist(vadata$weight)

#Height
summary(vadata$height)
hist(vadata$height)

#BMI
summary(vadata$bmi)
hist(vadata$bmi)


#Albumin
hist(vadata$albumin)
summary(vadata$albumin)
#Normal range is 3.4-5.4; look at relationship w/ asa and unusually low or high vals?

plot(vadata$asa, vadata$albumin)
#Should be okay, lowest albumin meas. are w/ sickest people

#Death30
table(vadata$death30)
length(vadata$death30[vadata$death30 == 1])/nrow(vadata) * 100
#about 3.27% death rate  



#==========================================================#
# Look at missing data for ALBUMIN (primary case)
#==========================================================#
# Don't need height and weight in analysis
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

summary(alb_miss$bmi) #some v. v. low measures, but are correct
summary(alb_pres$bmi) #some v. v. low measures, but are correct
#very similar

summary(alb_miss$death30)
summary(alb_pres$death30)
#very similar



#Is anyone else going to look at other missing data?

#==========================================================#
# Look at missing data for PROCED (only missing 549)
#==========================================================#
#==========================================================#
# Look at missing data for ASA (only missing 664)
#==========================================================#
#==========================================================#
# Look at missing data for BMI (only missing 702)
#==========================================================#


#==========================================================#
# Make general table 1 (no stratifications)
#==========================================================#

tabdata <- vadata
tabdata <- tabdata[, -c(which(colnames(tabdata) == "weight"),
                       which(colnames(tabdata) == "height"),
                       which(colnames(tabdata) == "sixmonth"))]
names(tabdata)
rows <- 1 + 1 + 3 + 6 + 2 + 3 + 1
tab <- matrix(data = NA, nrow = rows, ncol = 2)
colnames(tab) <- c("", "")

tab[1, 1] <- "Hospitals (n)"
tab[1, 2] <- length(unique(tabdata$hospcode))

tab[2, 1] <- "Patients undergoing heart surgery (n)"
tab[2, 2] <- nrow(tabdata)

tab[3, 1] <- "Procedure (n (%))"
tab[4:5, 1] <- c("Valve surgery", "CABG surgery")
tab[4, 2] <- paste(nrow(tabdata[tabdata$proced == 0, ]), paste("(",
                   round(nrow(tabdata[tabdata$proced == 0, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
tab[5, 2] <- paste(nrow(tabdata[tabdata$proced == 1, ]), paste("(",
                   round(nrow(tabdata[tabdata$proced == 1, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[6, 1] <- "ASA (n (%))"
tab[7:12, 1] <- c("1 = good health", "2", "3", "4", "5 = near death", "Missing")
tab[7, 2] <- paste(nrow(tabdata[tabdata$asa == 1 & is.na(tabdata$asa) == F, ]), paste("(",
                  round(nrow(tabdata[tabdata$asa == 1 & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
tab[8, 2] <- paste(nrow(tabdata[tabdata$asa == 2 & is.na(tabdata$asa) == F, ]), paste("(",
                  round(nrow(tabdata[tabdata$asa == 2 & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
tab[9, 2] <- paste(nrow(tabdata[tabdata$asa == 3 & is.na(tabdata$asa) == F, ]), paste("(",
                  round(nrow(tabdata[tabdata$asa == 3 & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
tab[10, 2] <- paste(nrow(tabdata[tabdata$asa == 4 & is.na(tabdata$asa) == F, ]), paste("(",
                  round(nrow(tabdata[tabdata$asa == 4 & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
tab[11, 2] <- paste(nrow(tabdata[tabdata$asa == 5 & is.na(tabdata$asa) == F, ]), paste("(",
                  round(nrow(tabdata[tabdata$asa == 5 & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
tab[12, 2] <- paste(nrow(tabdata[is.na(tabdata$asa) == T, ]), paste("(",
                  round(nrow(tabdata[is.na(tabdata$asa) == T, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[13:14, 1] <- c("BMI (median (IQR))", "Missing (n (%))")
tab[13, 2] <- paste(round(median(tabdata$bmi, na.rm = T), 2), paste("(",
                    round(IQR(tabdata$bmi, na.rm = T), 2), ")", sep = ""))
tab[14, 2] <- paste(nrow(tabdata[is.na(tabdata$bmi) == T, ]), paste("(",
                    round(nrow(tabdata[is.na(tabdata$bmi) == T, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[15:16, 1] <- c("Albumin (median (IQR))", "Missing (n (%))")
tab[15, 2] <- paste(round(median(tabdata$albumin, na.rm = T), 2), paste("(",
                   round(IQR(tabdata$albumin, na.rm = T), 2), ")", sep = ""))
tab[16, 2] <- paste(nrow(tabdata[is.na(tabdata$albumin) == T, ]), paste("(",
                   round(nrow(tabdata[is.na(tabdata$albumin) == T, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[17, 1] <- "30 day mortality (n (%))"
tab[17, 2] <- paste(nrow(tabdata[tabdata$death30 == 1 & is.na(tabdata$asa) == F, ]), paste("(",
                    round(nrow(tabdata[tabdata$death30 == 1 & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
 
# setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
# write.csv(tab, "TableOverallCharacteristics.csv")


#==========================================================#
# Make table 2- death rate by hospital last 6 mo
#==========================================================#

tab2 <- matrix(data = NA, nrow = length(unique(rec$hospcode)), ncol = 4)
colnames(tab2) <- c("Hospital", "Patients died", "Patients in surgery", "Percent died")

rec <- rec[order(rec$hospcode), c(which(colnames(rec) == "hospcode"),
                                  which(colnames(rec) == "death30"))]
tab2[1:44, 1] <- unique(rec$hospcode)
tab2[1:44, 2] <- aggregate(rec$death30, list(rec$hospcode), sum)[, 2]
tab2[1:44, 3] <- aggregate(rec$death30, list(rec$hospcode), length)[, 2]
tab2[1:44, 4] <- round(tab2[, 2]/tab2[, 3] * 100, 2)

# setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
# write.csv(tab2, "TableDeathsByHospital.csv")

