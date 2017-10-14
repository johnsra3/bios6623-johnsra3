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
# Fix issues in data (ex: impossible BMI values)
#==========================================================#

#remove ones w/ proced = 2
vadata <- vadata[-which(vadata$proced == 2), ]

#bmi- look at unusually high/low vals
(lowbmi <- vadata[vadata$bmi < 15 & is.na(vadata$bmi) == F, ])
#Row 1, 2, 3, 5, 6, 7, 8 look like weight is in kg (not lbs)
  #Row 4 looks like BMI calculation issue
(lowbmi <- lowbmi[-4, ])
lowbmi$weight_lbs <- lowbmi$weight * 2.2
lowbmi$bmi_calc <- lowbmi$weight_lbs/(lowbmi$height^2) * 703

(lowbmi_row4 <- vadata[vadata$bmi < 3 & is.na(vadata$bmi) == F, ])
(lowbmi_row4$bmi_calc <- lowbmi_row4$weight/(lowbmi_row4$height^2) * 703)

(highbmi <- vadata[vadata$bmi > 50 & is.na(vadata$bmi) == F, ])
#Looks like BMI calculation error
(highbmi$bmi_calc <- highbmi$weight/(highbmi$height^2) * 703)

#Remove bad BMI, then fix and rebind vadata, lowbmi, lowbmi_row4, highbmi
vadata <- vadata[vadata$bmi > 15 & vadata$bmi < 50, ]
#
lowbmi$bmi <- lowbmi$bmi_calc
lowbmi$weight <- lowbmi$weight_lbs
lowbmi <- lowbmi[, -c(which(colnames(lowbmi) == "bmi_calc"),
                      which(colnames(lowbmi) == "weight_lbs"))]
#
lowbmi_row4$bmi <- lowbmi_row4$bmi_calc
lowbmi_row4 <- lowbmi_row4[, -which(colnames(lowbmi_row4) == "bmi_calc")]
#
highbmi$bmi <- highbmi$bmi_calc
highbmi <- highbmi[, -which(colnames(highbmi) == "bmi_calc")]

vadata <- rbind.data.frame(vadata, lowbmi, lowbmi_row4, highbmi)


#==========================================================#
# Explore data
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
#some unusually low weights
hist(vadata$weight)

#Height
summary(vadata$height)
hist(vadata$height)

#BMI
summary(vadata$bmi)
#some unusually high or low BMIs... y*kes
hist(vadata$bmi)


#Albumin
hist(vadata$albumin)
summary(vadata$albumin)
#Normal range is 3.4-5.4; look at relationship w/ asa and unusually low or high vals?

plot(vadata$asa, vadata$albumin)
#Should be okay, lowest albumin meas. are w/ sickets people

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

summary(alb_miss$bmi) #some impossible measures! need to fix!
summary(alb_pres$bmi) #some impossible measures! need to fix!
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
 
# setwd("C:/Repositories/bios6623-johnsra3/Project1/Reports")
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

