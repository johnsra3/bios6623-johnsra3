#==========================================================#
# Project 2
# Purpose: Create Table 1
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data 
#==========================================================#

setwd("~/School/AdvancedData")
vadata <- read.csv("VadataCleaned.csv", header = T)

#==========================================================#
# Divide into different data sets, create factor levels
#==========================================================#

rec <- vadata[vadata$sixmonth == 39, ]
old <- vadata[vadata$sixmonth != 39, ]

vadata$asa <- factor(vadata$asa, levels = c("3 or less", "4 or greater"))

#==========================================================#
# Make general table 1 (no stratifications)
#==========================================================#

tabdata <- vadata
tabdata <- tabdata[, -c(which(colnames(tabdata) == "weight"),
                        which(colnames(tabdata) == "height"),
                        which(colnames(tabdata) == "sixmonth"))]
names(tabdata)
rows <- 1 + 1 + 3 + 3 + 2 + 3 + 1
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
tab[7:9, 1] <- c("3 or less", "4 or greater", "Missing")
tab[7, 2] <- paste(nrow(tabdata[tabdata$asa == "3 or less" & is.na(tabdata$asa) == F, ]), paste("(",
                                                                                                round(nrow(tabdata[tabdata$asa == "3 or less" & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
tab[8, 2] <- paste(nrow(tabdata[tabdata$asa == "4 or greater" & is.na(tabdata$asa) == F, ]), paste("(",
                                                                                                   round(nrow(tabdata[tabdata$asa == "4 or greater" & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
tab[9, 2] <- paste(nrow(tabdata[is.na(tabdata$asa) == T, ]), paste("(",
                                                                   round(nrow(tabdata[is.na(tabdata$asa) == T, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[10:11, 1] <- c("BMI (mean (SD))", "Missing (n (%))")
tab[10, 2] <- paste(round(mean(tabdata$bmi, na.rm = T), 2), paste("(",
                                                                    round(sd(tabdata$bmi, na.rm = T), 2), ")", sep = ""))
tab[11, 2] <- paste(nrow(tabdata[is.na(tabdata$bmi) == T, ]), paste("(",
                                                                    round(nrow(tabdata[is.na(tabdata$bmi) == T, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[12:13, 1] <- c("Albumin (mean (SD))", "Missing (n (%))")
tab[12, 2] <- paste(round(mean(tabdata$albumin, na.rm = T), 2), paste("(",
                                                                        round(sd(tabdata$albumin, na.rm = T), 2), ")", sep = ""))
tab[13, 2] <- paste(nrow(tabdata[is.na(tabdata$albumin) == T, ]), paste("(",
                                                                        round(nrow(tabdata[is.na(tabdata$albumin) == T, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[14, 1] <- "30 day mortality (n (%))"
tab[14, 2] <- paste(nrow(tabdata[tabdata$death30 == 1 & is.na(tabdata$asa) == F, ]), paste("(",
                                                                                           round(nrow(tabdata[tabdata$death30 == 1 & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

# setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
# write.csv(tab, "TableOverallCharacteristics.csv")
