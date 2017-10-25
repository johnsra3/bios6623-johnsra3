#==========================================================#
# Project 2
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data and load libraries
#==========================================================#

library(haven)
library(boot)

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
# Look at missing data for ALBUMIN 
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

summary(alb_miss$height)
summary(alb_pres$height)
#very similar

summary(alb_miss$weight)
summary(alb_pres$weight)
#very similar

summary(alb_miss$bmi) #some v. v. low measures, but are correct
summary(alb_pres$bmi) #some v. v. low measures, but are correct
#very similar

table(alb_miss$death30)
table(alb_pres$death30)
#very similar


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

tab[10:11, 1] <- c("BMI (median (IQR))", "Missing (n (%))")
tab[10, 2] <- paste(round(median(tabdata$bmi, na.rm = T), 2), paste("(",
                    round(IQR(tabdata$bmi, na.rm = T), 2), ")", sep = ""))
tab[11, 2] <- paste(nrow(tabdata[is.na(tabdata$bmi) == T, ]), paste("(",
                    round(nrow(tabdata[is.na(tabdata$bmi) == T, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[12:13, 1] <- c("Albumin (median (IQR))", "Missing (n (%))")
tab[12, 2] <- paste(round(median(tabdata$albumin, na.rm = T), 2), paste("(",
                   round(IQR(tabdata$albumin, na.rm = T), 2), ")", sep = ""))
tab[13, 2] <- paste(nrow(tabdata[is.na(tabdata$albumin) == T, ]), paste("(",
                   round(nrow(tabdata[is.na(tabdata$albumin) == T, ])/nrow(tabdata) * 100, 2), ")", sep = ""))

tab[14, 1] <- "30 day mortality (n (%))"
tab[14, 2] <- paste(nrow(tabdata[tabdata$death30 == 1 & is.na(tabdata$asa) == F, ]), paste("(",
                    round(nrow(tabdata[tabdata$death30 == 1 & is.na(tabdata$asa) == F, ])/nrow(tabdata) * 100, 2), ")", sep = ""))
 
 # setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
 # write.csv(tab, "TableOverallCharacteristics.csv")


#==========================================================#
# Logistic regression 
#==========================================================#

#With albumin
model1 <- glm(death30 ~ proced + asa + bmi + albumin, data = vadata,
              family = binomial(link = "logit"))
summary(model1)

#Without albumin
model2 <- glm(death30 ~ proced + asa + bmi, data = vadata,
              family = binomial(link = "logit"))
summary(model2)

#Decisions do not change-- albumin can be excluded, but still report results


#==========================================================#
# Predicted values to individuals' odds to indivs' pred p
#==========================================================#

#Need a complete case data for BMI, procedure, ASA
comp <- vadata[, c(which(colnames(vadata) == "hospcode"),
                   which(colnames(vadata) == "sixmonth"),
                   which(colnames(vadata) == "proced"),
                   which(colnames(vadata) == "asa_indic"),
                   which(colnames(vadata) == "bmi"),
                   which(colnames(vadata) == "death30"))]
comp <- comp[complete.cases(comp), ]

comp_rec <- comp[comp$sixmonth == 39, ]

#Extract fitted values from summary of model2
(int <- summary(model2)$coefficients[1, 1])
(pred_proced <- summary(model2)$coefficients[2, 1])
(pred_asa <- summary(model2)$coefficients[3, 1])
(pred_bmi <- summary(model2)$coefficients[4, 1])

#Create XB column in comp_rec (last 6 month period only)
comp_rec$XB <- int + pred_proced*comp_rec$proced + pred_asa*comp_rec$asa_indic + pred_bmi*comp_rec$bmi

#Create predicted p column in comp
comp_rec$pred_p <- inv.logit(comp_rec$XB) 
 

#==========================================================#
# Create table 2 w/ hospital death rates
#==========================================================#

tab2 <- matrix(data = NA, nrow = length(unique(rec$hospcode)), ncol = 5)
colnames(tab2) <- c("Hospital", "Patients died", "Patients in surgery", "Percent died",
                    "Predicted percent died for last 6 months (population-adjusted)")


rec <- rec[order(rec$hospcode), c(which(colnames(rec) == "hospcode"),
                                  which(colnames(rec) == "death30"))]
tab2[1:44, 1] <- unique(rec$hospcode)
tab2[1:44, 2] <- aggregate(rec$death30, list(rec$hospcode), sum)[, 2]
tab2[1:44, 3] <- aggregate(rec$death30, list(rec$hospcode), length)[, 2]
tab2[1:44, 4] <- round(tab2[, 2]/tab2[, 3] * 100, 2)

#Note: hospital 30 does not have any data for last period, so it will not be estimated for last pd.
hosp1_29 <- as.list(unique(comp_rec$hospcode)[1:29])
p1_29 <- as.data.frame(comp_rec[comp_rec$hospcode %in% hosp1_29, c(which(colnames(comp_rec) == "pred_p"),
                                                                   which(colnames(comp_rec) == "hospcode"))])
tab2[1:29, 5] <- round(aggregate(p1_29$pred_p, list(p1_29$hospcode), mean)[, 2] * 100, 2)

hosp31_44 <- seq(from = 31, to = 44, by = 1)
p31_44 <- as.data.frame(comp_rec[comp_rec$hospcode %in% hosp31_44, c(which(colnames(comp_rec) == "pred_p"),
                                                                     which(colnames(comp_rec) == "hospcode"))])
tab2[31:44, 5] <- round(aggregate(p31_44$pred_p, list(p31_44$hospcode), mean)[, 2] * 100, 2)

# setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
# write.csv(tab2, "TableDeathsByHospital.csv")


#==========================================================#
# Write loop to bootstrap
#==========================================================#

#Need following steps:
  # 1. Sample from w/i total population w/ replacement in complete cases
  # 2. Run logistic regression w/ this resampled population
  # 3. Extract fitted values and exponentiate them 
  # 4. Calculate p_fitted for each individual in the resampled data set w/ inv.logit
  # 5. Get an average p_fitted for each hospital w/ aggregate
  # 6. Find distribution of p_fitted for each hosp, w/ 2.5% and 97.5% pieces (boot.ci)

#Place to store p_fits
num_iter <- 100
boot.stats <- matrix(data = NA, ncol = length(unique(comp$hospcode)), nrow = num_iter)
fitold <- matrix(data = NA, ncol = nrow(comp_rec), nrow = num_iter)
colnames(boot.stats) <- unique(comp$hospcode)

for(i in 1:num_iter){ 
  
  boot.samps <- sample(nrow(comp), replace = T)
  boot.dat <- comp[boot.samps, ]
  boot.model <- glm(death30 ~ proced + asa_indic + bmi, data = boot.dat, family = binomial(link = "logit"))
  coeff <- summary(boot.model)$coefficients
  
  fitold$xb <- coeff[1] + coeff[2]*comp_rec$proced + coeff[3]*comp_rec$asa_indic + coeff[4]*comp_rec$bmi
  fitold$pfit <- inv.logit(fitold$xb)
  
  boot.stats[i, ] <- round(aggregate(p_below30$pred_p, list(p_below30$hospcode), mean)[, 2] * 100, 2)
  
  print(i)

}




# http://www.r-tutor.com/elementary-statistics/numerical-measures/percentile