#==========================================================#
# Project 2
# Purpose: Create table of observed/expected/bootstrap res
# Rachel Johnson
#==========================================================#

#==========================================================#
# Import data 
#==========================================================#

setwd("~/School/AdvancedData")
vadata <- read.csv("VadataCleaned.csv", header = T)

setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
comp_rec <- read.csv("CompleteCasesPd39Predicted.csv", header = T)
boot.res <- read.csv("BootstrapResults_raw.csv", header = T)
boot.res <- boot.res[, -1]

#==========================================================#
# Divide into different data sets, create factor levels
#==========================================================#

rec <- vadata[vadata$sixmonth == 39, ]
old <- vadata[vadata$sixmonth != 39, ]

vadata$asa <- factor(vadata$asa, levels = c("3 or less", "4 or greater"))


#==========================================================#
# Get confidence intervals from raw bootstrap data
#==========================================================#

boot.ci_low <- round(apply(boot.res, 2, quantile, probs = 0.025), 2)
boot.ci_high <- round(apply(boot.res, 2, quantile, probs = 0.975), 2)
boot.ci <- paste(paste(boot.ci_low, ",", sep = ""), boot.ci_high)
hosps <- c(seq(from = 1, to = 29, by = 1), seq(from = 31, to = 44, by = 1)) 

boottab <- cbind.data.frame(hosps, boot.ci)
colnames(boottab) <- c("Hospital", "95% CI")

#==========================================================#
# Create table 2 w/ hospital death rates
#==========================================================#

tab2 <- matrix(data = NA, nrow = length(unique(rec$hospcode)), ncol = 5)
colnames(tab2) <- c("Hospital", "Patients died", "Patients in surgery", "Percent died",
                    "Predicted percent died for last 6 months (population-adjusted)")

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
# Update hospital table w/ bootstrap CIs 
#==========================================================#

tab2_first <- tab2[1:29, ]
tab2_30 <- as.data.frame(tab2[30, ])
tab2_30 <- t(tab2_30)
tab2_end <- tab2[31:44, ]

boottab_first <- boottab[1:29, ]
boottab_end <- boottab[30:43, ]

finaltab_first <- merge(tab2_first, boottab_first, by = "Hospital")

finaltab_30 <- matrix(data = NA, nrow = 1, ncol = 6)
colnames(finaltab_30) <- colnames(finaltab_first)
finaltab_30[,1:5] <- tab2_30[, 1:5]

finaltab_end <- merge(tab2_end, boottab_end, by = "Hospital")

finaltab <- rbind.data.frame(finaltab_first, finaltab_30, finaltab_end)


#==========================================================#
# Hosp observed p/expected p > 1.2
#==========================================================#

finaltab <- as.data.frame(finaltab)
finaltab$`Unusually High?` <- ifelse(finaltab$`Percent died`/finaltab$`Predicted percent died for last 6 months (population-adjusted)` > 1.2,
                                     "Yes", "No")
table(finaltab$`Unusually High?`)
#17 hospitals were unusually high

# setwd("C:/Repositories/bios6623-johnsra3/Project2/Reports")
# write.csv(finaltab, "TableExpectedPropsBootstrap.csv")
