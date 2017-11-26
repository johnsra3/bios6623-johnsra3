#==============================================================#
# Project 3
# Work with bootstrap results
# Rachel Johnson
#==============================================================#

#==============================================================#
# Import data
#==============================================================#

setwd("C:/Repositories/bios6623-johnsra3/Project3/Reports")
mmres <- read.csv("MixedModelResults.csv", header = T)
animals_bs <- read.csv("BootstrapWithAllEstimates.csv", header = T)
animals_bs <- animals_bs[, -1]

colnames(animals_bs) <-c("Changepoint","Intercept","Change Point",
                         "Gender", "SES", "Age_59", 
                         "Interaction", "Dementia",  "Slope1", "Slope2")



#==============================================================#
# Table of bootstrap results for change point 
#==============================================================#

bstab <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 4))
colnames(bstab) <- c("Variable", "Change Point (likelihood)", "Change point (bootstrap)",
                     "Bootstrapped 95% CI")

bstab[1, 1] <- c("Animals")
bstab[1, 2] <- c(-3.9)
bstab[1, 3] <- round(mean(animals_bs[, 1]), 2)  
bstab[1, 4] <- paste(paste(round(quantile(animals_bs[, 1], 0.025), 2), ",", sep = ""),
                     round(quantile(animals_bs[, 1], 0.975), 2))
bstab

write.csv(bstab, "C:/Repositories/bios6623-johnsra3/Project3/Reports/BootstrapSummaryTable.csv")


#==============================================================#
# Table of bootstrap results for updated SEs w/ model estimates
#==============================================================#

setab <- as.data.frame(matrix(data = NA, nrow = 7, ncol = 4))
colnames(setab) <- c("Variable", "Model Estimate", "Bootstrapped Standard Error", "p-value")

setab[1:7, 1] <- colnames(animals_bs)[2:8]

setab[1, 2] <- round(mmres$summary.animals.model..coeff.fixed[mmres$X == "(Intercept)"], 2)
setab[2, 2] <- round(mmres$summary.animals.model..coeff.fixed[mmres$X == "timecp"], 2)
setab[3, 2] <- round(mmres$summary.animals.model..coeff.fixed[mmres$X == "gender2"], 2)
setab[4, 2] <- round(mmres$summary.animals.model..coeff.fixed[mmres$X == "SES"], 2)
setab[5, 2] <- round(mmres$summary.animals.model..coeff.fixed[mmres$X == "age_59"], 2)
setab[6, 2] <- round(mmres$summary.animals.model..coeff.fixed[mmres$X == "age_59:demind1"], 2)
setab[7, 2] <- round(mmres$summary.animals.model..coeff.fixed[mmres$X == "demind1"], 2)

setab[1, 3] <- round(sd(animals_bs[, 2]), digits = 2)
setab[2, 3] <- round(sd(animals_bs[, 3]), digits = 2)
setab[3, 3] <- round(sd(animals_bs[, 4]), digits = 2)
setab[4, 3] <- round(sd(animals_bs[, 5]), digits = 2)
setab[5, 3] <- round(sd(animals_bs[, 6]), digits = 2)
setab[6, 3] <- round(sd(animals_bs[, 7]), digits = 2)
setab[7, 3] <- round(sd(animals_bs[, 8]), digits = 2)
 
setab[1, 4] <- round(2*pnorm(-abs(mmres$summary.animals.model..coeff.fixed[mmres$X == "(Intercept)"])/
                               sd(animals_bs[, 2])), digits = 4)
setab[2, 4] <- round(2*pnorm(-abs(mmres$summary.animals.model..coeff.fixed[mmres$X == "timecp"])/
                               sd(animals_bs[, 3])), digits = 4)
setab[3, 4] <- round(2*pnorm(-abs(mmres$summary.animals.model..coeff.fixed[mmres$X == "gender2"])/
                               sd(animals_bs[, 4])), digits = 4)
setab[4, 4] <- round(2*pnorm(-abs(mmres$summary.animals.model..coeff.fixed[mmres$X == "SES"])/
                               sd(animals_bs[, 5])), digits = 4)
setab[5, 4] <- round(2*pnorm(-abs(mmres$summary.animals.model..coeff.fixed[mmres$X == "age_59"])/
                               sd(animals_bs[, 6])), digits = 4)
setab[6, 4] <- round(2*pnorm(-abs(mmres$summary.animals.model..coeff.fixed[mmres$X == "age_59:demind1"])/
                               sd(animals_bs[, 7])), digits = 4)
setab[7, 4] <- round(2*pnorm(-abs(mmres$summary.animals.model..coeff.fixed[mmres$X == "demind1"])/
                               sd(animals_bs[, 8])), digits = 4)

setab
setab[c(1, 2, 5), 4] <- "<0.0001"

write.csv(setab, "ResultsTabBootstrappedSEs.csv")


#==============================================================#
# Table of bootstrap results for pre + post slope
#==============================================================#

sltab <- as.data.frame(matrix(data = NA, nrow = 2, ncol = 3))

sltab[1:2, 1] <- c("Slope after change-point", "Slope before change-point")

sltab[1, 2] <- round(mean(animals_bs$Slope1), digits = 2)
sltab[2, 2] <- round(mean(animals_bs$Slope2), digits = 2)

sltab[1, 3] <- paste(paste(round(mean(animals_bs$Slope1) - 1.96 * sd(animals_bs$Slope1), digits = 2), ",", sep = ""),
                     round(mean(animals_bs$Slope1) + 1.96 * sd(animals_bs$Slope1), digits = 2))
sltab[2, 3] <- paste(paste(round(mean(animals_bs$Slope2) - 1.96 * sd(animals_bs$Slope2), digits = 2), ",", sep = ""),
                     round(mean(animals_bs$Slope2) + 1.96 * sd(animals_bs$Slope2), digits = 2))

write.csv(sltab, "BootstrapSlopeTables.csv")
