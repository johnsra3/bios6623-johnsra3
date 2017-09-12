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

summary(gums$attachbase)
boxplot(gums$attachbase)

summary(gums$attach1year)
boxplot(gums$attach1year)

gums$diffattach <- gums$attach1year - gums$attachbase
summary(gums$diffattach)
boxplot(gums$diffattach)

summary(gums$pdbase)
boxplot(gums$pdbase)

summary(gums$pd1year)
boxplot(gums$pd1year)

gums$diffpd <- gums$pd1year - gums$pdbase
summary(gums$diffpd)
boxplot(gums$diffpd)

#=================================================================#
# Label variables from data dictionary
#=================================================================#

labels <- gums

names(labels)

table(labels$trtgroup)
labels$trtgroup <- factor(labels$trtgroup, levels = c("1", "2", "3", "4", "5"),
                          labels = c("Placebo", "Control", "Low Dose",
                                     "Medium Dose", "High Dose"))

table(labels$sex)
labels$sex <- factor(labels$sex, levels = c("1", "2"), labels =
                       c("Male", "Female"))

table(labels$race)
labels$race <- factor(labels$race, levels = c("1", "2", "4", "5"),
                      labels = c("Native American", "African American",
                                 "Asian", "White"))

table(labels$smoker)
labels$smoker <- factor(labels$smoker, levels = c("0", "1"), labels = c("No", "Yes"))


#=================================================================#
# Investigate missing data for outcome
#=================================================================#

missing <- gums[is.na(gums$pd1year) == TRUE, ]
table(missing$sex)
table(missing$race)
summary(missing$age)
table(missing$smoker)
#None of these contain unusual patterns that would explain missingness

#Since they're missing the outcomes, they will be excluded from the analysis
  #therefore, will exclude them from dataset now
labels <- labels[is.na(labels$pd1year) == FALSE, ]
gums <- gums[is.na(gums$pd1year) == FALSE, ]

#=================================================================#
# Divide into group data sets to make table
#=================================================================#

levels(labels$trtgroup)
group1 <- labels[labels$trtgroup == "Placebo", ]
group2 <- labels[labels$trtgroup == "Control", ]  
group3 <- labels[labels$trtgroup == "Low Dose", ]
group4 <- labels[labels$trtgroup == "Medium Dose", ]  
group5 <- labels[labels$trtgroup == "High Dose", ] 
#Slightly more are missing from group 5, but not an incredible amt more 


#=================================================================#
# Divide into group data sets to make table
#=================================================================#

#Want to include sex (3 row), race (5 row), age (1), smoker (3), sites (1), 
  #attach base (1), attach 1yr (1), pdbase (1), pd1 yr (1)
num <- 3 + 5 + 1 + 3 + 1 + 1 + 1 + 1 + 1
tab <- matrix(data = NA, ncol = 6, nrow = 17)
colnames(tab) <- c("", levels(labels$trtgroup))
tab[, 1] <- c("Sex (n (%))", levels(labels$sex), "Race (n (%))", levels(labels$race),
              "Age (mean (sd))", "Smoker (n (%))", levels(labels$smoker), 
              "Sites measured (mean (sd))", "Attachment at baseline (mean (sd))",
              "Attachment at 1 year (mean (sd))", "Pocket depth at baseline (mean (sd))",
              "Pocket depth at 1 year (mean (sd))")

for(i in 1:5){
  tab[2, i+1] <- paste(nrow(gums[gums$trtgroup == i & gums$sex == 1, ]), "(",
                     round(nrow(gums[gums$trtgroup == i & gums$sex == 1, ])/nrow(gums[gums$trtgroup == i, ]), 2), ")")
  tab[3, i+1] <- paste(nrow(gums[gums$trtgroup == i & gums$sex == 2, ]), "(",
                       round(nrow(gums[gums$trtgroup == i & gums$sex == 2, ])/nrow(gums[gums$trtgroup == i, ]), 2), ")")
  tab[5, i+1] <- paste(nrow(gums[gums$trtgroup == i & gums$race == 1, ]), "(",
                       round(nrow(gums[gums$trtgroup == i & gums$race == 1, ])/nrow(gums[gums$trtgroup == i, ]), 2), ")")
  tab[6, i+1] <- paste(nrow(gums[gums$trtgroup == i & gums$race == 2, ]), "(",
                       round(nrow(gums[gums$trtgroup == i & gums$race == 2, ])/nrow(gums[gums$trtgroup == i, ]), 2), ")")
  tab[7, i+1] <- paste(nrow(gums[gums$trtgroup == i & gums$race == 4, ]), "(",
                       round(nrow(gums[gums$trtgroup == i & gums$race == 4, ])/nrow(gums[gums$trtgroup == i, ]), 2), ")")
  tab[8, i+1] <- paste(nrow(gums[gums$trtgroup == i & gums$race == 5, ]), "(",
                       round(nrow(gums[gums$trtgroup == i & gums$race == 5, ])/nrow(gums[gums$trtgroup == i, ]), 2), ")")
  tab[9, i+1] <- paste(round(mean(gums[gums$trtgroup == i, ]$age, na.rm = T), 2), "(", 
                        round(sd(gums[gums$trtgroup == i, ]$age, na.rm = T), 2), ")")
  tab[11, i+1] <- paste(nrow(gums[gums$trtgroup == i & gums$smoker == 0, ]), "(",
                        round(nrow(gums[gums$trtgroup == i & gums$smoker == 0, ])/nrow(gums[gums$trtgroup == i, ]), 2), ")")
  tab[12, i+1] <- paste(nrow(gums[gums$trtgroup == i & gums$smoker == 1, ]), "(",
                        round(nrow(gums[gums$trtgroup == i & gums$smoker == 1, ])/nrow(gums[gums$trtgroup == i, ]), 2), ")")
  tab[13, i+1] <- paste(round(mean(gums[gums$trtgroup == i, ]$sites, na.rm = T), 2), "(", 
                       round(sd(gums[gums$trtgroup == i, ]$sites, na.rm = T), 2), ")")
  tab[14, i+1] <- paste(round(mean(gums[gums$trtgroup == i, ]$attachbase, na.rm = T), 2), "(", 
                        round(sd(gums[gums$trtgroup == i, ]$attachbase, na.rm = T), 2), ")")
  tab[15, i+1] <- paste(round(mean(gums[gums$trtgroup == i, ]$attach1year, na.rm = T), 2), "(", 
                        round(sd(gums[gums$trtgroup == i, ]$attach1year, na.rm = T), 2), ")")
  tab[16, i+1] <- paste(round(mean(gums[gums$trtgroup == i, ]$pdbase, na.rm = T), 2), "(", 
                        round(sd(gums[gums$trtgroup == i, ]$pdbase, na.rm = T), 2), ")")
  tab[17, i+1] <- paste(round(mean(gums[gums$trtgroup == i, ]$pd1year, na.rm = T), 2), "(", 
                        round(sd(gums[gums$trtgroup == i, ]$pd1year, na.rm = T), 2), ")")
}

setwd("C:/Repositories/bios6623-johnsra3/Project0/Processed")
write.csv(tab, "DemographicsTable.csv")

#=================================================================#
# Explore univ relationships b/t covs & attachment outcome
#=================================================================#

#Need to remember that plots based on difference don't account for 
  #starting values, but still helpful to look at
boxplot(gums$diffattach ~ labels$trtgroup, 
        main = "Yearly Difference in Attachment by Treatment Group") 
#overall v little diff b/t groups
boxplot(gums$diffattach ~ labels$sex) #much greater range for females
boxplot(gums$diffattach ~ labels$race) #Afr Amer slightly lower, large white range
plot(gums$diffattach ~ labels$age) #no strong pattern
cor.test(gums$diffattach, labels$age) #not significant, cor is ~ -0.175
boxplot(gums$diffattach ~ labels$smoker) #v little difference
plot(gums$diffattach ~ labels$sites)
cor.test(gums$diffattach, labels$sites) #not significant, cor is ~ .162

#Looking at relationship b/t 2 outcomes
plot(gums$diffattach ~ labels$diffpd)
cor.test(gums$diffattach, gums$diffpd) #outcomes are significantly correlated!

plot(gums$diffattach ~ gums$attachbase)
cor.test(gums$diffattach, gums$attachbase) #significant! change dep on baseline!

#=================================================================#
# Explore univ relationships b/t covs & pocket depth outcome
#=================================================================#

#Need to remember that plots based on difference don't acct for starting vals
boxplot(gums$diffpd ~ labels$trtgroup,
        main = "Yearly Difference in Pocket Depth by Treatment Group")
#not super different by groups
boxplot(gums$diffpd ~ labels$sex) #females look a bit lower
boxplot(gums$diffpd ~ labels$race)
plot(gums$diffpd ~ labels$age)
cor.test(gums$diffpd, labels$age) #super not significant, cor is. ~ -0.074
boxplot(gums$diffpd ~ labels$smoker)
plot(gums$diffpd ~ labels$sites) 
cor.test(gums$diffpd, labels$sites) #super not significant, cor is ~ -0.048

plot(gums$diffpd ~ gums$pdbase)
cor.test(gums$diffpd, gums$pdbase) #significant! change is dep on baseline

#correlation test between two outcomes
cor.test(gums$diffattach, gums$diffpd)
plot(gums$diffattach, gums$diffpd, xlab = "Difference in Attachment",
     ylab = "Difference in Pocket Depth", main = "Difference in Two Outcomes", 
     pch = 19)
text(x = -0.95, y = 0.4, "Correlation = \n 0.5355")
text(x = -0.95, y = 0.2, "p-value\n< 0.001")


#=================================================================#
# Analyze data
#=================================================================#

#Model 1
model1_data <- gums[, c("id", "trtgroup", "attachbase", "attach1year", "diffattach")]

model1_data$trtgroup <- factor(model1_data$trtgroup, levels = c("2", "1", "3", "4", "5"))

model1 <- lm(diffattach ~ trtgroup + attachbase, data = model1_data)
summary(model1)

#Model 2
model2_data <- gums[, c("id", "trtgroup", "pdbase", "pd1year", "diffpd")]

model2_data$trtgroup <- factor(model2_data$trtgroup, levels = c("2", "1", "3", "4", "5"))

model2 <- lm(diffpd ~ trtgroup + pdbase, data = model2_data)
summary(model2)


#=================================================================#
# Create analysis tables
#=================================================================#

#Model 1
model1res <- summary(model1)$coef[, -3]
model1res <- round(model1res, 3)
rownames(model1res) <- c("Intercept (Control)", "Placebo", "Low Dose",
                         "Medium Dose", "High Dose", "Baseline Attachment")
model1res <- as.data.frame(model1res)
model1res$CI <- round(model1res$`Std. Error` * 1.96, 3)
model1res$CIlow <- model1res$Estimate - model1res$CI
model1res$CIhigh <- model1res$Estimate + model1res$CI  
model1res$CIfull <- paste("(", paste(model1res$CIlow, ",", model1res$CIhigh, sep = " "), ")", sep = "")  

model1tab <- model1res[, c(1, 7, 3)]
colnames(model1tab) <- c("Estimate", "95% Confidence Interval", "p-value")
model1tab[6, 3] <- "<0.001" #because was rounding to 0

setwd("C:/Repositories/bios6623-johnsra3/Project0/Processed")
write.csv(model1tab, "Model1_Attachment_AnalysisTable.csv")

#Model 2 
model2res <- summary(model2)$coef[, -3]
model2res <- round(model2res, 3)
rownames(model2res) <- c("Intercept (Control)", "Placebo", "Low Dose",
                         "Medium Dose", "High Dose", "Baseline Attachment")
model2res <- as.data.frame(model2res)
model2res$CI <- round(model2res$`Std. Error` * 1.96, 3)
model2res$CIlow <- model2res$Estimate - model2res$CI
model2res$CIhigh <- model2res$Estimate + model2res$CI
model2res$CIfull <- paste("(", paste(model2res$CIlow, ",", model2res$CIhigh, sep = " "), ")", sep ="")

model2tab <- model2res[, c(1, 7, 3)]
colnames(model2tab) <- c("Estimate", "95% Confidence Interval", "p-value")

setwd("C:/Repositories/bios6623-johnsra3/Project0/Processed")
write.csv(model2tab, "Model2_PocketDepth_AnalysisTable.csv")


#=================================================================#
# Model diagnostics
#=================================================================#

#Model 1
par(mfrow = c(2, 2))
plot(model1, which = 1:4)
#no v strong issues w/ residuals vs. fitted or q-q plot; model fit is acceptable

#Model 2
par(mfrow = c(2, 2))
plot(model2, which = 1:4)
#no v strong issues w/ residuals vs. fitted or q-q plot; model fit is acceptable