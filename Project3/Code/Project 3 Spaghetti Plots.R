#=============================================================#
# Project 3
# Spaghetti plots 
# Rachel Johnson
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(ggplot2)
library(gridExtra)

setwd("~/School/AdvancedData")
blockr <- read.csv("BlockROutcome.csv", header = T)
animals <- read.csv("AnimalsOutcome.csv", header = T)
logmem1 <- read.csv("LogMem1Outcome.csv", header = T)
logmem2 <- read.csv("LogMem2Outcome.csv", header = T)


#=============================================================#
# Spaghetti plots- exploration of trajectory for all indiv
#=============================================================#

#blockr
p1 <-ggplot(data = blockr, aes(x = age, y = blockR, group = id, col = as.factor(demind))) +
  geom_line() +
  theme_classic() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_discrete("Developed dementia/MCI") +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Block design score") +
  ggtitle("BlockR")



#animals
p2 <- ggplot(data = animals, aes(x = age, y = animals, group = id, col = as.factor(demind))) +
  geom_line() +
  theme_classic() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_discrete("Developed dementia/MCI") +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Animal category fluency score") +
  ggtitle("Animals")

#logmem1
p3 <- ggplot(data = logmem1, aes(x = age, y = logmemI, group = id, col = as.factor(demind))) +
  geom_line() +
  theme_classic() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_discrete("Developed dementia/MCI") +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Logical memory score 1") +
  ggtitle("LogMemI")

#logmem2
p4 <- ggplot(data = logmem2, aes(x = age, y = logmemII, group = id, col = as.factor(demind))) +
  geom_line() +
  theme_classic() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_color_discrete("Developed dementia/MCI") +scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Logical memory score 2") +
  ggtitle("LogMemII")

grid.arrange(p1, p2, p3, p4, ncol = 2)


#=============================================================#
# Spaghetti plots- demind = 1 time before diagnosis!
#=============================================================#
