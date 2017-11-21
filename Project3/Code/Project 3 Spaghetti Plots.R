#=============================================================#
# Project 3
# Spaghetti plots 
# Rachel Johnson
#=============================================================#

#=============================================================#
# Import data
#=============================================================#

library(ggplot2)

setwd("~/School/AdvancedData")
blockr <- read.csv("BlockROutcome.csv", header = T)
animals <- read.csv("AnimalsOutcome.csv", header = T)
logmem1 <- read.csv("LogMem1Outcome.csv", header = T)
logmem2 <- read.csv("LogMem2Outcome.csv", header = T)


#=============================================================#
# Spaghetti plots- exploration of trajectory for all indiv
#=============================================================#

#blockr
# p1 <-ggplot(data = blockr, aes(x = age, y = blockR, group = id, col = as.factor(demind))) +
#   geom_line() +
#   theme_classic() + 
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom",
#         legend.direction = "horizontal") +
#   scale_color_discrete("Developed dementia/MCI") +
#   scale_x_continuous(name = "Age") +
#   scale_y_continuous(name = "Block design score") +
#   ggtitle("BlockR")

#animals
ggplot(data = animals, aes(x = age, y = animals, group = id, col = as.factor(demind))) +
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
# p3 <- ggplot(data = logmem1, aes(x = age, y = logmemI, group = id, col = as.factor(demind))) +
#   geom_line() +
#   theme_classic() + 
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom",
#         legend.direction = "horizontal") +
#   scale_color_discrete("Developed dementia/MCI") +
#   scale_x_continuous(name = "Age") +
#   scale_y_continuous(name = "Logical memory score 1") +
#   ggtitle("LogMemI")

#logmem2
# p4 <- ggplot(data = logmem2, aes(x = age, y = logmemII, group = id, col = as.factor(demind))) +
#   geom_line() +
#   theme_classic() + 
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom",
#         legend.direction = "horizontal") +
#   scale_color_discrete("Developed dementia/MCI") +scale_x_continuous(name = "Age") +
#   scale_y_continuous(name = "Logical memory score 2") +
#   ggtitle("LogMemII")


#=============================================================#
# Spaghetti plots- demind = 1 time before diagnosis!
#=============================================================#

# p5 <- ggplot(data = blockr[blockr$demind == 1,], aes(x = timeb4dem, y = blockR, group = id)) +
#   geom_line() +
#   theme_classic() + 
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom",
#         legend.direction = "horizontal") +
#   geom_vline(xintercept = 0, col = "red") +
#   scale_x_continuous(name = "Time before diagnosis (years)") +
#   scale_y_continuous(name = "Block design score") +
#   ggtitle("BlockR")

ggplot(data = animals[animals$demind == 1,], aes(x = timeb4dem, y = animals, group = id)) +
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
  geom_vline(xintercept = 0, col = "red") +
  scale_x_continuous(name = "Time before diagnosis (years)") +
  scale_y_continuous(name = "Animals score") +
  ggtitle("Animals")

# p7 <- ggplot(data = logmem1[logmem1$demind == 1,], aes(x = timeb4dem, y = logmemI, group = id)) +
#   geom_line() +
#   theme_classic() + 
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom",
#         legend.direction = "horizontal") +
#   geom_vline(xintercept = 0, col = "red") +
#   scale_x_continuous(name = "Time before diagnosis (years)") +
#   scale_y_continuous(name = "LogMemI score") +
#   ggtitle("LogMemI")
# 
# p8 <- ggplot(data = logmem2[logmem2$demind == 1,], aes(x = timeb4dem, y = logmemII, group = id)) +
#   geom_line() +
#   theme_classic() + 
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom",
#         legend.direction = "horizontal") +
#   geom_vline(xintercept = 0, col = "red") +
#   scale_x_continuous(name = "Time before diagnosis (years)") +
#   scale_y_continuous(name = "LogMemII score") +
#   ggtitle("LogMemII")

