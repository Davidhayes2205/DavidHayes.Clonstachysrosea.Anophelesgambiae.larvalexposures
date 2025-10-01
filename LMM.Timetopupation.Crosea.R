#install.packages("lme4")
#install.packages("lsmeans")
#install.packages("readxl")
#install.packages("ggplot2")

library(lsmeans)
library(emmeans)
library(readxl)
library(ggplot2)

### Washed conidia ###

setwd('/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/00_Experimental.data/09_C.rosea.data/C.rosea.larvalexposures/00_Development time')

#Upload data
Timetopupation_Conidia <- read_excel("Timetopupation_Angambiae.xlsx", sheet = "Conidia")

#Transform Dose into a factor variable instead of a numeric variable
Timetopupation_Conidia$Dose <- as.factor(Timetopupation_Conidia$Dose)
is.factor(Timetopupation_Conidia$Dose)

#Linear model with Time to pupation as response variable
#Dose and Trial as predictor variables (fixed effects)
lm_Conidia <- lm(Timetopupa ~ Dose + Trial,
                 data = Timetopupation_Conidia)
summary(lm_Conidia)

#Calculates LSM and SEM values
lsmConidia <- lsmeans(lm_Conidia, ~ Dose)  
lsmConidia

lsmConidia <- as.data.frame(lsmConidia)
is.data.frame(lsmConidia)

#Plots data
fig_conidia <- ggplot(lsmConidia, aes(x=Dose, y=lsmean)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = Dose, ymin = lsmean-SE, ymax = lsmean+SE), width = 0.4, colour = "black") +
  scale_y_continuous(breaks = seq(0, 12, by=2), limits = c(0,10))

fig_conidia

setwd("/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/Hayesetal_Clonostachysmanuscript/Figures/Figures made in R")
ggsave("Hayesetal.2026figure3b.pdf", plot = fig_conidia, height = 12, width = 20)

### Culture supernatant ###

setwd('/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/00_Experimental.data/09_C.rosea.data/C.rosea.larvalexposures/00_Development time')


#Upload data
Timetopupation_SUP <- read_excel("Timetopupation_Angambiae.xlsx", sheet = "SUP")

#Transform Dose into a factor instead of a numeric
Timetopupation_SUP$Dose <- as.factor(Timetopupation_SUP$Dose)
is.factor(Timetopupation_SUP$Dose)

#Linear model with Time to pupation as response variable
#Dose and Trial as predictor variables (fixed effects)
lm_SUP <- lm(Timetopupa ~ Dose + Trial,
              data = Timetopupation_SUP)

summary(lm_SUP)

#Calculates LSM and SEM values
lsm_SUP <- lsmeans(lm_SUP, ~ Dose)  
lsm_SUP

lsm_SUP <- as.data.frame(lsm_SUP)
is.data.frame(lsm_SUP)

#Plots data
fig_SUP <- ggplot(lsm_SUP, aes(x=Dose, y=lsmean)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = Dose, ymin = lsmean-SE, ymax = lsmean+SE), width = 0.4, colour = "black") +
  scale_y_continuous(breaks = seq(0, 12, by=2), limits = c(0,10))

fig_SUP

setwd("/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/Hayesetal_Clonostachysmanuscript/Figures/Figures made in R")
ggsave("Hayesetal.2026figure3a.pdf", plot = fig_SUP, height = 12, width = 20)

