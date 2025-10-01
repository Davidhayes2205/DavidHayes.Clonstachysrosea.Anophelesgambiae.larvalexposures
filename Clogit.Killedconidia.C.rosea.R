
#install.packages("readxl")
#install.packages("data.table")
#install.packages("survival")
#install.packages("emmeans")
#install.packages("ggplot2")

library(readxl)
library(data.table)
library(survival) # Need for clogit function
library(emmeans) # or library(lsmeans)
library(ggplot2)

setwd('/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/00_Experimental.data/09_C.rosea.data/C.rosea.larvalexposures/00_Raw data/An.gambiae.exposures')

### Input data into R, need readxl package###
L1topupasurvival_killed <- read_excel("C.rosea.An.gambiaeRawdata.xlsx", sheet = "L1topupasurvival_killedconidia")

#Converts to a dataframe
dat_killed <- data.table(L1topupasurvival_killed)

#Calculates mean survival of the listed treatments and puts them in a datatable
mean_L1topupasurvival_killed <- dat_killed[Treatment == "AC" | Treatment == "Washed" | Treatment == "Heat" | Treatment == "UV" | Treatment == "Water", 
                                           mean(Survival_status), by = Treatment]

#Changes the column names to what is below
colnames(mean_L1topupasurvival_killed) <- c("Treatment","Proportion_survival")

mean_L1topupasurvival_killed

#Conditional logistic regression model
#Survival is response variable. Treatment is predictor variable (fixed effect). 
#Trial is stratified to account to high variance

clogit_killed <- clogit(Survival_status ~ Treatment + strata(Trial_ID), dat_killed)
summ.clogit_killed <- summary(clogit_killed)
summ.clogit_killed

#Multiple comparisions
multiplecomp_killed <- emmeans(clogit_killed, ~ Treatment)

# For pairwise comparisons with Bonferroni's adjustment
multiplecomp_killed_adjust <- pairs(multiplecomp_killed, adjust = "bonferroni") 

multiplecomp_killed_adjust

#Saves regression summary as a txt file

sink("clogit.txt")
print(summary(clogit(Individual_survival ~ Protocol + strata(Trial), mydata2)))
sink() 

figure <- ggplot(mean_L1topupasurvival_killed, aes(x=Treatment, y=Proportion_survival)) + 
  geom_bar(stat = "identity") +
  ylim(0,1)

figure
setwd("/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/Hayesetal_Clonostachysmanuscript/Figures/Figures made in R")
ggsave("killedconidia.pdf", plot = figure, height = 12, width = 20)
