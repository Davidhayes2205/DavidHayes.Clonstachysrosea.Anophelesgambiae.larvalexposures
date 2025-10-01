
#install.packages("data.table")
#install.packages("readxl")
#install.packages("ggplot2")

library(data.table)
library(readxl)
library(ggplot2)

setwd('/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/00_Experimental.data/09_C.rosea.data/C.rosea.larvalexposures/00_Raw data/An.gambiae.exposures')

### Input data into R, need readxl package###
L1topupasurvival <- read_excel("C.rosea.An.gambiaeRawdata.xlsx", sheet = "L1topupasurvival")
Timetolarvaldeath <- read_excel("C.rosea.An.gambiaeRawdata.xlsx", sheet = "Timetolarvaldeath")

#### Hayes et al. 2026 C. rosea paper survival figures ####
### Figure 2a Initial exposures ###

#Converts to a dataframe
dat <- data.table(L1topupasurvival)

#Calculates mean survival of the listed treatments and puts them in a datatable
mean_L1topupasurvival <- dat[Treatment == "Water" | Treatment == "10^8" | Treatment == "SUP" | Treatment == "ACSUP" | Treatment == "Washed", 
                             mean(Survival_status), by = Treatment]

#Changes the column names to what is below
colnames(mean_L1topupasurvival) <- c("Treatment","Proportion_survival")

mean_L1topupasurvival

#Preparation of graph
figure2a <- ggplot(mean_L1topupasurvival, aes(x=Treatment, y=Proportion_survival)) + 
  geom_bar(stat = "identity") +
  ylim(0,1)

figure2a

#save as pdf to manuscript onedrive folder
setwd("/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/Hayesetal_Clonostachysmanuscript/Figures/Figures made in R")
ggsave("Hayesetal.2026figure2a.pdf", plot = figure2a, height = 12, width = 20)

### Time to larval death ###
#This data comes from the observations on the date that a larva died. Observations only
# include that had died

#Prepare as a datatable, not sure why it works but it does
dat1 <- data.table(Timetolarvaldeath)

#Estimates means and puts into dataframe
mean_Timetolarvaldeath <- dat1[Treatment == "Water" | Treatment == "10^8" | Treatment == "SUP" | Treatment == "ACSUP" | Treatment == "Washed", 
                               mean(Death_dayspostexposure), by = Treatment]

#Changes column names
colnames(mean_Timetolarvaldeath) <- c("Treatment","Meandayofdeath")

mean_Timetolarvaldeath

#very simple plot of data
plot_timetolarvaldeath <- ggplot(mean_Timetolarvaldeath, aes(x = Treatment, y = Meandayofdeath)) +
  geom_bar(stat = "identity") +
  ylim(0,4)

plot_timetolarvaldeath




