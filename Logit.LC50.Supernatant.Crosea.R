setwd("/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/00_Experimental.data/09_C.rosea.data/C.rosea.larvalexposures/00_Raw data/An.gambiae.exposures")

#install.packages("tidyverse")
#install.packages("MASS")
#install.packages("readxl")
#install.packages("ggplot2")

library(tidyverse) #need for some dplyr commands I have
library(MASS) # Need to get LC50 from GLM
library(readxl) # Need to read excel file
library(ggplot2)

mydata <- read_excel("LC50.rawdata.xlsx", sheet = "LC50.SUP.refined")

mydata

#logit model assumptions
#1. Binomial outcome (0/1s for survival)
  #Good
#2. Each observation should be independent of others
  #Good
#3. The independent variable (concentration) should not be highly correlated with each other
  #Good
#4. No influential outliers
  #Good
#5. Linearity of independent variables and log odds (logit) of the dependent variable
  #I believe it's pretty linear when you only look from 10^6 - 10^7. 

#Logit model to estimate lethal concentrations

#I'm preparing two models to see which one is best. 
#Both models use survival (cbind(Alive, Dead)) as response variable
#mylogit uses Concentration and Trial as predictor variables (Fixed effect).
#mylogit2 uses only Concentration as predictor variable
#The code "family = binomial(link = "logit")" makes the model a logit analysis

mylogit <- glm(formula = cbind(Alive, Dead) ~ Concentration + Trial, family = binomial(link = "logit"), data = mydata)

mylogit2 <- glm(formula = cbind(Alive, Dead) ~ Concentration, family = binomial(link = "logit"), data = mydata)

#Provides summarys of the models
summary(mylogit) #important to calculate the formula
summary(mylogit2)

#I will go with mylogit2 because the AIC value is higher

LC <- dose.p(mylogit2, p= c(0.5, 0.1)) #Provides LC50, LC90, and SE for both
######The number represents proportion survival###### therefore p=0.1 represents 10% survival not 10% mortality

LC

#Confidence interval calculations
df <- data.frame(LC = c(50, 90))
df <- data.frame(LC = c(50, 90), Dose = unname(LC[1:2]), SE = unname(attributes(LC)$SE[,1]))

#Extracting upper and lower 95% confidence intervals. Using the formula upper/lower limit = Dose +/- 1.96*SE
#this is the formula used to calculate 95% confidence intervals from SE
upperlim50 <- df[1,2] + 1.96*df[1,3] 
lowerlim50 <- df[1,2] - 1.96*df[1,3] 
upperlim90 <- df[2,2] + 1.96*df[2,3] 
lowerlim90 <- df[2,2] - 1.96*df[2,3] 
  
upperlim50
lowerlim50
upperlim90
lowerlim90

#Binding limits together and naming the columns
upperlim <- rbind(upperlim50, upperlim90)
colnames(upperlim) <- "95%upperlim"
upperlim
lowerlim <- rbind(lowerlim50, lowerlim90)
colnames(lowerlim) <- "95%lowerlim"
lowerlim

#Binding the limits for LC50 and LC90 with the LC50/90 dose and SE
df <- cbind(df, upperlim, lowerlim)
rownames(df) <- NULL
df
  
setwd("/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/00_Experimental.data/09_C.rosea.data/C.rosea.larvalexposures/00_LC50calculation/Supernatant")
write.csv(df,'SupernatantLC50Concentrations.csv')

#Plot dose response graph. Add error bars of Standard deviation
setwd("/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/00_Experimental.data/09_C.rosea.data/C.rosea.larvalexposures/00_Raw data/An.gambiae.exposures")

mydata2 <- read_excel("LC50.rawdata.xlsx", sheet = "LC50.SUP.refined.sum")

mydata2

plot <- ggplot(data=mydata2, aes(x=Concentration, y=Percent_survival, group=1)) +
  geom_line()+
  geom_point() +
  scale_x_log10() +
  geom_errorbar(aes(x = Concentration, ymin = Percent_survival-SurvivalSTDEV, ymax = Percent_survival+SurvivalSTDEV), width = 0.2) +
  theme_classic()

plot

setwd("/Users/davidhayes/Library/CloudStorage/OneDrive-KansasStateUniversity/01_DavidHayes/Hayesetal_Clonostachysmanuscript/Figures/Figures made in R")

ggsave("Hayesetal.2026figure2b.pdf", plot = plot, height = 12, width = 20)

#########################################################################################################################
#Test for linearity of my predictor variable (Concentration) and the log odds (logit) of my response variance (Alive/Dead)

# Pulls Concentration values into a dataframe
mydata2 <- mydata %>%
  dplyr::select(Concentration) 
predictors <- colnames(mydata2) #not sure why / if I need this

mydata2
predictors

prob <- predict(mylogit, type = "response") #Calculating probability values that prepare the log odds (logit) values
prob

# Making the log odds (logit) and binding to predictor variable tidying the data for plot
mydata2 <- mydata2 %>%
  mutate(logit = log(prob/(1-prob))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata2

#Preparing plot to look for linearity of the data.
#Plot is linear, therefore, I don't need to log10 transform my predictor variable (Concentration)
Confirm_linearity <- ggplot(mydata2, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  scale_y_log10() +
  ylim(1000000,10000000) +
  xlim(-2,8) +
  facet_wrap(~predictors, scales = "free_y")

Confirm_linearity

