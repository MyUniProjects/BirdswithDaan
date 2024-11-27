# restore libraries
rm(list = ls()) # clear environment

# load libraries
library(tidyverse)
library(lme4)

dat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=1857363379&single=true&output=csv") |>
  dplyr::select(year, Hellmann_winter, NAO_winter, voleMarch)

  
#Assignment:
  #Is food abundance affected by either Hellmann’s or NAO ?
    #Vole abundance ~ Hellmann’s / NAO

#Check for outliers
hist(dat$Hellmann_winter)
hist(dat$NAO_winter)
hist(dat$voleMarch)


#Model 1 - Hellmann and NAO
#Visualizing regression
ggplot(data = dat, aes(x = Hellmann_winter, y = NAO_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m1 <- glmer(Hellmann_winter ~ NAO_winter, data= dat, family="gaussian")
summary(m1)

#Model 1 - Hellmann and voles
#Visualizing regression
ggplot(data = dat, aes(x = Hellmann_winter, y = NAO_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m1 <- glmer(Hellmann_winter ~ voleMarch, data= dat, family="gaussian")
summary(m1)













