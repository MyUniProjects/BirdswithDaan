# restore libraries
rm(list = ls()) # clear environment

# load libraries
library(tidyverse)
library(lme4)

dat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=1857363379&single=true&output=csv") |>
  dplyr::select(year, Hellmann_winter, NAO_winter, voleMarch) |>
  na.omit() |>
  mutate(Hellmann_cat = case_when(
    Hellmann_winter < 50 ~ "Low",
    Hellmann_winter >= 50 & Hellmann_winter < 100 ~ "Medium",
    Hellmann_winter >= 100 ~ "High")) |>
  mutate(NAO_cat = case_when(
    NAO_winter < 0 ~ "Negative",
    NAO_winter >= 0 ~ "Positive"))

bird <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=1702903723&single=true&output=csv") |>
  dplyr::select(ID_Buzzard, sex, year, min_age, laying_date) |>
  na.omit()
  


#Assignment:
  #Is food abundance affected by either Hellmann’s or NAO ?
    #Vole abundance ~ Hellmann’s / NAO

#Check for outliers
hist(dat$Hellmann_winter)
hist(dat$NAO_winter)
hist(dat$voleMarch)


##################################################################
#VOLES
#Model 1 - Hellmann and NAO
#Visualizing regression
ggplot(data = dat, aes(x = Hellmann_winter, y = NAO_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m1 <- lm(Hellmann_winter ~ NAO_winter, data= dat)
summary(m1)


#Model 2 - Hellmann and voles
#Visualizing regression
ggplot(data = dat, aes(x = Hellmann_winter, y = voleMarch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Using a categorized Hellmann index
m2 <- lmer(voleMarch ~ year + (1|Hellmann_cat), data = dat)
summary(m2)
#Repeatability: 25.77 / (25.77 + 184.70) = 0.1224403


#Model 3 - NAO and voles
#Visualizing regression
ggplot(data = dat, aes(x = NAO_winter, y = voleMarch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m3 <- lmer(voleMarch ~ year + (1|NAO_cat), data = dat)
#Indirect effect on land climate, so weaker effect than in sea
summary(m3)
#Repeatability: 39.48 / (39.48 + 184.73) = 0.1304041


#Model 4 - Hellmann / NAO and voles
m4 <- lmer(voleMarch ~ year + (1|Hellmann_cat) + (1|NAO_cat), data = dat)
#Indirect effect on land climate, so weaker effect than in sea
summary(m4)
#Repeatability Hellmann: 19.89 / (19.89 + 30.68 + 173.79) = 0.08865217
#Repeatability NAO:      30.68 / (19.89 + 30.68 + 173.79) = 0.1367445


#Model 5 - Voles over the years
#Visualizing relation
ggplot(data = dat, aes(x = year, y = voleMarch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m5 <- lm(voleMarch ~ year, data = dat)
summary(m5)


##################################################################
#LAYING DATE
buzzard <- bird |>
  dplyr::group_by(ID_Buzzard) |>
  dplyr::mutate(max_age = max(min_age)) |>
  dplyr::mutate(age = ifelse(min_age >= 10, "old", "young")) |>
  dplyr::ungroup() |>
  dplyr::filter(!(age == "young" & max_age >= 10))

#Model 6 - Laying date in relation to Hellmann
#Visualizing regression
ggplot(data = dat, aes(x = Hellmann_winter, y = voleMarch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#Model 7 - Laying date in relation to NAO









