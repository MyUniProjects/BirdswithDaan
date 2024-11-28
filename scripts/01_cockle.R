#### Cockle Data Analyses
renv::activate()
renv::snapshot()
renv::restore()

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("lme4")
library(lme4)

### Load the data
envdat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=1857363379&single=true&output=csv") |>
  as.tibble()
oydat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=253239144&single=true&output=csv")
cockdat <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=553798957&single=true&output=csv")

#### Step 1 Analyse Food Abundance ~ Hellmann's or NAO 
plot(cockdat$CocklesKg)
hist(cockdat$CocklesKg)

Hellmanns <- envdat |>
  dplyr::select("Hellmann_winter", "ID_Env", "year", "NAO_winter") |>
  dplyr::filter(year>1989)

### Categorize Hellmann's 
Hellmanns <- Hellmanns %>%
  mutate(Hellmann_cat = case_when(
    Hellmann_winter >= 0 & Hellmann_winter < 50 ~ "Low",
    Hellmann_winter >= 50 & Hellmann_winter < 100 ~ "Medium",
    Hellmann_winter >= 100  ~ "High"
  ))  |>
  mutate(NAO_cat = case_when(
    NAO_winter < 0.0 ~ "Negative",
    NAO_winter >= 0.0 ~ "Positive")
  )

cockdat |>
  dplyr::select("CocklesKg", "year")

food_ab <- left_join(cockdat, Hellmanns, by = "year")
food_ab <- food_ab |>
  dplyr::select(-MusselsHa, -MussOysHa, -Lugworm) |>
  na.omit()

p1 <- ggplot(food_ab, aes(x = CocklesKg, y = Hellmann_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Cockles Kg", y = "Hellmann's Index") +
  theme_minimal()

ggsave("figures/Cockle_Hellman_winter.png", p1, width = 6, height = 4, units = "in", dpi = 300)

p2 <- ggplot(food_ab, aes(x= CocklesKg, y = NAO_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Cockles Kg", y = "NAO Index") +
  theme_minimal()

ggsave("figures/Cockle_NAO_winter.png", p2, width = 6, height = 4, units = "in", dpi = 300)

### Make a model 
mod1 <- lm(CocklesKg ~ Hellmann_winter, data = food_ab)
summary(mod1)
# not significant but there is small effect

mod2 <- lm(CocklesKg ~ Hellmann_winter + year, data = food_ab)
summary(mod2)
# not significant but there is small effect 

anova(mod1, mod2)
################################################
mod3 <- lm(CocklesKg ~ NAO_winter, data = food_ab)
summary(mod3)

mod4 <- lm(CocklesKg ~ NAO_winter + year, data = food_ab)
summary(mod4)

mod5 <- lm(CocklesKg ~ Hellmann_winter + NAO_winter, data = food_ab)
summary(mod5)

mod6 <- lm(CocklesKg ~ Hellmann_winter + NAO_winter + year, data = food_ab)
summary(mod6)

anova(mod6, mod5)

### Mixed Model
mod7 <- lmer(CocklesKg ~ Hellmann_winter + NAO_winter + (1|Hellmann_cat), data = food_ab)
summary(mod7)

mod8 <- lmer(CocklesKg ~ Hellmann_winter + NAO_winter + (1|Hellmann_cat) + (1|NAO_cat), data = food_ab)
summary(mod8)
# Looks like NAO has effect on cockle kg 

mod9 <- lmer(CocklesKg ~ NAO_winter + year + (1|NAO_cat), data = food_ab)
summary(mod9)

ggplot(food_ab, aes(x = CocklesKg, y = NAO_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Cockles Kg", y = "NAO") +
  theme_minimal()

ggplot(food_ab, aes(x = NAO_cat, y = CocklesKg)) +
  geom_boxplot() +
  labs(x = "NAO Category", y = "Cockles Kg") +
  theme_minimal()
## So this shows that cockle kg is not really influenced by NAO, so not really influenced by harsh or mind winters(?)

p3 <- ggplot(food_ab, aes(x = Hellmann_cat, y = CocklesKg)) +
  geom_boxplot() +
  labs(x = "Hellmann Category", y = "Cockles Kg") +
  theme_minimal()
## This shows that cockle kg is influenced by Hellmann's index, so influenced by harsh winters. Meaning that harsher winter leads to less cockles. 

ggsave("figures/Cockle_Hell_cat.png", p3, width = 6, height = 4, units = "in", dpi = 300)

mod10 <- lm(CocklesKg ~ Hellmann_cat + year, data=food_ab)
summary(mod10)

mod21 <- lm(CocklesKg ~ Hellmann_cat, data = food_ab)
summary(mod21)

# plot the mod21 
ggplot(food_ab, aes(x = Hellmann_cat, y = CocklesKg)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Hellmann Category", y = "Cockles Kg") +
  theme_minimal()

# use tukey test to see which groups are different
mod21 <- aov(CocklesKg ~ Hellmann_cat, data = food_ab) # Refit with aov
tukey_result <- TukeyHSD(mod21)
print(tukey_result)

install.packages("multcompView")
library(multcompView)
tukey_cld <- multcompView::multcompLetters(TukeyHSD(mod21)$Hellmann_cat[,"p adj"])
tukey_cld

mod22 <- aov(CocklesKg ~ NAO_cat, data = food_ab) 
tukey_result <- TukeyHSD(mod22)
print(tukey_result)

p_values <- TukeyHSD(mod22)$NAO_cat[,"p adj"]
names(p_values) <- rownames(TukeyHSD(mod22)$NAO_cat)

tukey_cld <- multcompView::multcompLetters(p_values)
tukey_cld

###############################################################################
# Step 2: Analyse Cockle abudance over the years 
p4 <- ggplot(food_ab, aes(x = year, y = CocklesKg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Cockles Kg") +
  theme_minimal()

ggsave("figures/Cockle_year.png", p4, width = 6, height = 4, units = "in", dpi = 300)

p5 <- ggplot(food_ab, aes(x = year, y = NAO_winter)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  # Polynomial degree 2
  labs(x = "Year", y = "NAO Index") +
  theme_minimal()

ggsave("figures/NAO_year.png", p5, width = 6, height = 4, units = "in", dpi = 300)

p6 <- ggplot(food_ab, aes(x = year, y = Hellmann_winter)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  
  labs(x = "Year", y = "Hellmann's Index") +
  theme_minimal()

ggsave("figures/Hellman_year.png", p6, width = 6, height = 4, units = "in", dpi = 300)

mod11 <- lm(CocklesKg ~ year, data = food_ab)
summary(mod11)  

###############################################################################
# Step 3. Laying date and  winter proxy

### assign a new couple_ID to oydat 
oydat$couple_ID <- as.integer(factor(paste(oydat$maleID, oydat$femID, sep = "_")))

winterprox <- envdat |>
  dplyr::select("Hellmann_winter", "year", "NAO_winter") 
winterprox <- winterprox %>%
  mutate(Hellmann_cat = case_when(
    Hellmann_winter >= 0 & Hellmann_winter < 50 ~ "Low",
    Hellmann_winter >= 50 & Hellmann_winter < 100 ~ "Medium",
    Hellmann_winter >= 100  ~ "High"
  ))  |>
  mutate(NAO_cat = case_when(
    NAO_winter < 0.0 ~ "Negative",
    NAO_winter >= 0.0 ~ "Positive")
  )

winteroys <- left_join(oydat, winterprox, by = "year")
winteroys <- winteroys|> na.omit()

ggplot(winteroys, aes(x = year, y = layingdate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Laying Date") +
  theme_minimal()

## plot mean layingdate
p7 <- ggplot(winteroys, aes(x = year, y = layingdate)) +
  stat_summary(fun = mean, geom = "point") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Laying Date") +
  theme_minimal()

ggsave("figures/Layingdate_year.png", p7, width = 6, height = 4, units = "in", dpi = 300)

ggplot(winteroys, aes(x = Hellmann_cat, y=layingdate)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Hellmann's Index", y = "Laying Date") +
  theme_minimal()

p8 <-ggplot(winteroys, aes(x = NAO_cat, y=layingdate)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "NAO Index", y = "Laying Date") +
  theme_minimal() +
  geom_text(aes(x = 1, y = 100, label = "b"), 
            size = 3, 
            color = "black") +
  geom_text(aes(x=2, y=100, label="a"), 
            size = 3,
            color = "black")
ggsave("figures/Layingdate_NAO_cat.png", p8, width = 6, height = 4, units = "in", dpi = 300)

mod23 <- lm(layingdate ~ Hellmann_cat, data = winteroys)
mod23 <- aov(CocklesKg ~ Hellmann_cat, data = food_ab) # Refit with aov
tukey_result <- TukeyHSD(mod23)
print(tukey_result)


tukey_cld <- multcompView::multcompLetters(TukeyHSD(mod23)$Hellmann_cat[,"p adj"])
tukey_cld

mod24 <- aov(layingdate ~ NAO_cat, data = winteroys) 
tukey_result <- TukeyHSD(mod24)
print(tukey_result)

p2_values <- TukeyHSD(mod24)$NAO_cat[,"p adj"]
names(p2_values) <- rownames(TukeyHSD(mod24)$NAO_cat)

tukey_cld <- multcompView::multcompLetters(p2_values)
tukey_cld


mod10 <- lm(layingdate ~ year, data = winteroys)
summary(mod10)

mod11 <- lmer(layingdate ~ year + (1|couple_ID), data = winteroys)
summary(mod11)
# couple_ID has a big effect on laying date
# repeatability is 64.24 / (64.24 + 75.07) = 0.461
# variance of couple_ID is 64.24
# variance of residuals is 75.07

mod12 <- lmer(layingdate ~ year + (1|couple_ID) + (1|maleID) + (1|femID), data = winteroys)
summary(mod12)
# now couple_ID has a much lower variance than before namely 9.395
# probably because fem and male ID explain a big part of the variance of couple_ID
# variance of residuals is a bit lower namely 73.936

mod13 <- lmer(layingdate ~ year + (1|couple_ID) + (1|maleID) + (1|femID) + (1|year), data=winteroys)
summary(mod13)   
# the variance of the residual is now even lower, namely 67.627
# now the difference between years has been taken into account. Which is important because there is variance between the couples per year, and also there is difference in femID between couples because males mate with different females depending on whether there partner died. 


###############################################################################
# Step 4. How is laying date correlated with winter proxies 

ggplot(winteroys, aes(x = Hellmann_winter, y = layingdate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Hellmann's Index", y = "Laying Date") +
  theme_minimal()

ggplot(winteroys, aes(x = NAO_winter, y = layingdate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "NAO Index", y = "Laying Date") +
  theme_minimal()

mod14 <- lmer(layingdate ~ year + Hellmann_winter + (1|couple_ID) + (1|maleID) + (1|femID), data = winteroys)
summary(mod14)
## So Hellman winter does not seem to have a big effect as a fixed effect

mod25 <- lm(layingdate ~ NAO_cat, data = winteroys)
summary(mod25)

mod15 <- lmer(layingdate ~ Hellmann_cat+ (1|femID), data = winteroys)
summary(mod15)
## When looking at the Hellman categories there is a bigger effect than just the nummerical hellman but still not lots of effect. Additionally the residual variance is only down by 1.0 compared to the models without Hellman

mod16<- lmer(layingdate ~ year + NAO_winter + (1|couple_ID) + (1|maleID) + (1|femID), data = winteroys)
summary(mod16)
## NAO winter does not seem to have a big effect as a fixed effect

mod17 <- lmer(layingdate ~ NAO_cat + (1|femID) , data = winteroys)
summary(mod17)
## When looking at the NAO categories there is a bigger effect than just the nummerical NAO but still not lots of effect. Additionally the residual variance is only down by 1.0 compared to the models without NAO

mod18 <- lmer(layingdate ~ year + Hellmann_cat + NAO_cat + (1|couple_ID) + (1|maleID) + (1|femID), data = winteroys)
summary(mod18)
## Both Hellman and NAO categories togethere seems to have not a big influence on explaining the variance. 

#################################################################################
# Step 5. How is laying date correlated with cockle abundance
cockoydat <- left_join(oydat, cockdat, by = "year") 
cockoydat |> na.omit()
mod19 <- lm(layingdate ~ CocklesKg, data = cockoydat)
summary(mod19)
## laying date is not correlated with cockle abundance, but maybe in can explain variance ? 

mod20 <- lmer(layingdate ~ year + (1|CocklesKg) + (1|couple_ID) + (1|maleID) + (1|femID) + (1|year), data = cockoydat)
summary(mod20)

ggplot(cockoydat, aes(x = CocklesKg, y = layingdate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Cockles Kg", y = "Laying Date") +
  theme_minimal()

ggplot(cockoydat, aes(x = CocklesKg, y = layingdate)) +
  stat_summary(fun = mean, geom = "point") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Cockles Kg", y = "Laying Date") +
  theme_minimal()
## Only 2 day difference in laying date when comparing the means of the years 

##########################################################################
## Step. 6 Is there difference in laying date depending on age? 
mod20 <- lmer(layingdate ~ year + (year|femID) + (1|maleID) + (1|couple_ID), data = winteroys)
summary(mod20)

# Step 1: Get the first breeding year for each femID
oydat$first_breeding_year <- ave(oydat$year, oydat$femID, FUN = min)

# Step 2: Calculate age (age at the time of breeding for each year)
oydat$age <- oydat$year - oydat$first_breeding_year + 1
max(oydat$age)
hist(oydat$age)

femdat <- oydat |>
  dplyr::group_by(femID) |>
  dplyr::mutate(max_age = max(age)) |>
  dplyr::mutate(age_cat = ifelse(age >= 10, "old", "young")) |>
  dplyr::ungroup() |>
  dplyr::filter(!(age_cat == "young" & max_age >= 10))

ggplot(femdat, aes(x = age_cat, y = layingdate)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age", y = "Laying Date") +
  theme_minimal()

ggplot(femdat, aes(x = max_age, y = layingdate)) +
  stat_summary(fun = mean, geom = "point", color = "black") +  # Summarize layingdate (mean) for each age
  geom_smooth(method = "lm", se = FALSE, color = "red") +     # Add a linear regression line
  labs(x = "Age", y = "Mean Laying Date") +                   # Update axis labels
  theme_minimal() 

mod26 <- lm(layingdate ~ age_cat, data = femdat)
summary(mod26)

mod27 <- lmer(layingdate ~ age_cat + (1|femID), data = femdat)
summary(mod27)

#Load package qdapTools to be able to use the lookup function
install.packages("qdapTools")
library(qdapTools)

# Center age per femID (individual)
ind_age <- aggregate(cbind(max_age) ~ femID, femdat, mean)
femdat$avg_age <- lookup(femdat$femID, ind_age[, c("femID", "max_age")])

# Center age for each individual (within-individual effect)
femdat$age_cen <- femdat$max_age - femdat$max_age  # Center layingdate by subtracting the individual's mean layingdate

# Fit mixed model with age_cen (within-individual effect) and avg_age (between-individual effect)
m1 <- glmer(layingdate ~ age_cen + avg_age + (1 | femID), data = femdat, family = "gaussian")  # Gaussian family for continuous response

# View model summary and confidence intervals
summary(m1)
confint(m1)

###############################################################################
### Analyse plasticity of Hellmann's Index 
#Center Hellmann's Index  per individual
ind_avgF<-aggregate(cbind(Hellmann_winter)~femID,winteroys,mean) 
# Calc avg density per fem
## Between individual effect: mean density for each female! This is how individuals differ
winteroys$Btw_Ind_Hell<-lookup(winteroys$femID,ind_avgF[,c("femID","Hellmann_winter")])
## Within individual effect: how each value differs from individual mean.
winteroys$Wthin_Ind_Hell<-winteroys$Hellmann_winter-winteroys$Btw_Ind_Hellmann 
#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect
m2<-glmer(layingdate~Wthin_Ind_Hell + Btw_Ind_Hell+ (1|femID), data= winteroys, family="gaussian")
summary(m2)
confint(m2)

## Analayse plasticity of NAO Index
ind_avgN <- aggregate(cbind(NAO_winter) ~ femID, winteroys, mean)
winteroys$Btw_Ind_NAO <- lookup(winteroys$femID, ind_avgN[, c("femID", "NAO_winter")])
winteroys$Wthin_Ind_NAO <- winteroys$NAO_winter - winteroys$Btw_Ind_NAO
m3 <- glmer(layingdate ~ Wthin_Ind_NAO + Btw_Ind_NAO + (1|femID), data = winteroys, family = "gaussian")
summary(m3)
confint(m3)
## There is plasticity in laying date in the whole population. Because the there is within individual effect but this is not significantly different from the between individual effect. 

## Analayse plasticity of Cockles Kg
ind_avgC <- aggregate(cbind(CocklesKg) ~ femID, cockoydat, mean)
cockoydat$Btw_Ind_Coc <- lookup(cockoydat$femID, ind_avgC[, c("femID", "CocklesKg")])
cockoydat$Wthin_Ind_Coc <- cockoydat$CocklesKg - cockoydat$Btw_Ind_Coc
m4 <- glmer(layingdate ~ Wthin_Ind_Coc + Btw_Ind_Coc + (1|femID), data = cockoydat, family = "gaussian")
summary(m4)
confint(m4)





