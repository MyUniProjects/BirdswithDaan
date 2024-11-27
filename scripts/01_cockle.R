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
food_ab |>
  dplyr::select(-MusselsHa, -MussOysHa, -Lugworm)

ggplot(food_ab, aes(x = CocklesKg, y = Hellmann_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Cockles Kg", y = "Hellmann's Index") +
  theme_minimal()

ggplot(food_ab, aes(x= CocklesKg, y = NAO_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Cockles Kg", y = "NAO Index") +
  theme_minimal()

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

ggplot(food_ab, aes(x = Hellmann_cat, y = CocklesKg)) +
  geom_boxplot() +
  labs(x = "Hellmann Category", y = "Cockles Kg") +
  theme_minimal()
## This shows that cockle kg is influenced by Hellmann's index, so influenced by harsh winters. Meaning that harsher winter leads to less cockles. 

mod10 <- lm(CocklesKg ~ Hellmann_cat + year, data=food_ab)
summary(mod10)

###############################################################################
# Step 2: Analyse Cockle abudance over the years 
ggplot(food_ab, aes(x = year, y = CocklesKg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Cockles Kg") +
  theme_minimal()

ggplot(food_ab, aes(x = year, y = NAO_winter)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  # Polynomial degree 2
  labs(x = "Year", y = "NAO Index") +
  theme_minimal()

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

ggplot(winteroys, aes(x = year, y = layingdate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Laying Date") +
  theme_minimal()

## plot mean layingdate
ggplot(winteroys, aes(x = year, y = layingdate)) +
  stat_summary(fun = mean, geom = "point") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Laying Date") +
  theme_minimal()

ggplot(winteroys, aes(x = Hellmann_cat, y=layingdate)) +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Hellmann's Index", y = "Laying Date") +
  theme_minimal()

mod10 <- lm(layingdate ~ year, data = winteroys)
summary(mod10)

mod11 <- lmer(layingdate ~ year + (1|couple_ID), data = winteroys)
summary(mod11)

       
   
       
