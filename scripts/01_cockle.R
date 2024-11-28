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

p8 <- 

###############################################################################
# Step 4. How is laying date correlated with winter proxies 
mod14 <- lmer(layingdate ~ year + Hellmann_winter + (1|couple_ID) + (1|maleID) + (1|femID) + (1|year), data = winteroys)
summary(mod14)
## So Hellman winter does not seem to have a big effect as a fixed effect

mod15 <- lmer(layingdate ~ year + Hellmann_cat + (1|couple_ID) + (1|maleID) + (1|femID) + (1|year), data = winteroys)
summary(mod15)
## When looking at the Hellman categories there is a bigger effect than just the nummerical hellman but still not lots of effect. Additionally the residual variance is only down by 1.0 compared to the models without Hellman

mod16<- lmer(layingdate ~ year + NAO_winter + (1|couple_ID) + (1|maleID) + (1|femID) + (1|year), data = winteroys)
summary(mod16)
## NAO winter does not seem to have a big effect as a fixed effect

mod17 <- lmer(layingdate ~ year + NAO_cat + (1|couple_ID) + (1|maleID) + (1|femID) + (1|year), data = winteroys)
summary(mod17)
## When looking at the NAO categories there is a bigger effect than just the nummerical NAO but still not lots of effect. Additionally the residual variance is only down by 1.0 compared to the models without NAO

mod18 <- lmer(layingdate ~ year + Hellmann_cat + NAO_cat + (1|couple_ID) + (1|maleID) + (1|femID) + (1|year), data = winteroys)
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


oydat <- oydat |>
  dplyr::group_by(femID) |>
  dplyr::mutate(max_year = max(min_year)) |>
  dplyr::mutate(year = ifelse(min_age >= 10, "old", "young")) |>
  dplyr::ungroup() |>
  dplyr::filter(!(year == "young" & max_age >= 10))
