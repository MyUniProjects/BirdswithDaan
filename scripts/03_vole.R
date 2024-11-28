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
#VOLES - winter harshness
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
#LAYING DATE - winter harshness and bird age

#Is the laying date affected by either Hellmann’s or NAO?
  #Buzzard laying date ~ Hellmann’s / NAO

buzzard <- bird |>
  dplyr::group_by(ID_Buzzard) |>
  dplyr::mutate(max_age = max(min_age)) |>
  dplyr::mutate(age = ifelse(min_age >= 8, "old", "young")) |>
  dplyr::ungroup() |>
  dplyr::filter(!(age == "young" & max_age >= 8))

buzzard_dat <- dplyr::left_join(buzzard, dat, by = "year")
  

#Model 6 - Laying date in relation to Hellmann
#Visualizing regression
ggplot(data = buzzard_dat, aes(x = Hellmann_winter, y = laying_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# use tukey test to see which groups are different
#install.packages("multcompView")
library(multcompView)

m6_1 <- aov(laying_date ~ Hellmann_cat, data = buzzard_dat)
tukey_result1<-TukeyHSD(m6_1)
tukey_result1

tukey_cld1 <- multcompView::multcompLetters(TukeyHSD(m6_1)$Hellmann_cat[,"p adj"])
tukey_cld1

letters_df1 <- data.frame(
  group = c("Low", "Medium", "High"),
  cld = c("a", "a", "b"),
  ypos = c(200, 200, 200))

ggplot(data = buzzard_dat, aes(x = Hellmann_cat, y = laying_date)) +
  geom_boxplot() +
  geom_text(
    data = letters_df1,
    aes(x = group, y = ypos, label = cld),
    inherit.aes = FALSE,
    vjust = -0.5) +
  labs(x = "Hellmann index", y = "Laying date") +
  theme_minimal()

m6 <- lmer(laying_date ~ Hellmann_winter + (1|year), data = buzzard_dat)
summary(m6)
#Repeatability for year: 16.11 / (16.11 + 80.32) = 0.1670642


#Model 7 - Laying date in relation to NAO
ggplot(data = buzzard_dat, aes(x = NAO_winter, y = laying_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# use tukey test to see which groups are different
m7_1 <- aov(laying_date ~ NAO_cat, data = buzzard_dat)
tukey_result2<-TukeyHSD(m7_1)
tukey_result2

p_values <- TukeyHSD(m7_1)$NAO_cat[,"p adj"]
names(p_values) <- rownames(TukeyHSD(m7_1)$NAO_cat)
tukey_cld2 <- multcompView::multcompLetters(p_values)
tukey_cld2

letters_df2 <- data.frame(
  group = c("Positive", "Negative"),
  cld = c("a", "b"),
  ypos = c(200, 200))

ggplot(data = buzzard_dat, aes(x = NAO_cat, y = laying_date)) +
  geom_boxplot() +
  labs(x = "NAO index", y = "Laying date") +
  geom_text(
    data = letters_df2,
    aes(x = group, y = ypos, label = cld),
    inherit.aes = FALSE,
    vjust = -0.5) +
  theme_minimal()

m7 <- lmer(laying_date ~ NAO_winter + (1|year), data = buzzard_dat)
summary(m7)
#Repeatability for year: 13.48 / (13.48 + 80.32) = 0.14371


#Model 8 - Laying date in relation to bird age
ggplot(data = buzzard_dat, aes(x = min_age, y = laying_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# use tukey test to see which groups are different
m8_1 <- aov(laying_date ~ age, data = buzzard_dat)
tukey_result3<-TukeyHSD(m8_1)
tukey_result3

p_values2 <- TukeyHSD(m8_1)$age[,"p adj"]
names(p_values2) <- rownames(TukeyHSD(m8_1)$age)
tukey_cld3 <- multcompView::multcompLetters(p_values2)
tukey_cld3

letters_df3 <- data.frame(
  group = c("young", "old"),
  cld = c("a", "b"),
  ypos = c(200, 200))

ggplot(data = buzzard_dat, aes(x = age, y = laying_date)) +
  geom_boxplot() +
  geom_text(
    data = letters_df3,
    aes(x = group, y = ypos, label = cld),
    inherit.aes = FALSE,
    vjust = -0.5) +
  labs(x = "Bird age", y = "Laying date") +
  theme_minimal()

m8 <- lmer(laying_date ~ min_age + (1|year), data = buzzard_dat)
summary(m8)


#Model 9 & 10 - Laying date for young VS old birds in relation to Hellmann and NAO
buzzard_young <- buzzard_dat |>
  dplyr::filter(age == "young")
buzzard_old <- buzzard_dat |>
  dplyr::filter(age == "old")

#Young birds
m9 <- lmer(laying_date ~ min_age + Hellmann_winter + NAO_winter + (1|year) + (1|ID_Buzzard), data = buzzard_young)
summary(m9)
confint(m9)

#Old birds
m10 <- lmer(laying_date ~ min_age + Hellmann_winter + NAO_winter + (1|year), data = buzzard_old)
summary(m10)


##################################################################
#Food abundance - laying date

#Is there a correlation between food abundance and laying date?
#  Buzzard laying date ~ vole abundance

#Model 11 - Laying date in relation to vole abundance
ggplot(data = buzzard_dat, aes(x = voleMarch, y = laying_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m11 <- lmer(laying_date ~ voleMarch + (1|ID_Buzzard) + (1|year), data = buzzard_dat)
summary(m11)










