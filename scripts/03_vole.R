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

breeding_counts <- bird |>
  count(ID_Buzzard) |>         # Replace 'ID_Buzzard' with the actual column name for individual IDs
  rename(breeding_events = n)   # Rename the count column for clarity

# Summarize the counts (how many individuals bred once, twice, etc.)
summary_counts <- breeding_counts |>
  count(breeding_events) |>    # Count the frequency of each breeding count
  rename(times_bred = breeding_events, num_individuals = n)
summary_counts 
#Plot with ggplot
ggplot(data = summary_counts, aes(x = times_bred, y = num_individuals)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of breeding events", y = "Number of individuals") +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) 
ggsave("plots/breeding_counts.png", width = 10, height = 5, dpi = 600)


#Assignment:
  #Is food abundance affected by either Hellmann’s or NAO ?
    #Vole abundance ~ Hellmann’s / NAO

#Check for outliers
hist(dat$Hellmann_winter)
hist(dat$NAO_winter)
hist(dat$voleMarch)


##################################################################

# DATA AND MODELS EXPLORATION (final models below)

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
#save as a csv file
write.csv(buzzard_dat, "buzzard_dat.csv")
  

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

m11_1 <- lm(laying_date ~ voleMarch, data = buzzard_dat)
summary(m11_1)

m11 <- lmer(laying_date ~ voleMarch + (1|ID_Buzzard) + (1|year), data = buzzard_dat)
summary(m11)

#install.packages("patchwork")
library(patchwork)

##################################################################

# CREATION OF THREE FINAL MODELS
  # MODEL 1 - March Vole abundance ~ Hellmann / NAO
  # MODEL 2 - Buzzard laying date ~ Vole abundance
  # MODEL 3 - Buzzard laying date ~ Bird age
    # EXTEND WITH SUBJECT CENTERED MODEL

##################################################################
# MODEL 1 - March Vole abundance ~ Hellmann / NAO - based on data base "dat" (environmental cov.)

#Hellmann and NAO
#Visualizing regression
ggplot(data = dat, aes(x = Hellmann_winter, y = NAO_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m1_1 <- lm(NAO_winter ~ Hellmann_winter, data= dat)
summary(m1_1)

m1_5 <- lm(voleMarch ~ year, data = dat)
summary(m1_5)

#Hellmann and voles
#Visualizing regression
p1 <- ggplot(data = dat, aes(x = Hellmann_winter, y = voleMarch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
p1
#NAO and voles
#Visualizing regression
p2 <- ggplot(data = dat, aes(x = NAO_winter, y = voleMarch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
p2
winter_voles <- p1 + p2 + patchwork::plot_layout(ncol = 2)
winter_voles
ggsave("plots/winter_voles.png", winter_voles, width = 10, height = 5, dpi = 600)
#Voles over the years
#Visualizing relation
ggplot(data = dat, aes(x = year, y = voleMarch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Effect of random variables
m1_2 <- lmer(voleMarch ~ (1|Hellmann_cat), data = dat)
summary(m1_2)
m1_3 <- lmer(voleMarch ~ (1|NAO_cat), data = dat)
summary(m1_3)
m1_6 <- lmer(voleMarch ~ (1|Hellmann_cat) + (1|NAO_cat), data = dat)
summary(m1_6)


# Final model
M1 <- lmer(voleMarch ~ (1|Hellmann_cat) + (1|NAO_cat), data = dat)
summary(M1)
confint(M1)
#Repeatability Hellmann: 14.66 / (14.66 + 177.38) = 0.07633826
#Repeatability NAO:      20.03 / (20.03 + 177.38) = 0.101464

###################

# MODEL 2 - Buzzard laying date ~ Winter harshness + Vole abundance - based on 
#   data base "buzzard_dat" (buzzard_friesland)

#Laying date in relation to Hellmann
#Visualizing regression
ggplot(data = buzzard_dat, aes(x = Hellmann_winter, y = laying_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m2_1 <- aov(laying_date ~ Hellmann_cat, data = buzzard_dat)
tukey_result1<-TukeyHSD(m2_1)
tukey_result1

tukey_cld1 <- multcompView::multcompLetters(TukeyHSD(m2_1)$Hellmann_cat[,"p adj"])
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

#Laying date in relation to NAO
ggplot(data = buzzard_dat, aes(x = NAO_winter, y = laying_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m2_2 <- aov(laying_date ~ NAO_cat, data = buzzard_dat)
tukey_result2<-TukeyHSD(m2_2)
tukey_result2

p_values <- TukeyHSD(m2_2)$NAO_cat[,"p adj"]
names(p_values) <- rownames(TukeyHSD(m2_2)$NAO_cat)
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

#Laying date in relation to vole abundance


ggplot(data = buzzard_dat, aes(x = voleMarch, y = laying_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Spring vole abundance", y = "Laying date (days from 1 January)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))
ggsave("plots/laying_date_vole.png", width = 10, height = 5, dpi = 600)


# Effect of random variables
m2_4 <- lmer(laying_date ~ (1|Hellmann_cat), data = buzzard_dat)
summary(m2_4)
m2_5 <- lmer(laying_date ~ (1|NAO_cat), data = buzzard_dat)
summary(m2_5)
m2_6 <- lmer(laying_date ~ (1|year), data = buzzard_dat)
summary(m2_6)
m2_7 <- lmer(laying_date ~ (1|ID_Buzzard), data = buzzard_dat)
summary(m2_7)
m2_9 <- lmer(laying_date ~ (1|voleMarch), data = buzzard_dat)
summary(m2_9)
#Hellmann could not be added:
m2_8 <- lmer(laying_date ~ (1|NAO_cat) + (1|year) + (1|ID_Buzzard) + (1|voleMarch), data = buzzard_dat)
summary(m2_8)


# Final model: vole abundance on laying date
M2_1 <- lmer(laying_date ~ voleMarch + (1|ID_Buzzard) + (1|year), data = buzzard_dat)
summary(M2_1)
confint(M2_1)
#Repeatability ID_Buzzard: 44.36 / (44.36 + 47.83) = 0.4811802
#Repeatability year:       10.87 / (10.87 + 47.83) = 0.1851789
#voleMarch intercept: 101.85 / slope: -0.193 / t value: -3.32: significant


###################

# MODEL 3 - Buzzard laying date ~ Bird age - based on
#   data base "buzzard_dat" (buzzard_friesland)

#Laying date in relation to bird age
ggplot(data = buzzard_dat, aes(x = min_age, y = laying_date)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m2_3 <- aov(laying_date ~ age, data = buzzard_dat)
tukey_result3<-TukeyHSD(m2_3)
tukey_result3

p_values2 <- TukeyHSD(m2_3)$age[,"p adj"]
names(p_values2) <- rownames(TukeyHSD(m2_3)$age)
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

# Effect of random variables
# See random effects explained in model 2
m3_1 <- lmer(laying_date ~ (1|age), data = buzzard_dat)
summary(m3_1)


# Final model: bird age on laying date
M3 <- lmer(laying_date ~ min_age + (1|year) + (1|ID_Buzzard), data = buzzard_dat)
summary(M3)
#Repeatability ID_Buzzard: 44.72 / (44.72 + 47.75) = 0.4836163
#Repeatability year:       16.40 / (16.40 + 47.75) = 0.2556508
#min_age intercept: 99.52 / slope: -0.107 / t value: -1.246: non-significant?


# SUBJECT CENTERED MODEL - to better look at effect of age on laying date
#Load package qdapTools to be able to use the lookup function
#install.packages("qdapTools")
library(qdapTools)

# Center age per ID_Buzzard (individual)
ind_age <- aggregate(cbind(min_age) ~ ID_Buzzard, buzzard_dat, mean)
buzzard_dat$btw_ind_avg_age <- lookup(buzzard_dat$ID_Buzzard, ind_age[, c("ID_Buzzard", "min_age")])

# Center age for each individual (within-individual effect)
buzzard_dat$wth_ind_age_cen <- buzzard_dat$min_age - buzzard_dat$btw_ind_avg_age  # Center age by subtracting the individual's mean age

# Fit mixed model with age_cen (within-individual effect) and avg_age (between-individual effect)
M4 <- lmer(laying_date ~ wth_ind_age_cen + btw_ind_avg_age + (1 | ID_Buzzard), data = buzzard_dat)

# View model summary and confidence intervals
summary(M4)
confint(M4)

#Confidence intervals just overlap, meaing that the within individual and between individual effects are 
#not significantly different from each other. This means that the age effect on laying date is not significant.
43.34 / (43.34 + 62.22) = 0.41
62.22 / (43.35 + 62.22) = 0.59


# Plotting of the final models
# Add predicted values to the data
buzzard_dat$predicted <- predict(M4)

# Within-individual effect: Plot wth_ind_age_cen
p3 <- ggplot(buzzard_dat, aes(x = wth_ind_age_cen, y = predicted)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Within-Individual Effect of Age (Centered)",
       x = "Centered Age (within individual)",
       y = "Laying Date (days from 1 January)") +
  theme_minimal() +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))
p3

# Between-individual effect: Plot btw_ind_avg_age
p4 <- ggplot(buzzard_dat, aes(x = btw_ind_avg_age, y = predicted)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Between-Individual Effect of Average Age",
       x = "Average Age (between individual)",
       y = "Laying Date (days from 1 January)") +
  theme_minimal() +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))
p4

# Combine the plots
final_model_plot <- p3 + p4 + patchwork::plot_layout(ncol = 2)
final_model_plot
ggsave("plots/final_model_cent_plot.png", final_model_plot, width = 10, height = 5, dpi = 600)







