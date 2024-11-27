# restore libraries
rm(list = ls()) # clear environment

# load libraries
library(tidyverse)

env <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=1857363379&single=true&output=csv") |>
  dplyr::select(year, Hellmann_winter, NAO_winter)

vole <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=1857363379&single=true&output=csv") |>
  dplyr::select(year, voleMarch)
  
#Assignment:
  #Is food abundance affected by either Hellmann’s or NAO ?
    #Vole abundance ~ Hellmann’s / NAO


