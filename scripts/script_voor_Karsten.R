#Ik kijk hoe tijd (Year) de layingdate kan voorspellen
#Mijn "subject" is female ID (FemID)
#mijn dataframe heet "d"

#Within and between subject variation
#Center Year  per individual
x <- d %>% 
  dplyr::group_by(FemID) %>%
  summarise(Btw_Ind_Y = mean(Year)) #between individual year

## Between individual effect: mean year for each female! This is how individuals differ
d <- d %>% left_join(x, by = "FemID")

## Within individual effect: how each value differs from individual 
d <- d %>% 
  dplyr::mutate(within_ind_Y = Year - Btw_Ind_Y)