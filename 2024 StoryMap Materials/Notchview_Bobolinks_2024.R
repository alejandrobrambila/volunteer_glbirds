
## This script is about the Volunteer Grassland Bird Monitoring project at Notchview
## in the nesteing season of 2024.

## Questions I want to answer: Where were the bobolinks seen primarily (different fields)?
## When during the year were they typically found?

## Packages: dplyr, tidyverse, readxl, readr, ggplot2

library("tidyverse", "dplyr", "ggplot")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data/")
Notchview_Bobolinks <- read_csv("Notchview_Bobolinks_2024.csv", , na = c("N/A", ""))

Notchview_Bobolinks |>
  rename(
    Under_50_Males = `<50_Males`,
    Under_50_Females = `<50_Females`,
    Under_50_Total = `<50_Total`,
    Over_50_Males = `>50_Male`,
    Over_50_Females = `>50_Females`,
    Over_50_Total = `>50_Total`
  ) |>
  mutate(
    Point = factor(Point)
  ) |>
  mutate(
    avg_Total_Counts = mean(Total_Counts)
  ) |>
  group_by(
    Field
  ) |>
  select(
    Property, Field, avg_Total_Counts
  ) 


ggplot(
  data = Notchview_Bobolinks, aes(x = Field, y = avg_Total_Counts)) +
  geom_point()






