
#This script is for determining the male/female split of the bobolink 
#observations at a given property (using the full 2024 dataset), and connecting
#it to the volunteer effort/skill at identifying male vs female.

#The process for the script takes the full property data set, chooses a given 
#property, and plots the within and outside 50m males vs females observed,
#with volunteers specified by color. The data spans the whole nesting season.
###############################################################################



library("tidyverse", "dplyr", "ggplot2", "patchwork")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data/2024storymap")
###Raw_Data <- read.csv("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data/2024storymap/Property_Datasets/Bobolinks(Dexter).csv", na = c("N/A", ""))
Raw_Data <- read.csv("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data/2024storymap/All_Prop_2024_Bobolinks.csv", na = c("N/A", ""))


Property_Surveyor_2024 <- Raw_Data |>
  rename(
    Bobolinks = `Total_Counts`
  ) |>
  mutate(
    Property = factor(Property),
    Point = factor(Point),
    Field = factor(Field),
    Surveyor = factor(Surveyor),
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Bobolinks = ifelse(Bobolinks == "#VALUE!", 0, Bobolinks),
    Bobolinks = as.numeric(Bobolinks),
    within_50_Male = as.numeric(within_50_Male),
    outside_50_Male = as.numeric(outside_50_Male),
    within_50_Female = as.numeric(within_50_Female),
    outside_50_Female = as.numeric(outside_50_Female),
    outside_50_Total = as.numeric(outside_50_Total),
    within_50_Total = as.numeric(within_50_Total)
  ) |>
  select(
    Property,
    Surveyor,
    within_50_Male,
    within_50_Female,
    outside_50_Male,
    outside_50_Female
  ) |>
  group_by(
    Property
  ) |>
  
###############################################################################
## This line below is where you change the property name to get the property
## you want
  
  filter(
    Property == "Dexter Drumlin"
  ) |>
  
###############################################################################
  
  
  arrange(
    Surveyor
  ) |>
  relocate(
    outside_50_Male,
    .after = within_50_Male
  ) |>
  relocate(
    outside_50_Female,
    .after = within_50_Female
  ) |>
  ungroup()

# Property_Surveyor_2024[Property_Surveyor_2024 == 0] <- NA
  
 
### Now below is making the scatter plots  

plot_within <- ggplot(Property_Surveyor_2024, aes(x = within_50_Male, 
                                    y = within_50_Female,
                                    color = Surveyor)) +
         geom_point(
           stat = "identity",
           position = "jitter",
           na.rm = FALSE,
           size = 1.5,
           alpha = 0.75,
           show.legend = FALSE 
         ) +
  labs(x = "Males Within 50m", y = "Females Within 50m") +
  theme(plot.background = element_rect(color = "black")) 
#  scale_x_discrete(limits = c(1,6)) +
#  scale_y_discrete(limits = c(1,4))


plot_outside <- ggplot(Property_Surveyor_2024, aes(x = outside_50_Male, 
                                    y = outside_50_Female,
                                    color = Surveyor)) +
        geom_point(
          stat = "identity",
          position = "jitter",
          na.rm = FALSE,
          size = 1.5,
          alpha = 0.75
        ) +
  labs(x = "Males Outside of 50m", y = "Females Outside of 50m") +
  theme(plot.background = element_rect(color = "black")) 
#  scale_x_discrete(limits = c(0,6)) +
#  scale_y_discrete(limits = c(0,4))
  
(guide_area() / (plot_within + plot_outside)) +
  plot_annotation(
    title = "Bobolinks Observed by Each Volunteer",
  ) +
  plot_layout(
    guides = "collect",
    heights = c(0.25, 2.5)
  ) &
  theme(legend.position = "top")


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
