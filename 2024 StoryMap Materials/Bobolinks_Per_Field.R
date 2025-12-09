
## This script is about the Volunteer Grassland Bird Monitoring project at Notchview
## in the nesting season of 2024.

## Packages: dplyr, tidyverse, readxl, readr, ggplot2


###########################################################################
# This script successfully takes a single dataset, tidies it, takes the average
# number of bobolinks seen at each field per visit, and plots it on a bar plot.
# It works for all of the 2024 datasets!
###########################################################################

library("tidyverse", "dplyr", "ggplot")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data/Property_Datasets/")
Property_Bobolinks_24 <- read_csv("Bobolinks(Appleton).csv", na = c("N/A", ""))


Property_Bobolinks <- Property_Bobolinks_24 |>
  rename(
    Under_50_Males = `<50_Males`,
    Under_50_Females = `<50_Females`,
    Under_50_Total = `<50_Total`,
    Over_50_Males = `>50_Male`,
    Over_50_Females = `>50_Females`,
    Over_50_Total = `>50_Total`,
    Bobolinks = `Total_Counts`,
    Visits = `Date`
  ) |>
  
  mutate(
    Point = factor(Point),
    Field = factor(Field)
  ) |>
  relocate(
    Bobolinks, .after = Field
  ) |>
  
## Above is data tidying, below are the transformations.
  
  select(
    Field, Point, Bobolinks
  ) |>
  arrange(
    Field, Point
  ) |>
  filter(
    !c(Bobolinks == "#VALUE!")
  )



## Below is making a second data frame that counts how many times each
## point was visited, and takes an average. Not every point was visited the 
## same amount of times per field (volunteer missed a point in a visit, for 
## instance), so the average is total visits across all points / number of
## points for each field. This is the average amount of times each field was
## visited.

Survey_Points <- Property_Bobolinks |>
  select(
    Field, Point
  ) |>
  group_by(
    Field
  ) |>
  count(
    Point
  ) |>
  rename(
    Total_Visits = `n`
  ) |> 
  distinct(
    Point, Total_Visits
  ) |>
  pivot_wider(
    names_from = Point,
    values_from = Total_Visits
  ) |>
  
  rowwise() |>
  
  mutate(
    Field_Visits = mean(c_across(everything()), na.rm = TRUE)
  )

sp2<-app_dex_tidy%>%
    select(
    Field, Point
  ) |>
  group_by(
    Field
  ) |>
  count(
    Point
  ) |>
  rename(
    Total_Visits = `n`
  )

## Below is counting the bobolinks at each point, and then merging
## the two data frames

Property_Bobolinks <- Property_Bobolinks |>
  select(
    Field, Point, Bobolinks
  ) |>
  arrange(
    Field, Point
  ) |>
  group_by(
    Field, Point
  ) |>
  mutate(
    Bobolinks = sum(as.numeric(Bobolinks))
  ) |>
  distinct(
    Field, .keep_all = TRUE
  ) |>
  arrange(
    Point
  ) |>
  pivot_wider(
    names_from = Point,
    values_from = Bobolinks
  ) |>
  
  {\(.) {replace(.,is.na(.),0)}}() |>
  
  ## Everything up to here is good. Here Notchview_Bobolinks is tidy with point
  ## names in the column headers, and bobolink data in the data, and na = 0. 
  ## Next, you're trying to take the sum of each row to have the total bobolinks
  ## seen at each field (sum of all the points per field), which you will then
  ## average by the amount of times that field was visited.
  
  ungroup(
    Field
  ) |>
  rowwise() |>
  mutate(
    Total_Bobolinks = sum(c_across(-(1)))
  ) |>
  select(
    Field, Total_Bobolinks
  ) |>
  right_join(
    Survey_Points
  ) |>
  relocate(
    Field_Visits,
    .before = Total_Bobolinks
  ) |>
  group_by(
    Field
  ) |>
  mutate(
    avg_Bobolinks = Total_Bobolinks / Field_Visits
  ) |>
  group_by(
    Field
  )

## EVERYTHING UP TO HERE WORKS. Up to here, you have a data frame with the 
## field names, each point, Total_Visits (to each field), and Total_Bobolinks 
##(total seen at each field over the season).
## Last line takes the average bobolinks seen at each field per visit.

ggplot(Property_Bobolinks, aes(x = Field, y = avg_Bobolinks)) +
  geom_bar(
    width = 0.6,
    stat = "identity",
    color = "black",
    fill = "#99CCCC"
  ) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.9)) +  
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(limits = c(0,10)) +
  labs(title = "Bobolink Observations", y = "Average Bobolinks") +
  theme(plot.title = element_text(face = "bold", size = 17.5, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15, vjust = 1)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(plot.background = element_rect(color = "black", fill = NA, linewidth = 1))




#going to read in appleton and dexter as one
app <- read_csv("Bobolinks(Appleton).csv", na = c("N/A", ""))
dex <- read_csv("Bobolinks(Dexter).csv", na = c("N/A", ""))%>%
  select(-Notes)
app_dex<-rbind(app, dex)
##tidy full and transform full set
app_dex_tidy <- app_dex |>
  rename(
    Under_50_Males = `<50_Males`,
    Under_50_Females = `<50_Females`,
    Under_50_Total = `<50_Total`,
    Over_50_Males = `>50_Male`,
    Over_50_Females = `>50_Females`,
    Over_50_Total = `>50_Total`,
    Bobolinks = `Total_Counts`,
    Visits = `Date`) |>
  mutate(
    Point = factor(Point),
    Field = factor(Field)) |>
  relocate( Bobolinks, .after = Field) |>
  select( Field, Point, Bobolinks) |>
  arrange( Field, Point) |>
  filter(!c(Bobolinks == "#VALUE!") )
