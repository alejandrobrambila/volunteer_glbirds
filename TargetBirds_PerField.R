

###########################################################################
# This script successfully takes a single data set, tidies it, takes the average
# number of target birds seen at each field per visit, and plots it on a bar plot.
# It works for all of the 2025 data sets!

# The point of this is to show and estimate of the number of target birds using
# the field throughout the nesting season. It takes an average based on the 
# number of visits to the field in order to avoid double counting birds on 
# multiple visits (ie counting the same birds on visits 1 and 2)
###########################################################################


#######################################################
# These first three lines are where you might have to edit the script to match
# your file name and location. The "library" line is fine the way it is. Do
# not touch it. The "setwd" line codes the location of your file. The 
# "Property_Birds" line codes to the name of the file. Please see the 
# "(General) Using R.docx" file to learn about what to do with these lines.

library("tidyverse", "dplyr", "ggplot")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Desktop")
Property_Birds <- read_csv("Master_TargetBirds(Sheet1).csv", na = c("N/A", ""))

#######################################################

# Now below is where the script starts to work with the data set

Property_Birds <- Property_Birds |>
  rename(
    Total_Observations = `Total_Target`,
    Target_Bird = `Target Bird`
  ) |>
  mutate(
    Point = factor(Point),
    Field = factor(Field)
  ) |>
  relocate(
    Total_Observations, .after = Field
  ) |>
  
  ## Above is data tidying, below are the transformations.
  
  select(
    Property, Field, Point, Target_Bird, within_50_Total
  ) |>
  arrange(
    Field, Point
  ) |>
  filter(
    !c(within_50_Total == "#VALUE!"),
    !c(within_50_Total == "NA")
  ) |>
  
##############################################################
# This "filter" line below is where you choose the property that you are
# interested in. Simply replace the property name, within the "", with the
# property of your choice. MUST be exactly what is written in the Excel file.

  filter(
    Property == "Appleton Farms"
  ) 
  
##############################################################



## Below is making a second data frame that counts how many times each
## point was visited, and takes an average. Not every point was visited the 
## same amount of times per field (volunteer missed a point in a visit, for 
## instance), so the average is total visits across all points / number of
## points for each field. This is the average amount of times each field was
## visited.

## "Field_Visits" is divided by 4 because there are four target species. 
## Otherwise, it counts each target species on each individual day as its own
## separate visit.

Survey_Points <- Property_Birds |>
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
    Field_Visits = mean(c_across(everything()), na.rm = TRUE),
    Field_Visits = Field_Visits / 4
  )

## Below is counting the bobolinks at each point, and then merging
## the two data frames

Property_Birds <- Property_Birds |>
  select(
    Field, Point, Target_Bird, within_50_Total
  ) |>

#########################################################
# This "filter" line below is where you choose the target species that you want
# to look at. Replace Bobolink with Savannah Sparrow, Grasshopper Sparrow, or
# Eastern Meadowlark (within the "") to get the target bird of choice.

  filter(
    Target_Bird == "Bobolink"
  ) |>
##########################################################

  arrange(
    Field, Point
  ) |>
  group_by(
    Field, Point
  ) |>
  mutate(
    Total_Target = sum(as.numeric(within_50_Total))
  ) |>
  select(
    -within_50_Total
  ) |>
  distinct(
    Field, .keep_all = TRUE
  ) |>
  arrange(
    Point
  ) |>
  
  {\(.) {replace(.,is.na(.),0)}}() |>
  
  ## Next, you're trying to take the sum of each row to have the total birds
  ## seen at each field (sum of all the points per field), which you will then
  ## average by the amount of times that field was visited.
  
  ungroup(
    Field
  ) |>
  select(
    Field, Total_Target
  ) |>
  right_join(
    Survey_Points
  ) |>
  relocate(
    Field_Visits,
    .before = Total_Target
  ) |>
  group_by(
    Field
  ) |>
  mutate(
    avg_Target = Total_Target / Field_Visits
  ) |>
  select(
    Field, Field_Visits, avg_Target
  ) |>
  group_by(
    Field
  ) |>
  summarize(
    avg_Target = sum(avg_Target)
  )
  


## Up to here, you have a data frame with the 
## field names, each point, Total_Visits (to each field), and Total_Bobolinks 
## (total seen at each field over the season).


ggplot(Property_Birds, aes(x = Field, y = avg_Target)) +
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
  labs(title = "Target Bird Observations", y = "Average Observations Per Visit") +
  theme(plot.title = element_text(face = "bold", size = 17.5, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15, vjust = 1)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(plot.background = element_rect(color = "black", fill = NA, linewidth = 1))

