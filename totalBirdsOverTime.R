


# This script takes the Master_TargetBirds spreadsheet, filters by a chosen
# property and target species, and creates a plot showing how many of the
# target species were observed on any given monitoring day throughout the
# nesting season.

## NOTE: properties where people monitored multiple routes, like Appleton and
# Notchview, might have slightly skewed data. This script simply takes a sum
# of the observations on a given day, and divides it by the number of monitors
# who visited on that given day. This gives the average amount of birds of 
# the chosen species observed on that day.



library("tidyverse", "dplyr", "ggplot", "lubridate")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Desktop")
Master_Birds <- read_csv("Master_TargetBirds.csv", na = c("N/A", ""))

Target_Seen <- Master_Birds |>
  rename(
    Target_Bird = `Target Bird`
  ) |>
  filter(
    !c(within_50_Total == "#VALUE!"),
    !c(within_50_Total == "NA")
  ) |>
  mutate(
    Date = mdy(paste(Month, Day, Year, sep = "/"))
  ) |>
  mutate(
    Point = factor(Point),
    Field = factor(Field),
    Date = as.Date(Date, format = "%m/%d/%Y"),
  ) |>
  relocate(
    Total_Target, .after = Field
  ) |>
  
  ## Above is data tidying, below are the transformations.
  ##################################################################
##################################################################

filter(
  Property == "Appleton Farms",
  Target_Bird == "Bobolink"
) |>
  
  ##################################################################
##################################################################

select(
  Date, Total_Target, Surveyor
  ) |>
  group_by(
    Date
  ) |>
  mutate(
    sum_Target = sum(as.numeric(Total_Target))
  ) |>
  select(
    -Total_Target
  ) |>
  distinct(
    Date, Surveyor, .keep_all = TRUE
  ) |>
  mutate(
    count_Surveyor = n(),
    sum_Target = sum_Target / count_Surveyor
  ) |>
  select(
    -Surveyor, -count_Surveyor
  ) |>
  mutate(
    month.name = format(Date, "%B"),
    Day = as.numeric(format(Date, "%d")),
    month.name = factor(month.name, levels = c("January", "February", "March",
                                               "April", "May", "June", "July", "August",
                                               "September", "October", "November",
                                               "December"))
  ) |>
  rename(
    "Month" = month.name
  ) |>
  group_by(
    Month
  ) |>
  arrange(
    Date
  )

colors <- c("April" = "#0066CC", "May" = "#CC99FF", "June" = "#66CC66",
            "July" = "#FF9933", "August" = "#CC0000")

ggplot(Target_Seen, aes(x = Date, y = sum_Target, fill = Month)) +
  geom_bar(
    width = 1,
    stat = "identity", 
    position = position_dodge2(preserve = "single")
  ) + 
  scale_fill_manual(values = colors) +
  labs(title = "Grassland Bird Observations \n Throughout the Nesting Season",
       x = "Day", y = "Observations") +
  theme(plot.title = element_text(face = "bold", size = 17.5, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15, vjust = 0.3)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text = element_text(color = "black")) +
  theme(plot.background = element_rect(color = "black", fill = NA, linewidth = 1))



