
###############################################################################
## This script takes monitoring data and calculates the average amount of
## bobolinks seen per visit. A "visit" includes each point in a field to account
## for any potential overlap. Essentially, the two data frames in this script 
## are as follows: "Target_Seen" counts the number of bobolinks seen on a 
## given day, across all fields and points. "Points_Visited" includes the 
## column "sum_Visits," which is the total amount of points that were visited
## that day (some were visited twice in a day by different people, so each of
## these counts as a separate visit (this maybe shouldn't be the case...)). The
## average displayed in the plot is the total number of bobolinks seen on a 
## given day divided by the number of points visited in a day.
###############################################################################


library("tidyverse", "dplyr", "ggplot", "lubridate")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Desktop")
Master_Birds <- read_csv("Master_TargetBirds.csv", na = c("N/A", ""))

Target_Seen <- Master_Birds |>
  rename(
    Target_Bird = `Target Bird`
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
    Property == "Jewell Hill",
    Target_Bird == "Bobolink"
  ) |>
  
##################################################################
##################################################################

  select(
    Date, Total_Target
  ) |>
  filter(
    !c(within_50_Total == "#VALUE!"),
    !c(within_50_Total == "NA")
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
    Date, .keep_all = TRUE
  )

###### Making a second data set to count the number of times that each point
# was visited

Points_Visited <- Master_Birds |>
  mutate(
    Date = mdy(paste(Month, Day, Year, sep = "/"))
  ) |>
  mutate(
    Point = factor(Point),
    Field = factor(Field),
    Date = as.Date(Date, format = "%m/%d/%Y")
  ) |>
  select(
    Date, Field, Point
  ) |>
  group_by(
    Date, Field
  ) |>
  arrange(
    Date, Field
  ) |>
  count(
    Point
  ) |>
  rename(
    "Daily_Visits" = "n"
  ) |>
  pivot_wider(
    names_from = Point,
    values_from = Daily_Visits
  ) |>
  
  {\(.) {replace(.,is.na(.),0)}}() |>
  
  ungroup(
    Date, Field
  ) |>
  rowwise() |>
  mutate(
    Total_Visits = sum(c_across(-(1:2)))
  ) |>
  relocate(
    Total_Visits,
    .after = Field
  ) |>
  select(
    Date, Field, Total_Visits
  ) |>
  group_by(
    Date
  ) |>
  pivot_wider(
    names_from = Field,
    values_from = Total_Visits
  ) |>
  
  {\(.) {replace(.,is.na(.),0)}}() |>
  
  rowwise() |>
  mutate(
    sum_Visits = sum(c_across(everything()))
  ) |>
  relocate(
    sum_Visits,
    .after = Date
  ) |>
  select(
    Date, sum_Visits
  ) |>
  left_join(
    Target_Seen
  ) |>
  mutate(
    avg_Target = sum_Target / sum_Visits,
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

ggplot(Points_Visited, aes(x = Date, y = avg_Target, fill = Month)) +
  geom_bar(
    width = 1,
    stat = "identity", 
    # fill = "#CC99FF",
    color = "black",
    position = position_dodge2(preserve = "single")
  ) + 
  scale_fill_manual(values = colors) +
  labs(title = "Target Observations Throughout the Nesting Season",
       x = "Day", y = "Average Target") +
  theme(plot.title = element_text(face = "bold", size = 17.5, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15, vjust = 0.3)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text = element_text(color = "black")) +
  theme(plot.background = element_rect(color = "black", fill = NA, linewidth = 1))

