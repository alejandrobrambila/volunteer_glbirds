
## The purpose of this script is to compare the 2024 bobolinks data with 2023 
## to see if there was any change in the bobolink numbers, and if so, where
## it occurred. Realistically, only a drastic difference will be significant.

library("tidyverse", "dplyr", "ggplot2")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data/2023materials")
Full_2023_raw <- read_csv("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data/2023materials/All_Bobolinks_Data_2023.csv", na = c("N/A", ""))
Full_2024_raw <- read_csv("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data/2024storymap/All_Prop_2024_Bobolinks.csv", na = c("N/A", ""))

Full_2023 <- Full_2023_raw |>
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
    Property = factor(Property),
    Point = factor(Point),
    Field = factor(Field),
    Bobolinks = ifelse(Bobolinks == "#VALUE!", 0, Bobolinks),
    Bobolinks = as.numeric(Bobolinks)
  ) |>
  relocate(
    Bobolinks, .after = Field
  ) |>
  select(
    Property, Field, Point, Bobolinks
  ) |>
  select(
    Property, Field, Point, Bobolinks
  ) |>
  group_by(
    Property
  ) |>
  arrange(
    Property, Field, Point
  ) |>
  select(
    -Bobolinks
  ) |>
  distinct(
    .keep_all = TRUE
  ) |>
  group_by(
    Property, Field
  ) |>
  summarize(
    Property, Field,
    n = n()
  ) |>
  arrange(
    Property
  ) |>
  rename(
    "Num_of_Points" = n
  ) |>
  distinct(
    Num_of_Points,
    .keep_all = TRUE
  )

## Below is making a second data frame that counts how many times each
## point was visited

Visits_2023 <- Full_2023_raw |>
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
  filter(
    !c(Bobolinks == "#VALUE!")
  ) |>
  mutate(
    Property = factor(Property),
    Point = factor(Point),
    Field = factor(Field),
    Bobolinks = as.numeric(Bobolinks)
  ) |>
  relocate(
    Bobolinks, .after = Field
  ) |>
  select(
    Property, Field, Bobolinks
  ) |>
  group_by(
    Property, Field
  ) |>
  arrange(
    Property, Field
  ) |>
  mutate(
    sum_Bobolinks = sum(Bobolinks)
  ) |>
  select(
    -Bobolinks
  ) |>
  distinct(
    sum_Bobolinks,
    .keep_all = TRUE
  ) |>
  left_join(
    Full_2023
  ) |>
  mutate(
    Bobolinks_per_Field = sum_Bobolinks / Num_of_Points
  ) |>
  select(
    -sum_Bobolinks,
    -Num_of_Points
  ) |>
  pivot_wider(
    names_from = Field,
    values_from = Bobolinks_per_Field
  ) |>
  
  {\(.) {replace(.,is.na(.),0)}}() |>
  
  rowwise() |>
  
  mutate(
    Seasonal_Bobolinks = sum(c_across(-1))
  ) |>
  relocate(
    Seasonal_Bobolinks,
    .after = Property
  ) |>
  select(
    Property, Seasonal_Bobolinks
  ) |>
  ungroup() |>
  mutate(
    Year = "2023"
  )


########## Separating the 2023 data (above) and the 2024 (below)

Full_2024 <- Full_2024_raw |>
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
    Property = factor(Property),
    Point = factor(Point),
    Field = factor(Field),
    Bobolinks = ifelse(Bobolinks == "#VALUE!", 0, Bobolinks),
    Bobolinks = as.numeric(Bobolinks)
  ) |>
  relocate(
    Bobolinks, .after = Field
  ) |>
  select(
    Property, Field, Point, Bobolinks
  ) |>
  select(
    Property, Field, Point, Bobolinks
  ) |>
  group_by(
    Property
  ) |>
  arrange(
    Property, Field, Point
  ) |>
  select(
    -Bobolinks
  ) |>
  distinct(
    .keep_all = TRUE
  ) |>
  group_by(
    Property, Field
  ) |>
  summarize(
    Property, Field,
    n = n()
  ) |>
  arrange(
    Property
  ) |>
  rename(
    "Num_of_Points" = n
  ) |>
  distinct(
    Num_of_Points,
    .keep_all = TRUE
  )

## Below is making a second data frame that counts how many times each
## point was visited

Visits_2024 <- Full_2024_raw |>
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
  filter(
    !c(Bobolinks == "#VALUE!")
  ) |>
  mutate(
    Property = factor(Property),
    Point = factor(Point),
    Field = factor(Field),
    Bobolinks = as.numeric(Bobolinks)
  ) |>
  relocate(
    Bobolinks, .after = Field
  ) |>
  select(
    Property, Field, Bobolinks
  ) |>
  group_by(
    Property, Field
  ) |>
  arrange(
    Property, Field
  ) |>
  mutate(
    sum_Bobolinks = sum(Bobolinks)
  ) |>
  select(
    -Bobolinks
  ) |>
  distinct(
    sum_Bobolinks,
    .keep_all = TRUE
  ) |>
  left_join(
    Full_2024
  ) |>
  mutate(
    Bobolinks_per_Field = sum_Bobolinks / Num_of_Points
  ) |>
  select(
    -sum_Bobolinks,
    -Num_of_Points
  ) |>
  pivot_wider(
    names_from = Field,
    values_from = Bobolinks_per_Field
  ) |>
  
  {\(.) {replace(.,is.na(.),0)}}() |>
  
  rowwise() |>
  
  mutate(
    Seasonal_Bobolinks = sum(c_across(-1))
  ) |>
  relocate(
    Seasonal_Bobolinks,
    .after = Property
  ) |>
  select(
    Property, Seasonal_Bobolinks
  ) |>
  ungroup() |>
  mutate(
    Year = "2024"
  )

### Below is merging Visits_2023 and Visits_2024 into one data frame, so I can
### put them into the same plot the way I want to

All_Visits_2324 <- 
  bind_rows(Visits_2023, Visits_2024) |>
  arrange(
    Property
  ) |>
  mutate(
    Year = as.factor(Year)
  ) 


###########################################################



ggplot(All_Visits_2324, aes(x = reorder(Property, -Seasonal_Bobolinks), 
                            y = Seasonal_Bobolinks, 
                            fill = Year)) +
  geom_bar(
    width = 0.85,
    stat = "identity",
    position = "dodge",
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.x = element_text(vjust = 0.35)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  labs(title = "Bobolinks", subtitle = "Bobolink Observations Over Two Years
       of Monitoring", x = "Property", y = "Bobolink Observations")
  
 


