
## Packages: dplyr, tidyverse, readxl, readr, ggplot2

## Question answered here: what fields at the property are more bobolinks seen?

###########################################################################
# This script is trying to do what the "Bobolinks Per Field" script does,
# but for the whole big data set all together. So far, it doesn't work.
###########################################################################

library("tidyverse", "dplyr", "ggplot")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Desktop/Grassland_Bird_Data")
Full_Data_All_Bobolinks <- read_csv("All_Prop_2024_Bobolinks.csv", na = c("N/A", ""))

All_Fields <- Full_Data_All_Bobolinks |>
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
  
## Above is data tidying, below are the transformations.
  
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

All_Bobolinks <- Full_Data_All_Bobolinks |>
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
    sum_Bobolinks = sum(Bobolinks) #at this point you are at number of bobolinks at each point*visit, the sum column shows total seen in that field over the season
  ) |>
  select(
    -Bobolinks
  ) |>
  distinct(
    sum_Bobolinks,
    .keep_all = TRUE
  ) |>
  left_join(
    All_Fields
  ) |>
  mutate(
    Bobolinks_per_Field = sum_Bobolinks / Num_of_Points #new controls for num of points. assumption: each point is a sample of looking at the same birds. alternatives are adding across points (fully independent points), or adding across <50m averaging across >50m (semi-independent points). final option might require averaging before adding up over time.  
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
    Seasonal_Bobolinks = sum(c_across(-1)) #adding up within property across fields
  ) |>
  relocate(
    Seasonal_Bobolinks,
    .after = Property
  ) |>
  select(
    Property, Seasonal_Bobolinks
  ) |>
  ungroup() |>
  
## The below mutate() calculates the total number of bobolinks seen statewide.
  
  mutate(
    Everywhere_Bobolinks = sum(Seasonal_Bobolinks)
  )

## Up to here, the script makes a table with the number of bobolinks seen at
## each property over the whole nesting season. The bobolink values include an
## mean calculation factoring in the amount of points per field in order to 
## avoid data inflation of monitors seeing/counting the same bobolinks multiple
## times while monitoring different points at a given field.

## Below is simply calculating the number of times each property was visited.
## This doesn't factor in each field or how many points at each field, etc, it
## is just the number of unique dates that each property was visited. This is
## more to highlight volunteer effort than make super critical analysis.

## AB note: I think this is actually a pretty good match for the first plot.
## Because you have already controlled for number of points in a field, this
## controls for how many circuits were monitored - which should deal both with
## frequency of visitation and number of fields at a farm. I think those are the
## three sampling elements to make sure you're accounting for in any analysis. 

All_Visits <- Full_Data_All_Bobolinks |>
  rename(
    Under_50_Males = `<50_Males`,
    Under_50_Females = `<50_Females`,
    Under_50_Total = `<50_Total`,
    Over_50_Males = `>50_Male`,
    Over_50_Females = `>50_Females`,
    Over_50_Total = `>50_Total`,
    Bobolinks = `Total_Counts`,
  ) |>
  mutate(
    Property = factor(Property),
    Point = factor(Point),
    Field = factor(Field),
    Bobolinks = ifelse(Bobolinks == "#VALUE!", 0, Bobolinks),
    Bobolinks = as.numeric(Bobolinks)
  ) |>
  select(
    Property, Date # only thing you might miss here is two people doing two different circuits on the same day. might keep circuit (the 1-2-3 of the point numbers) or observer (if they have to be doing distinct circuits) to run distinct in the next line on
  ) |>
  distinct(
    Property, Date,
    .keep_all = TRUE
  ) |> 
  group_by(
    Property
  ) |>
  count(
    Property
  ) |>
  rename(
    "Total_Visits" = n
  ) |>
  ungroup(
    Property
  ) |>
  
## This mutate line below just calculates the total number of visits the 
## volunteers made statewide.
  
  mutate(
    Everywhere_visits = sum(Total_Visits)
  ) |>
  
## Making one dataframe ("All_Visits") for the purpose of facet_wrap()
  
  left_join(
    All_Bobolinks
  )

## Below counts the number of volunteers. Does NOT include instances where 
## multiple people monitored the same field at the same time (estimating just
## for the general number)

All_Volunteers <- Full_Data_All_Bobolinks |>
  select(
    Property, Surveyor
  ) |>
  distinct(
    Surveyor,
    .keep_all = TRUE
  ) |>
#  count(
 #   Surveyor
 # ) |>
  count(Property)|> # I think this is what you were trying to do? kept previous text if not
  mutate(
    Total_Volunteers = sum(n)
  )



  ggplot(All_Bobolinks, aes(y = Seasonal_Bobolinks, 
                            x = reorder(Property, -Seasonal_Bobolinks))) +
    geom_bar(
      width = 0.8,
      stat = "identity",
      color = "black",
      fill = "#FF9933"
    ) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(axis.text.x = element_text(vjust = 0.35)) +
    theme(axis.text.x = element_text(hjust = 1)) +
    labs(title = "Bobolinks", subtitle = "Number of Bobolinks Seen at Each
         Property Throughout the Nesting Season", x = "Property", 
         y = "Total Bobolinks")
  
  ggplot(All_Visits, aes(x = reorder(Property, -Total_Visits), y = Total_Visits)) +
    geom_bar(
      width = 0.8,
      stat = "identity",
      color = "black",
      fill = "#88CC00"
    ) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(axis.text.x = element_text(vjust = 0.35)) +
    theme(axis.text.x = element_text(hjust = 1)) +
    labs(title = "Property Visits", subtitle = "How Many Times Each Property
         Was Visited", x = "Property", y = "Total Visits")

  ggplot


#fully joined data set for the facet wrap
  All_Joined<-left_join(All_Visits, All_Volunteers)%>%
    pivot_longer(cols=c(Total_Visits, Seasonal_Bobolinks, n), names_to = "var", values_to="val")
  
ggplot(All_Joined, aes(x = reorder(Property, -val), y = val)) + #the reorder came out a little wonky, because it's trying to keep all three facets in the same order
    geom_bar(
      width = 0.8,
      stat = "identity",
      color = "black",
      fill = "#88CC00"
    ) +
  facet_wrap(~var, scales="free")+
    theme(axis.text.x = element_text(angle = 90)) +
    theme(axis.text.x = element_text(vjust = 0.35)) +
    theme(axis.text.x = element_text(hjust = 1)) +
    labs(title = "Property Visits", subtitle = "How Many Times Each Property #will have to rename facets by their headers not full header in this one
         Was Visited", x = "Property", y = "Total Visits") #same with x axis. 
  
#AB: can also try comparing properties (or also at the field level)
All_Joined2<-left_join(All_Visits, All_Volunteers)|>
  mutate(bob_vis=Seasonal_Bobolinks/Total_Visits)

ggplot(All_Joined2, aes(x=reorder(Property, -bob_vis), bob_vis))+geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.x = element_text(vjust = 0.35)) +
  theme(axis.text.x = element_text(hjust = 1)) + ylab("average bobolinks seen per visit")
#interesting, jewell hill pops up to the top here and appleton down. what do we think this means? 
## I dont think it means there are more bobolinks at jewell hill, because each 'visit' 
## to appleton is only looking at a portion of the property.  might have to do on a per field/circuit 
## basis and then add up! 




  

