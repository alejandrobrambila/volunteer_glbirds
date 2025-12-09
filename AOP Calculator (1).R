
## This script is meant to take stream crossing data from Adam's Data Collector,
## which has been downloaded as a CSV, and calculate the AOP score (Aquatic
## Organism Passage) based on NAACC guidelines. 

## This is just the math. It does not include any discrepancy between structure
## type (bridges receive low substrate coverage scores because it's a bridge).
## Go to line 184 and see the code hidden with ## for how to sort the data by 
## property and structure type

library("tidyverse", "dplyr", "ggplot", "sf")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Culvert Project/Data Collector")
Crossings <- read_csv("Culverts_JustBryant.csv", na = c("N/A", ""))
Properties <- unzip("TTOR_Reservations.zip", junkpaths = FALSE)

Crossings <- Crossings |>
  filter()
    Property == "Bryant Homestead"
  ) |>
  select(
    Lat,
    Long,
    Property,
    Outlet_Armoring,
    Water_Velocity,
    Substrate_Type,
    Scour_Pool,
    Water_Depth,
    Outlet_Stream_Width,
    Outlet_Drop,
    Outlet_Height,
    Outlet_Width,
    Inlet_Grade,
    Inlet_Stream_Width,
    Inlet_Height,
    Inlet_Width,
    Internal_Structures,
    Substrate_Coverage,
    Physical_Barriers,
    Structure_Length,
    Crossing_Type,
    Waterway_Type,
    Structure_Shape,
    Structure_Type,
    Structure_ID
  ) |>
  rename(
    Field_Outlet_Drop = 'Outlet_Drop'
  ) |>
  
## This next part is converting everything from meters to feet
  
  mutate(
    Outlet_Stream_Width = Outlet_Stream_Width * 3.28084,
    Field_Outlet_Drop = Field_Outlet_Drop * 3.28084,
    Outlet_Height = Outlet_Height * 3.28084,
    Outlet_Width = Outlet_Width * 3.28084,
    Inlet_Stream_Width = Inlet_Stream_Width * 3.28084,
    Inlet_Height = Inlet_Height * 3.28084,
    Inlet_Width = Inlet_Width * 3.28084
  ) |>
  relocate(
    Structure_ID, .after = Property,
  ) |>
  relocate(
    Structure_Type, .after = Structure_ID
  ) |>
  relocate(
    Structure_Shape, .after = Structure_Type
  ) |>
  relocate(
    Crossing_Type, .after = Structure_Shape
  ) |>
  relocate(
    Waterway_Type, .after = Crossing_Type
  ) |>
  
## Below is calculations for Openness. Openness = smaller value of cross-
## sectional area divided by structure length.
  
  mutate(
    xSec_Inlet = case_when(
      Structure_Shape == "Circle" ~ ((Inlet_Width/2)^2 * pi),
      Structure_Shape == "Rectangle" ~ (Inlet_Width * Inlet_Height),
      Structure_Shape == "Ellipse" ~ ((Inlet_Width/2)*(Inlet_Height/2)*pi),
      Structure_Shape == "Bridge/undefinable" ~ (Inlet_Width * Inlet_Height)
    ),
    xSec_Outlet = case_when(
      Structure_Shape == "Circle" ~ ((Outlet_Width/2)^2 * pi),
      Structure_Shape == "Rectangle" ~ (Outlet_Width * Outlet_Height),
      Structure_Shape == "Ellipse" ~ ((Outlet_Width/2)*(Outlet_Height/2)*pi),
      Structure_Shape == "Bridge/undefinable" ~ (Outlet_Width * Outlet_Height)
    ),
    Inlet_Openness = xSec_Inlet / Structure_Length,
    Outlet_Openness = xSec_Outlet / Structure_Length,
    Meas_Openness = pmin(Inlet_Openness, Outlet_Openness),
    Openness = ((1)*(1-exp(-(15)*Meas_Openness*(1-0.62)))^(1/(1-0.62)))
    ) |>
  relocate(
    Openness, .after = Waterway_Type
  ) |>
  select(
    -xSec_Inlet, -xSec_Outlet, -Inlet_Openness, -Outlet_Openness, -Meas_Openness
  ) |>
  
## Below is the calculation for the Height component score
  
  mutate(
    max_Height = pmin(Inlet_Height, Outlet_Height),
    calc_Height = ((1.1)*(max_Height)^2 / ((2.2^2)+(max_Height^2))),
    Height = case_when(
      calc_Height > 1 ~ 1,
      calc_Height < 1 ~ calc_Height
    )
  ) |>
  relocate(
    Height, .after = Openness
  ) |>
  select(
    -max_Height, -calc_Height
  ) |>
  
# Below is the calculation for Outlet Drop component score

  mutate(
    Outlet_Drop = (1-(((1.029412)*(Field_Outlet_Drop)^2) / (0.51449579^2 + 
                                    Field_Outlet_Drop^2)))
  ) |>
  relocate(
    Outlet_Drop, .after = Height
  ) |>
  select(
    -Field_Outlet_Drop
  ) |>
  
## Below is calculating Constriction (width of structure compared to width
## of stream at the inlet)
  
  mutate(
    Constriction = case_when(
      Inlet_Width < Inlet_Stream_Width*0.5 ~ 0,
      Inlet_Stream_Width*0.5 < Inlet_Width & Inlet_Width < Inlet_Stream_Width*0.75 ~ 0.5,
      Inlet_Stream_Width*0.75 < Inlet_Width & Inlet_Width < Inlet_Stream_Width ~ 0.75,
      Inlet_Width == Inlet_Stream_Width ~ 0.9,
      Inlet_Width > Inlet_Stream_Width ~ 1
    )
  ) |>
  
  
## Next is multiplying the component scores by their NAACC weight
  
  mutate(
    Outlet_Drop = Outlet_Drop * 0.161,
    Physical_Barriers = Physical_Barriers * 0.135,
    Constriction = Constriction*0.090,
    Inlet_Grade = Inlet_Grade*0.088,
    Water_Depth = Water_Depth*0.082,
    Water_Velocity = Water_Velocity*0.080,
    Scour_Pool = Scour_Pool*0.071,
    Substrate_Type = Substrate_Type*0.070,
    Substrate_Coverage = Substrate_Coverage*0.057,
    Openness = Openness*0.052,
    Height = Height*0.045,
    Outlet_Armoring = Outlet_Armoring*0.037,
    Internal_Structures = Internal_Structures*0.032,
    Composite_Score = (Outlet_Drop + Physical_Barriers + Constriction + 
                          Inlet_Grade + Water_Depth + Water_Velocity + Scour_Pool +
                          Substrate_Type + Substrate_Coverage + Openness + Height +
                          Outlet_Armoring + Internal_Structures)
  ) |>
  relocate(
    Composite_Score, .after = Structure_ID
  ) |>
  select(
    -Structure_Length, -Inlet_Height, -Inlet_Width, -Outlet_Width, -Outlet_Height,
    -Inlet_Stream_Width, -Outlet_Stream_Width
  ) |>
  mutate(
    AOP_Score = pmin(Composite_Score,(Outlet_Drop/0.161))
  ) |>
  relocate(
    AOP_Score, .before = Composite_Score 
  ) 
  

## HERE IS WHERE YOU CAN SORT BY PROPERTY, WATERWAY TYPE,
## AND STRUCTURE TYPE. To activate a given section, remove the hashtags ##
## from the left-hand-side of every line, then run the entire script (highlight
## everything and click run at the top of the screen). To deactivate a section,
## simply add at least one hashtag to the start of every line in the section.

## The filter() tool is how you sort by certain metrics. Write your own code
## if there's more that you're interested in!


# 1. the code below sorts by the structure type. Replace Culvert with the 
# type that you want to use.

#Crossings <- Crossings |>
#  filter(
#    Structure_Type == "Culvert"
#  )
 
# 2. the code below sorts by the waterway type. Replace Natural Stream with
# the type that you want to use.
 
#Crossings <- Crossings |>
#  filter(
#    Waterway_Type == "Natural Stream"
#  )

Properties <- read_sf("./Reservations.shp")
Properties <- Properties |>
  filter(
    PROPERTY == "Bryant Homestead"
  )

ggplot() +
  geom_sf(data = Properties, aes(fill = "#99FF33", 
                        color = "black")) +
  geom_point(data = Crossings, aes(x = Long, y = Lat, 
                      scale_fill_gradient(low = "blue", high = "red")))



  theme_void() +
  geom_point(data = Crossings, aes(x = Lat, y = Long))










