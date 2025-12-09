

## This script calculates the AOP of a culvert based on data taken from Adam's
## Microsoft Forms collector, which has been downloaded as a CSV, and calculate
## the AOP score (Aquatic Organism Passage) based on NAACC guidelines.  
## It does the same thing as the GIS AOP Calculator, but just from different 
## initial source data.

## This is just the math. It does not include any discrepancy between structure
## type (bridges receive low substrate coverage scores because it's a bridge).
## Go to line 217 and see the code hidden with ## for how to sort the data by 
## property and structure type


#############################
# Lines 21 through 24 are where you read in the data as a CSV. You will need to
# input where the file is into the setwd line, and then put the name of the
# file into the line directly after. All of the file names must match exactly.
#############################

library("tidyverse", "dplyr", "ggplot")
setwd("C:/Users/asussbauer/OneDrive - TTOR/Culvert Project/Data Collector")
Crossings <- read_csv("FormsData_Test1.csv", na = c("N/A", ""))
colnames(Crossings) <- gsub(" ", "_", colnames(Crossings))

Crossings <- Crossings |>
  filter(
   !Id <= 0 
  ) |>
  filter(
    Property == "Bryant Homestead"
  ) |>
  rename(
    Inlet_Width = "Inlet_Width_(m)",
    Inlet_Height = "Inlet_Height_(m)",
    Inlet_Stream_Width = "Inlet_Stream_Width_(m)",
    Outlet_Width = "Outlet_Width_(m)",
    Outlet_Height = "Outlet_Height_(m)",
    Outlet_Stream_Width = "Outlet_Stream_Width_(m)",
    Field_Outlet_Drop = "Outlet_Drop_(m)",
    Structure_Length = "Structure_Length_(m)",
  ) |>
  select(
    -Id, -Completion_time, -Name, -Email, -Start_time, 
    -Collection_Date
  ) |>
  mutate(
    Outlet_Stream_Width = Outlet_Stream_Width * 3.28084,
    Field_Outlet_Drop = Field_Outlet_Drop * 3.28084,
    Outlet_Height = Outlet_Height * 3.28084,
    Outlet_Width = Outlet_Width * 3.28084,
    Inlet_Stream_Width = Inlet_Stream_Width * 3.28084,
    Inlet_Height = Inlet_Height * 3.28084,
    Inlet_Width = Inlet_Width * 3.28084,
    Structure_Length = Structure_Length * 3.28084
  ) |>
  separate_wider_delim(
    GPS_Location, delim = ",", names = c("Lat","Long")
  ) |>
  mutate(
    Substrate_Type = case_when(
      Substrate_Type == "No Substrate" ~ 0,
      Substrate_Type == "Not Appropriate" ~ 0.25,
      Substrate_Type == "Contrasting" ~ 0.75,
      Substrate_Type == "Comparable" ~ 1
    ),
    Substrate_Coverage = case_when(
      Substrate_Coverage == "No coverage" ~ 0,
      Substrate_Coverage == "25% covered" ~ 0.3,
      Substrate_Coverage == "50% covered" ~ 0.5,
      Substrate_Coverage == "75% covered" ~ 0.7,
      Substrate_Coverage == "100% covered" ~ 1
    ),
    Physical_Barriers = case_when(
      Physical_Barriers == "No barrier" ~ 1,
      Physical_Barriers == "Minor" ~ 0.8,
      Physical_Barriers == "Moderate" ~ 0.5,
      Physical_Barriers == "Severe" ~ 0
    ),
    Internal_Structures = case_when(
      Internal_Structures == "Baffles/Weirs" ~ 0,
      Internal_Structures == "Supports" ~ 0.8,
      Internal_Structures == "None/Other" ~ 1
    ),
    Water_Depth = case_when(
      Water_Depth == "Significantly Deeper" ~ 0.5,
      Water_Depth == "Significantly Shallower" ~ 0,
      Water_Depth == "Comparable Depth" ~ 1,
      Water_Depth == "Dry Stream and Structure" ~ 1
    ),
    Inlet_Grade = case_when(
      Inlet_Grade == "Inlet Drop" ~ 0,
      Inlet_Grade == "Perched Inlet" ~ 0,
      Inlet_Grade == "Collapsed or Submerged inlet" ~ 1,
      Inlet_Grade == "At Stream Grade" ~ 1
    ),
    Water_Velocity = case_when(
      Water_Velocity == "Much faster" ~ 0,
      Water_Velocity == "Much slower" ~ 0.5,
      Water_Velocity == "Similar speed" ~ 1,
      Water_Velocity == "Dry stream and structure" ~ 1
    ),
    Scour_Pool = case_when(
      Scour_Pool == "Large" ~ 0,
      Scour_Pool == "Small" ~ 0.8,
      Scour_Pool == "No Scour Pool" ~ 1
    ),
    Outlet_Armoring = case_when(
      Outlet_Armoring == "No armoring" ~ 1,
      Outlet_Armoring == "Not extensive" ~ 0.5,
      Outlet_Armoring == "Extensive" ~ 0
    )
  ) |>
  
  ## Below is calculations for Openness. Openness = smaller value of cross-
  ## sectional area divided by structure length.
  
  mutate(
    xSec_Inlet = case_when(
      Structure_Shape == "Circle" ~ ((Inlet_Width/2)^2 * pi),
      Structure_Shape == "Rectangle/Square" ~ (Inlet_Width * Inlet_Height),
      Structure_Shape == "Bridge/undefinable" ~ (Inlet_Width * Inlet_Height)
    ),
    xSec_Outlet = case_when(
      Structure_Shape == "Circle" ~ ((Outlet_Width/2)^2 * pi),
      Structure_Shape == "Rectangle/Square" ~ (Outlet_Width * Outlet_Height),
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
      Inlet_Width <= Inlet_Stream_Width*0.5 ~ 0,
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

# 1. the code below sorts by a given property. Replace Bryant Homestead with
# the name of the property you want (must match the data exactly)

#Crossings <- Crossings |>
#  filter(
#    Property == "Bryant Homestead"
#  )

# 2. the code below sorts by the structure type. Replace Culvert with the 
# type that you want to use.

#Crossings <- Crossings |>
#  filter(
#    Structure_Type == "Culvert"
#  )

# 3. the code below sorts by the waterway type. Replace Natural Stream with
# the type that you want to use.

#Crossings <- Crossings |>
#  filter(
#    Waterway_Type == "Natural Stream"
#  )



















