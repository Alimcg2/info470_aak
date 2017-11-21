

# ----------------------------------------------------------------------------------------------#
# Pulling in data and setup

library(dplyr)
library(ggplot2)
library(plotly)
landfill <- read.csv("landfill.csv")
power <- read.csv("power.csv")
water <- read.csv("water.csv")
pop2017 <- read.csv("pop2017.csv")


# ----------------------------------------------------------------------------------------------#
# Landfill Data 

landfill <- landfill %>% 
  select(State, Landfill.Name, Longitude, Latitude, Ownership.Type, Year.Landfill.Opened, 
         Landfill.Closure.Year, Waste.in.Place..tons., LFG.Collection.System.In.Place., 
         LFG.Collected..mmscfd., LFG.Energy.Project.Type, Project.Type.Category, MW.Capacity, 
         Current.Year.Emission.Reductions..MMTCO2e.yr....Direct)

landfill_by_state <- landfill %>% 
  select(State, Landfill.Name, Waste.in.Place..tons., LFG.Collection.System.In.Place., 
         LFG.Collected..mmscfd., MW.Capacity, Current.Year.Emission.Reductions..MMTCO2e.yr....Direct) %>% 
  group_by(State) %>% 
  summarise(num.landfills = n(),
            total.waste = sum(as.numeric(Waste.in.Place..tons., na.rm = TRUE)), 
            total.with.lfg = sum(LFG.Collection.System.In.Place. == "Yes", na.rm = TRUE), 
            total.lfg.collected = sum(LFG.Collected..mmscfd., na.rm = TRUE),
            total.capacity = sum(MW.Capacity, na.rm = TRUE),
            total.reduction = sum(Current.Year.Emission.Reductions..MMTCO2e.yr....Direct, na.rm = TRUE)) 


# Summary Stats

# Number of Landfills Per State
num.landfills <- landfill_by_state %>% 
  select(State, num.landfills)
# Average waste in landfills per state, total waste in landfills per state
waste.landfills <- landfill %>% 
  select(State, Waste.in.Place..tons.) %>% 
  group_by(State) %>% 
  summarise(average.waste = mean(as.numeric(Waste.in.Place..tons.), na.rm = TRUE), total.waste = sum(as.numeric(Waste.in.Place..tons., na.rm = TRUE)))
# Number of Landfills and number of LFG programs, percent of landfills with LFG programs
num.LFG <- landfill_by_state %>% 
  select(State, num.landfills, total.with.lfg) %>% 
  mutate(percent.lfg = (total.with.lfg / num.landfills) * 100)
# State's percentage of waste over total waste
percent.waste <- landfill_by_state %>% 
  select(State, total.waste) %>% 
  mutate(percent.waste.of.all = (total.waste / sum(total.waste)) * 100)

# ----------------------------------------------------------------------------------------------#
# Power Data

power <- power %>% 
  select(-State.ozone.season.net.generation..MWh., -State.ozone.season.NOx.emissions..tons.)

# ----------------------------------------------------------------------------------------------#
# Water Data 

water <- water %>% 
  select(-Groundwater.Fresh, -Groundwater.Saline, -Surfacewater.Fresh, -Surfacewater.Saline, -Irrigation, -LiveStock, -Aquaculture, 
         -Mining.Fresh, -Mining.Saline, -ThermoelectricPower.Fresh, -ThermoelectricPower.Saline, -PublicWithdrawals.Groundwater, 
         -PublicWithdrawals.Surfacewater, -Irrigation.Groundwater, -Irigation.Surfacewater, -Livestock.Groundwater, -Livestock.Surfacewater, 
         -Aquaculture.Groundwater, -Aquaculture.Surfacewater, -Mining.Groundwater, -Mining.Surfacewater, -Thermoelectric.Groundwater,
         -Thermoelectric.Surfacewater)

