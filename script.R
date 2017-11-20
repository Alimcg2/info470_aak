

# ----------------------------------------------------------------------------------------------#
# Pulling in data and setup

library(dplyr)
landfill <- read.csv("landfill.csv")
power <- read.csv("power.csv")
water <- read.csv("water.csv")


# ----------------------------------------------------------------------------------------------#
# Landfill Data 

landfill <- landfill %>% 
  select("State", "Longitude", "Latitude", "Ownership.Type", "Year.Landfill.Opened", 
         "Landfill.Closure.Year", "Waste.in.Place..tons.", "LFG.Collection.System.In.Place.", 
         "LFG.Collected..mmscfd.", "LFG.Energy.Project.Type", "Project.Type.Category", "MW.Capacity", 
         "Current.Year.Emission.Reductions..MMTCO2e.yr....Direct")

landfill_by_state <- landfill %>% select("State", "Waste.in.Place..tons.", "LFG.Collection.System.In.Place.", 
                                          "LFG.Collected..mmscfd.", "MW.Capacity", "Current.Year.Emission.Reductions..MMTCO2e.yr....Direct") %>% 
  group_by(State) %>% summarise(total.waste = sum(as.numeric(Waste.in.Place..tons., na.rm = TRUE)), 
                                total.with.lfg = sum(LFG.Collection.System.In.Place. == "Yes", na.rm = TRUE), 
                                total.lfg.collected = sum(LFG.Collected..mmscfd., na.rm = TRUE),
                                total.capacity = sum(MW.Capacity, na.rm = TRUE),
                                total.reduction = sum(Current.Year.Emission.Reductions..MMTCO2e.yr....Direct, na.rm = TRUE)) 

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

