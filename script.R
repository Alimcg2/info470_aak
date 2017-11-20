
library(dplyr)

landfill <- read.csv("landfill.csv")
landfill <- landfill %>% 
  select("State", "Longitude", "Latitude", "Ownership.Type", "Year.Landfill.Opened", 
         "Landfill.Closure.Year", "Waste.in.Place..tons.", "LFG.Collection.System.In.Place.", 
         "LFG.Collected..mmscfd.", "LFG.Energy.Project.Type", "Project.Type.Category", "MW.Capacity", 
         "Current.Year.Emission.Reductions..MMTCO2e.yr....Direct")
power <- read.csv("power.csv")
water <- read.csv("water.csv")

print(water$State)