
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

landfill_by_state <- landfill %>% select("State", "Waste.in.Place..tons.", "LFG.Collection.System.In.Place.", 
                                          "LFG.Collected..mmscfd.", "MW.Capacity", "Current.Year.Emission.Reductions..MMTCO2e.yr....Direct") %>% 
  group_by(State) %>% summarise(total.waste = sum(as.numeric(Waste.in.Place..tons., na.rm = TRUE)), 
                                total.with.lfg = sum(LFG.Collection.System.In.Place. == "Yes", na.rm = TRUE), 
                                total.lfg.collected = sum(LFG.Collected..mmscfd., na.rm = TRUE),
                                total.capacity = sum(MW.Capacity, na.rm = TRUE),
                                total.reduction = sum(Current.Year.Emission.Reductions..MMTCO2e.yr....Direct, na.rm = TRUE)) 
