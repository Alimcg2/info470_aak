

# ----------------------------------------------------------------------------------------------#
# Pulling in data and setup

library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
landfill <- read.csv("landfill.csv")
power <- read.csv("power.csv")
water <- read.csv("water.csv")
pop2017 <- read.csv("pop2017.csv")
# state abbr - state.abb[grep("New York", state.name)]

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

# adding popluation to the landfillsby state
i <- 1
StateAbbr = c()
while (i < 52){
  StateAbbr <- c(StateAbbr, state.abb[grep(pop2017$State[i], state.name)])
  i <- i + 1
}
StateAbbr <- as.data.frame(StateAbbr)
pop2017 <- bind_cols(pop2017, StateAbbr)
landfill_by_state <- left_join(landfill_by_state, pop2017, by = c("State" = "StateAbbr")) %>% 
  filter(State.y != "NA")

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
  mutate(percent.lfg = (total.with.lfg / num.landfills) * 100) %>% 
  arrange(desc(percent.lfg))
# State's percentage of waste over total waste
percent.waste <- landfill_by_state %>% 
  select(State, total.waste) %>% 
  mutate(percent.waste.of.all = (total.waste / sum(total.waste)) * 100) %>% 
  arrange(desc(percent.waste.of.all))
# State's waste over population
population.waste <- landfill_by_state %>% 
  select(State, X2017.Population, total.waste) %>% 
  mutate(population.waste = (total.waste / X2017.Population)) %>% 
  arrange(desc(population.waste))


# Tests
# landfills with x amount of trash do they have a lfg system?
# public vs private with lfg system?
# maybe a feasibility reason that people don't have it?
# year opened and the lfg system
# get data on the population density

# chi-squared independence on state and waste
states <- landfill_by_state$State
waste <- landfill_by_state$total.waste
chisq.test(waste, states)

# correlation between LFG collected and waste
LFG.collected <- landfill_by_state$total.lfg.collected
cor.test(LFG.collected, waste)

# chi-squared goodness of fit on waste based on state population and actual
expected <- landfill_by_state$X2017.Population / sum(landfill_by_state$X2017.Population, na.rm = TRUE)
observed <- landfill_by_state$total.waste
# LMAO THIS IS HELLA WRONG
chisq.test(observed, p = expected)


# ----------------------------------------------------------------------------------------------#
# Power Data

power <- power %>% 
  select(-State.ozone.season.net.generation..MWh., -State.ozone.season.NOx.emissions..tons.)


# Summary Stats

# HEat input is how many mmbtus that have to go in to generate that output
# Convert these into jewels

# Actual Generation over nameplate capacity
# OOPS NEED TO CONVERT MWh into MW HAHHAHAA
amount.used <- power %>% 
  select(State.abbreviation, State.nameplate.capacity..MW., State.annual.net.generation..MWh.) %>% 
  mutate(amount.power.used = as.numeric(State.annual.net.generation..MWh.) / as.numeric(State.nameplate.capacity..MW.))

# ----------------------------------------------------------------------------------------------#
# Water Data 

water <- water %>% 
  select(-Groundwater.Fresh, -Groundwater.Saline, -Surfacewater.Fresh, -Surfacewater.Saline, -Irrigation, -LiveStock, -Aquaculture, 
         -Mining.Fresh, -Mining.Saline, -ThermoelectricPower.Fresh, -ThermoelectricPower.Saline, -PublicWithdrawals.Groundwater, 
         -PublicWithdrawals.Surfacewater, -Irrigation.Groundwater, -Irigation.Surfacewater, -Livestock.Groundwater, -Livestock.Surfacewater, 
         -Aquaculture.Groundwater, -Aquaculture.Surfacewater, -Mining.Groundwater, -Mining.Surfacewater, -Thermoelectric.Groundwater,
         -Thermoelectric.Surfacewater)

