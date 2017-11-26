

# ---------------------------------------------------------------------------------------------- #
# Pulling in data and setup

library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
landfill <- read.csv("landfill.csv")
power <- read.csv("power.csv")
water <- read.csv("water.csv")
pop2017 <- read.csv("pop2017.csv")

# ---------------------------------------------------------------------------------------------- #
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

# adding popluation to the landfills by state
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

# ------------------- #
# Summary Stats

# Number of Landfills Per State
num.landfills <- landfill_by_state %>% 
  select(State, num.landfills)
# Average waste in landfills per state, total waste in landfills per state
waste.landfills <- landfill %>% 
  select(State, Waste.in.Place..tons.) %>% 
  group_by(State) %>% 
  summarise(average.waste = mean(as.numeric(Waste.in.Place..tons.), na.rm = TRUE), 
            total.waste = sum(as.numeric(Waste.in.Place..tons., na.rm = TRUE)))
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
  mutate(population.waste = (total.waste / X2017.Population) * 100) %>% 
  arrange(desc(population.waste))



# ------------------- #
# Tests

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


# ------------------- #
# Graphs

# Total waste by population graph
waste.by.pop <- choroplthFunc(population.waste, population.waste$population.waste, population.waste$State, 
              population.waste$population.waste, "Total Waste (in tons) Per Capita", c("yellow", "red"))
# Percent of landfills with an lfg system in the state
percent.with.lfg <- choroplthFunc(num.LFG, num.LFG$percent.lfg, num.LFG$State, num.LFG$percent.lfg, 
              "Percentage of Landfills With LFG Collection", c("red","yellow"))
# Lanfills by their amount of waste and if they have an LFG system
bubble.by.waste.lfg <- bubbleGraphFunc(landfill, landfill$Longitude, landfill$Latitude, as.numeric(landfill$Waste.in.Place..tons.),
                landfill$LFG.Collection.System.In.Place.,
                paste("State: ", landfill$State, "<br> Name: ", landfill$Landfill.Name,
                      "<br> Waste (tons): ", landfill$Waste.in.Place..tons.), 
                "Landfills by Waste and if there is a LFG System")
# Year Facility Opened and The Number of Plants
# NEED TO FILTER ON STATE HERE AND CREATE AN INPUT THING
year.lfg <- landfill %>% 
  select(Year.Landfill.Opened, LFG.Collection.System.In.Place.) %>% 
  group_by(Year.Landfill.Opened) %>% 
  summarise(total = n())
year.withlfg <- landfill %>% 
  select(Year.Landfill.Opened, LFG.Collection.System.In.Place.) %>% 
  filter(LFG.Collection.System.In.Place. == "Yes") %>% 
  group_by(Year.Landfill.Opened) %>% 
  summarise(countWith = n())
year.withoutlfg <- landfill %>% 
  select(Year.Landfill.Opened, LFG.Collection.System.In.Place.) %>% 
  filter(LFG.Collection.System.In.Place. == "No") %>% 
  group_by(Year.Landfill.Opened) %>% 
  summarise(countWithout = n())
lfg.by.year.half <- left_join(year.lfg, year.withlfg, by="Year.Landfill.Opened") 
lfg.by.year <- left_join(lfg.by.year.half, year.withoutlfg, by="Year.Landfill.Opened")
num.plants.by.year.lfg <- plot_ly(lfg.by.year, x = ~Year.Landfill.Opened, y = ~countWithout, type = 'bar', name = 'Without LFG') %>%
  add_trace(y = ~countWith, name = 'With LFG') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')



# ----------------------------------------------------------------------------------------------#
# Power Data
i <- 1
StateFull = c()
while (i < 52){
  StateFull <- c(StateFull, state.name[grep(power$State.abbreviation[i], state.abb)])
  i <- i + 1
}
power <- power %>% 
  select(-State.ozone.season.net.generation..MWh., -State.ozone.season.NOx.emissions..tons.)  %>% 
  filter(State.abbreviation != "DC")

StateFull <- as.data.frame(StateFull)
power <- bind_cols(power, StateFull)

# ------------------- #
# Summary Stats

# HEat input is how many mmbtus that have to go in to generate that output
# Convert these into jewels

# Annual Generation over their capacity
amount.used <- power %>% 
  select(State.abbreviation, State.nameplate.capacity..MW., State.annual.net.generation..MWh.) %>% 
  mutate(amount.power.used = as.numeric(State.annual.net.generation..MWh.) / as.numeric(State.nameplate.capacity..MW.))
# Heat input over their anual output
input.to.output <- power %>% 
  select()

# ------------------- #
# Tests



# ----------------------------------------------------------------------------------------------#
# Water Data 

water <- water %>% 
  select(-Groundwater.Fresh, -Groundwater.Saline, -Surfacewater.Fresh, -Surfacewater.Saline, -Irrigation, 
         -LiveStock, -Aquaculture, -Mining.Fresh, -Mining.Saline, -ThermoelectricPower.Fresh, -ThermoelectricPower.Saline, 
         -PublicWithdrawals.Groundwater, -PublicWithdrawals.Surfacewater, -Irrigation.Groundwater, 
         -Irigation.Surfacewater, -Livestock.Groundwater, -Livestock.Surfacewater, -Aquaculture.Groundwater, 
         -Aquaculture.Surfacewater, -Mining.Groundwater, -Mining.Surfacewater, -Thermoelectric.Groundwater, 
         -Thermoelectric.Surfacewater)





# ----------------------------------------------------------------------------------------------#
# Common functions 
choroplthFunc <- function(data, zData, stateAbb, colorData, titleText, colorPick){
  l <- list(color = toRGB("white"), width = 2)
  # specify some map projection/options
  g <- list(scope = 'usa', projection = list(type = 'albers usa'),showlakes = TRUE,lakecolor = toRGB('white'))
  plot_geo(data, locationmode = 'USA-states') %>%
    add_trace( z = zData, locations = stateAbb, color = colorData, colors = colorPick, 
               marker = list(line = l))  %>%
    layout(title = titleText, geo = g)
}

bubbleGraphFunc <- function(data, lon, lat, sizeData, colorData, hoverText, titleText) {
  g <- list(
    scope = 'usa', projection = list(type = 'albers usa'), showland = TRUE, landcolor = toRGB("gray85"), 
    subunitcolor = toRGB("white"), countrycolor = toRGB("white")
  )
  plot_geo(landfill, locationmode = 'USA-states', sizes = c(1, 50)) %>%
    add_markers(
      x = lon, y = lat, size = sizeData, color = colorData, hoverinfo = "text",
      text = hoverText
    ) %>%
    layout(title = titleText, geo = g)
}
stackChartFunc <- function(data, xData, YData, YDataName, YData2, YDataName2){
  plot_ly(data, x = xData, y = YData, type = 'bar', name = YDataName) %>%
    add_trace(y = YData2, name = YDataName2) %>%
    layout(yaxis = list(title = 'Count'), barmode = 'stack')
}
