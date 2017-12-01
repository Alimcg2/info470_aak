

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
popdensity <- read.csv("popdensity.csv")
source("functions.R")
states <- as.data.frame(state.name, state.abb) %>% 
  mutate(stateabrev = state.abb)

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
            total.waste = sum(as.numeric(Waste.in.Place..tons.)),
            total.with.lfg = sum(LFG.Collection.System.In.Place. == "Yes", na.rm = TRUE), 
            total.lfg.collected = sum(LFG.Collected..mmscfd., na.rm = TRUE),
            total.capacity = sum(MW.Capacity, na.rm = TRUE),
            total.reduction = sum(Current.Year.Emission.Reductions..MMTCO2e.yr....Direct, na.rm = TRUE)) %>% 

#write.csv(landfill_by_state, file = "CABS.csv")
casum <- landfill %>% 
  filter(State == "CA") %>% 
  filter(Waste.in.Place..tons. != "NA") %>% 
  summarise(total = sum(as.numeric(Waste.in.Place..tons.)))
landfill_by_state <- read.csv("CABS.csv")

#landfill_by_state <- left_join(landfill_by_state, states, by = c("State" = "stateabrev")) 
#landfill_by_state <- left_join(landfill_by_state, pop2017, by = c("state.name" = "State")) 
#landfill_by_state <- landfill_by_state %>% 
#  filter(state.name != "N/A")
#landfill_by_state <-left_join(landfill_by_state, popdensity, by = c("state.name" = "State"))

# ----------------------------------------------------------------------------------------------#
# Water Data 



water <- water %>% 
  select(-Groundwater.Fresh, -Groundwater.Saline, -Surfacewater.Fresh, -Surfacewater.Saline, -Irrigation, 
         -LiveStock, -Aquaculture, -Mining.Fresh, -Mining.Saline, -ThermoelectricPower.Fresh, 
         -ThermoelectricPower.Saline, 
         -PublicWithdrawals.Groundwater, -PublicWithdrawals.Surfacewater, -Irrigation.Groundwater, 
         -Irigation.Surfacewater, -Livestock.Groundwater, -Livestock.Surfacewater, -Aquaculture.Groundwater, 
         -Aquaculture.Surfacewater, -Mining.Groundwater, -Mining.Surfacewater, -Thermoelectric.Groundwater, 
         -Thermoelectric.Surfacewater) %>% 
  mutate(SelfSuppliedIndustrialTotal = (as.numeric(water$SelfSuppliedIndustrial.Saline) + 
                                          as.numeric(water$SelfSuppliedIndustrial.Fresh))) %>% 
  filter(State != "District Of Columbia") %>% 
  mutate(State.abbr = )


# ----------------------------------------------------------------------------------------------#
# Power  Data 

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


# ----------------------------------------------------------------------------------------------#
# New Stuff

# Landfill Scores
total.waste.population <- landfill_by_state %>% 
  mutate(total.waste.pop = total.waste / X2017.Population) %>% 
  select(state.name, total.waste.pop) %>% 
  mutate(biggest = max(total.waste.pop)) %>% 
  mutate(waste.pop = 1 - (total.waste.pop / biggest))%>% 
  select(state.name, waste.pop)

total.waste.populationdensity <- landfill_by_state %>% 
  mutate(waste.density = total.waste / Density) %>% 
  select(state.name, waste.density) %>% 
  mutate(biggest = max(waste.density)) %>% 
  mutate(landfill.a.1 = 1 - (waste.density / biggest))%>% 
  select(state.name, waste.density)

lfg.collected.population <- landfill_by_state %>% 
  mutate(lfg.collected.pop = total.lfg.collected / X2017.Population) %>% 
  select(state.name, lfg.collected.pop) %>% 
  mutate(biggest = max(lfg.collected.pop)) %>% 
  mutate(lfg.collected = lfg.collected.pop / biggest) %>% 
  select(state.name, lfg.collected)

# Power Scores
noncombust.total <- power %>% 
  mutate(total.combust = as.numeric(gsub(",","",power$State.annual.total.combustion.net.generation..MWh.))
         + as.numeric(gsub(",","",power$State.annual.total.noncombustion.net.generation..MWh.))) %>% 
  mutate(noncombust = as.numeric(gsub(",","",power$State.annual.total.noncombustion.net.generation..MWh.)) / total.combust) %>% 
  select(StateFull, noncombust) %>% 
  mutate(biggest = max(noncombust)) %>% 
  mutate(combustion = noncombust / biggest) %>% 
  select(StateFull, combustion)

renewables.total <- power %>% 
  mutate(total.renewables = as.numeric(gsub(",","",power$State.annual.total.renewables.net.generation..MWh.))
         + as.numeric(gsub(",","",power$State.annual.total.nonrenewables.net.generation..MWh.))) %>% 
  mutate(renewables = as.numeric(gsub(",","",power$State.annual.total.renewables.net.generation..MWh.)) / total.renewables) %>% 
  select(StateFull, renewables) %>% 
  mutate(biggest = max(renewables)) %>% 
  mutate(renews = renewables / biggest) %>% 
  select(StateFull, renews)

total.emissions <- power %>% 
  select(StateFull, State.annual.NOx.emissions..tons., 
         State.annual.CH4.emissions..lbs., 
         State.annual.CO2.emissions..tons., 
         State.annual.N2O.emissions..lbs., 
         State.annual.SO2.emissions..tons., 
         State.annual.net.generation..MWh.) %>% 
  mutate(total = (State.annual.N2O.emissions..lbs. * 0.0005) +
           State.annual.CO2.emissions..tons. +
           State.annual.NOx.emissions..tons. +
           State.annual.SO2.emissions..tons. +
           (State.annual.CH4.emissions..lbs.* 0.0005)) %>% 
  mutate(emissions.total.gen = total / State.annual.net.generation..MWh.)
  
emissions.generation <- total.emissions %>% 
  mutate(biggest = max(emissions.total.gen)) %>% 
  mutate(emissions = 1 - (emissions.total.gen / biggest)) %>% 
  select(StateFull, emissions)

# Water Scores
water.withdrawls.by.pop <- slice(water, 1:50) %>% 
  transform( scores = as.numeric(Total) / as.numeric(Population.Total)) %>% 
  select(State, scores)

water_withdrawals_given_pop <- water.withdrawls.by.pop %>% 
  mutate(biggest = max(scores)) %>% 
  mutate(withdrawals = 1 - (scores / biggest)) %>% 
  select(State, withdrawals)

all.data <- left_join(total.waste.population, lfg.collected.population, by="state.name")
all.data <- left_join(all.data, total.waste.populationdensity, by = "state.name")
all.data <- left_join(all.data, noncombust.total, by = c("state.name" = "StateFull"))
all.data <- left_join(all.data,renewables.total, by = c("state.name" = "StateFull"))
all.data <- left_join(all.data,emissions.generation, by = c("state.name" = "StateFull"))
all.data <- left_join(all.data,water_withdrawals_given_pop, by = c("state.name" = "State"))
locations <- state.abb

# population 
all.data.pop <- all.data %>% 
  filter(state.name != "District of Columbia") %>% 
  mutate(totalScore = (waste.pop * 0.16) +
           (lfg.collected * 0.13) +
           (emissions * 0.25) + 
           (renews * 0.12) +
           (combustion * 0.14) +
           (withdrawals * 0.2)) %>% 
  arrange(state.name) %>% 
  mutate(loc = locations)
choroplthFunc(all.data.pop, all.data.pop$totalScore, all.data.pop$loc, all.data.pop$totalScore, 
              "titleText", c("red", "yellow"))

# population density
all.data.density <- all.data %>% 
  filter(state.name != "District of Columbia") %>% 
  mutate(totalScore = (waste.density * 0.16) +
           (lfg.collected * 0.13) +
           (emissions * 0.25) + 
           (renews * 0.12) +
           (combustion * 0.14) +
           (withdrawals * 0.2)) %>% 
  arrange(state.name) %>% 
  mutate(loc = locations)
choroplthFunc(all.data.density, all.data.density$totalScore, all.data.density$loc,
              all.data.density$totalScore, "titleText", c("red", "yellow"))

# summary stats

emissions.sum.stats <- summarise(total.emissions, variable = "total emissions by population", mean = mean(emissions.total.gen),
            median = median(emissions.total.gen))
water.sum.stats <- summarise(water.withdrawls.by.pop, variable = "total water withdrawls by population", mean = mean(scores),
                       median = median(scores))
# total waste
# % renewable
# total LFG collected
# % non-combustible
sum.stats <- rbind(emissions.sum.stats, water.sum.stats)


# ----------------------------------------------------------------------------------------------#
# Old Stuff

# -------------- #
# Common functions 

# ------------------- #
# Summary Stats Landfill

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
# Tests Landfill
# Chi-square test for independence - year opened versus LFG
lfg_and_year_data <- landfill %>% select(Year.Landfill.Opened, LFG.Collection.System.In.Place.)
lfg_and_year_data <- na.omit(lfg_and_year_data)

trash_year_and_lfg_chi_square <- chisq.test(lfg_and_year_data$Year.Landfill.Opened, 
                                            lfg_and_year_data$LFG.Collection.System.In.Place.)

# Chi-square test for independence - private/public versus LFG
lfg_and_ownership_data <- landfill %>% select(Ownership.Type, LFG.Collection.System.In.Place.)
lfg_and_ownership_data <- na.omit(lfg_and_ownership_data)

trash_ownership_and_lfg_chi_square <- chisq.test(lfg_and_ownership_data$Ownership.Type, 
                                                 lfg_and_ownership_data$LFG.Collection.System.In.Place.)
 
# Chi-square test for independence - state versus lfg  
state_and_lfg_data <- landfill %>% select(State, LFG.Collection.System.In.Place.)
state_and_lfg_data <- na.omit(state_and_lfg_data)

trash_state_and_lfg_chi_square <- chisq.test(state_and_lfg_data$State, 
                                             state_and_lfg_data$LFG.Collection.System.In.Place.)
# ------------------- #
# Graphs Landfill
# Total waste by population graph
waste.by.pop <- choroplthFunc(population.waste, population.waste$population.waste, population.waste$State, 
              population.waste$population.waste, "Total Waste (in tons) Per Capita", 
              c("forestgreen", "darkmagenta"))
# Percent of landfills with an lfg system in the state
percent.with.lfg <- choroplthFunc(num.LFG, num.LFG$percent.lfg, num.LFG$State, num.LFG$percent.lfg, 
              "Percentage of Landfills With LFG Collection", c("darkmagenta", "forestgreen"))
# Lanfills by their amount of waste and if they have an LFG system
bubble.by.waste.lfg <- bubbleGraphFunc(landfill, landfill$Longitude, landfill$Latitude, 
                                       as.numeric(landfill$Waste.in.Place..tons.),
                landfill$LFG.Collection.System.In.Place.,
                paste("State: ", landfill$State, "<br> Name: ", landfill$Landfill.Name,
                      "<br> Waste (tons): ", landfill$Waste.in.Place..tons.), 
                "Landfills by Waste and if there is a LFG System")
# Year Facility Opened and The Number of Plants
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
num.plants.by.year.lfg <- plot_ly(lfg.by.year, x = ~Year.Landfill.Opened, y = ~countWithout, 
                                  type = 'bar', name = 'Without LFG', marker = list(color = "Purple")) %>%
  add_trace(y = ~countWith, name = 'With LFG',  marker = list(color = "Green")) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
# Number of plants by year and if they are public or private
year.pub <- landfill %>% 
  select(Year.Landfill.Opened, Ownership.Type) %>% 
  filter(Ownership.Type == "Public") %>% 
  group_by(Year.Landfill.Opened) %>% 
  summarise(countPublic = n())
year.priv <- landfill %>% 
  select(Year.Landfill.Opened, Ownership.Type) %>% 
  filter(Ownership.Type == "Private") %>% 
  group_by(Year.Landfill.Opened) %>% 
  summarise(countPrivate = n())
ownership.year <- left_join(year.pub, year.priv, by="Year.Landfill.Opened")
num.plants.by.year.owner <- plot_ly(ownership.year, x = ~Year.Landfill.Opened, y = ~countPublic, type = 'bar', 
                                    name = 'Public', marker = list(color = "Green")) %>%
  add_trace(y = ~countPrivate, name = 'Private', marker = list(color = "Purple")) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')

# ------------------- #
# Summary Stats Power

# HEat input is how many mmbtus that have to go in to generate that output
# Convert these into jewels

# Annual Generation over their capacity
amount.used <- power %>% 
  select(State.abbreviation, State.nameplate.capacity..MW., State.annual.net.generation..MWh.) %>% 
  mutate(amount.power.used = as.numeric(State.annual.net.generation..MWh.) 
         / as.numeric(State.nameplate.capacity..MW.))
# Heat input over their anual output
input.to.output <- power %>% 
  select(State.abbreviation, State.total.annual.heat.input..MMBtu.,State.annual.net.generation..MWh.) %>% 
  mutate(input.output = as.numeric(State.total.annual.heat.input..MMBtu.) 
         / as.numeric(State.annual.net.generation..MWh.))
# Total combustion over noncombustion 
# Smaller numbers are better
combustion.noncumbustion <- power %>% 
  select(State.abbreviation, State.annual.total.combustion.net.generation..MWh., 
         State.annual.total.noncombustion.net.generation..MWh.) %>% 
  mutate(combustion.ratio = as.numeric(State.annual.total.combustion.net.generation..MWh.) /
           as.numeric(State.annual.total.noncombustion.net.generation..MWh.))
# Total renewables over nonrenewables
# Smaller numbers are better 
renewables.nonrenewables <- power %>% 
  select(State.abbreviation, State.annual.total.renewables.net.generation..MWh.,
         State.annual.total.nonrenewables.net.generation..MWh.) %>% 
  mutate(renewables.ratio = as.numeric(State.annual.total.renewables.net.generation..MWh.) /
           as.numeric(State.annual.total.nonrenewables.net.generation..MWh.))


# ------------------- #
# Tests Power
states <- power$State.abbreviation
emissions <- c("NOx", "SO2", "CO2", "CH4", "N20")

# add column for greatest source of fuel for each state
fuels <- power %>% mutate(Coal = as.numeric(gsub(",","",State.annual.coal.net.generation..MWh.)), 
                          Oil = as.numeric(gsub(",","",State.annual.oil.net.generation..MWh.)),
                          Gas = as.numeric(gsub(",","",State.annual.gas.net.generation..MWh.)),
                          Nuclear = as.numeric(gsub(",","",State.annual.nuclear.net.generation..MWh.)),
                          Hydro = as.numeric(gsub(",","",State.annual.hydro.net.generation..MWh.)),
                          Biomass = as.numeric(gsub(",","",State.annual.biomass.net.generation..MWh.)), 
                          Wind = as.numeric(gsub(",","",State.annual.wind.net.generation..MWh.)),
                          Solar = as.numeric(gsub(",","",State.annual.solar.net.generation..MWh.)), 
                          Geothermal = as.numeric(gsub(",","",State.annual.geothermal.net.generation..MWh.)), 
                          OtherFossil = as.numeric(gsub(",","",State.annual.other.fossil.net.generation..MWh.))) %>% 
  select(State.abbreviation, Coal, Oil, Gas, Nuclear, Hydro, Biomass, Wind, Solar, Geothermal, OtherFossil)
fuels[is.na(fuels)] <- " "
maxfuels <- as.data.frame(colnames(fuels)[apply(fuels,1,which.max)])
fuels <- bind_cols(fuels, maxfuels)


# Linear regression 
input_output_data <- power %>% select(State.total.annual.heat.input..MMBtu., State.annual.net.generation..MWh.)
input_output_data <- na.omit(input_output_data)

#power_input_output_linearregression <- lm(as.numeric(State.annual.net.generation..MWh.) 
#                                         ~ as.numeric(State.total.annual.heat.input..MMBtu.), 
#                                          data=input_output_data)
#print(power_input_output_linearregression)

# add column for most emitted chemical 
emissions <- power %>% mutate(NOx = as.numeric(gsub(",","",State.annual.NOx.emissions..tons.)),
                              SO2 = as.numeric(gsub(",","",State.annual.SO2.emissions..tons.)),
                              CO2 = as.numeric(gsub(",","",State.annual.CO2.emissions..tons.)),
                              CH4 = as.numeric(gsub(",","",State.annual.CH4.emissions..lbs.)), #convert to tons
                              N2O = as.numeric(gsub(",","",State.annual.N2O.emissions..lbs.)) #convert to tons
) %>% 
  select(State.abbreviation, NOx, SO2, CO2, CH4, N2O)
emissions[is.na(emissions)] <- " "
maxemissions <- as.data.frame(colnames(emissions)[apply(emissions,1,which.max)])
emissions <- bind_cols(emissions, maxemissions)

# ------------------- #
# Graphs Power

# emissions by state and emission type
state.emissiontype <- plot_ly(power, x = ~State.abbreviation, y = ~State.annual.NOx.emissions..tons., 
                              type = 'bar', name = 'NOx Emissions', 
                              marker = list(color = 'rgb(193, 0, 0)')) %>%
  add_trace(y = ~State.annual.SO2.emissions..tons., name = 'SO2 Emissions', 
            marker = list(color = 'rgb(249, 107, 107)')) %>%
  add_trace(y = ~State.annual.CO2.emissions..tons., name = 'CO2 Emissions', 
            marker = list(color = 'rgb(252, 117, 0)')) %>%
  add_trace(y = ~State.annual.CH4.emissions..lbs., name = 'CH4 Emissions', 
            marker = list(color = 'rgb(255, 199, 0)')) %>%
  add_trace(y = ~State.annual.N2O.emissions..lbs., name = 'N2O Emissions', 
            marker = list(color = 'rgb(255, 228, 132)')) %>%
  layout(yaxis = list(title = ""), xaxis = list(title = ""), barmode = 'stack', 
         title="Power Plant Emissions by State and Emission Type")
# generation by state and type
state.generationtype <- plot_ly(power, x = ~State.abbreviation, y = ~State.annual.coal.net.generation..MWh., 
                                type = 'bar', name = 'Coal',  marker = list(color = 'rgb(94, 0, 0)')) %>%
  add_trace( y = ~State.annual.oil.net.generation..MWh., name = 'Oil', 
             marker = list(color = 'rgb(193, 0, 0)')) %>%
  add_trace(y = ~State.annual.nuclear.net.generation..MWh., name = 'Nuclear', 
            marker = list(color = 'rgb(249, 107, 107)')) %>%
  add_trace(y = ~State.annual.gas.net.generation..MWh., name = 'Gas', 
            marker = list(color = 'rgb(255, 174, 104)')) %>%
  add_trace(y = ~State.annual.biomass.net.generation..MWh., name = 'Biomass',
            marker = list(color = 'rgb(252, 117, 0)')) %>%
  add_trace(y = ~State.annual.geothermal.net.generation..MWh., name = 'Geothermal', 
            marker = list(color = 'rgb(160, 74, 0)')) %>%
  add_trace(y = ~State.annual.hydro.net.generation..MWh., name = 'Hydro', 
            marker = list(color = 'rgb(160, 125, 0)')) %>%
  add_trace(y = ~State.annual.wind.net.generation..MWh., name = 'Wind', 
            marker = list(color = 'rgb(255, 199, 0)')) %>%
  add_trace(y = ~State.annual.solar.net.generation..MWh., name = 'Solar',
            marker = list(color = 'rgb(255, 228, 132)')) %>%
  add_trace(y = ~State.annual.other.fossil.net.generation..MWh., name = 'Other Fossil', 
            marker = list(color = 'rgb(155, 155, 155)')) %>% 
  layout(yaxis = list(title = ''), xaxis = list(title = ""), barmode = 'stack', 
         title = "Power Plant Generation by State and Energy Type")
# States and combustion vs non combustion
combustion.map <- choroplthFunc(combustion.noncumbustion, combustion.noncumbustion$combustion.ratio, 
                                combustion.noncumbustion$State.abbreviation, combustion.noncumbustion$combustion.ratio, 
                                "States and Combustion Ratio", c("Yellow", "Red"))
# States and renewables vs non renewables
renewables.map <- choroplthFunc(renewables.nonrenewables, renewables.nonrenewables$renewables.ratio, 
                                renewables.nonrenewables$State.abbreviation, renewables.nonrenewables$renewables.ratio, 
                                "States and Renewables Ratio", c("Yellow", "Red"))

# Power linear regression graph of heat input and output 
#power_lin_regression <- plot(as.numeric(input_output_data$State.total.annual.heat.input..MMBtu.), 
#                             as.numeric(input_output_data$State.annual.net.generation..MWh.))
#power_lin_regression <- abline(power_input_output_linearregression)


#fit <- lm(as.numeric(State.annual.net.generation..MWh.)
#          ~ as.numeric(State.total.annual.heat.input..MMBtu.),
#          data = input_output_data)
#plot_ly(input_output_data, x = ~State.total.annual.heat.input..MMBtu.) %>% 
#  add_markers(y = ~State.annual.net.generation..MWh.) %>% 
#  add_lines(x = ~State.total.annual.heat.input..MMBtu., y = fitted(fit))

# ------------------- #
# Summary Stats Water

# ------------------- #
# Tests Water

# ------------------- #
# Graphs Water

