

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
            total.reduction = sum(Current.Year.Emission.Reductions..MMTCO2e.yr....Direct, na.rm = TRUE)) 

#write.csv(landfill_by_state, file = "CABS.csv")
#casum <- landfill %>% 
#  filter(State == "CA") %>% 
#  filter(Waste.in.Place..tons. != "NA") %>% 
#  summarise(total = sum(as.numeric(Waste.in.Place..tons.)))
landfill_by_state <- read.csv("CABS.csv")
landfill_by_state <- landfill_by_state %>% 
filter(State != "PR")

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

landfill.withna <- landfill %>% 
  filter(is.na(Waste.in.Place..tons.)) %>% 
  group_by(State) %>% 
  summarise(Total.With.No.Waste = n())
landfill.withna <- left_join(landfill_by_state, landfill.withna, by = "State") 
landfill.withna <- select(landfill.withna, State, Total.With.No.Waste, num.landfills) %>% 
  mutate(percent = ( 1 - (Total.With.No.Waste / num.landfills)) * 100) %>% 
  arrange(percent)

landfill_by_state <- left_join(landfill_by_state, landfill.withna, by = "State")  
landfill_by_state <- landfill_by_state %>% 
  arrange(percent)
landfill_by_state$percent[46:50] <- 1
#new waste
total.waste.population <- landfill_by_state %>% 
  mutate(total.waste.pop = (total.waste * (100 - percent)) / X2017.Population) %>% 
  select(state.name, total.waste.pop) 
# Landfill Scores
# get waste/pop

# get waste scores
waste.score <- total.waste.population %>% 
  mutate(biggest = max(total.waste.pop)) %>% 
  mutate(waste.pop = 1 - (total.waste.pop / biggest))%>% 
  select(state.name, waste.pop)

# get lfg/pop
lfg.collected.population <- landfill_by_state %>% 
  mutate(lfg.collected.pop = total.lfg.collected / X2017.Population) %>% 
  select(state.name, lfg.collected.pop) 
# get lfg scores
lfg.score <- lfg.collected.population %>%
  mutate(biggest = max(lfg.collected.pop)) %>% 
  mutate(lfg.collected = lfg.collected.pop / biggest) %>% 
  select(state.name, lfg.collected)

# Power Scores
# get noncombust ratio
noncombust.total <- power %>% 
  mutate(total.combust = as.numeric(gsub(",","",power$State.annual.total.combustion.net.generation..MWh.))
         + as.numeric(gsub(",","",power$State.annual.total.noncombustion.net.generation..MWh.))) %>% 
  mutate(noncombust = as.numeric(gsub(",","",power$State.annual.total.noncombustion.net.generation..MWh.)) / total.combust) %>% 
  select(StateFull, noncombust) 
# get noncombust score
noncombust.score <- noncombust.total %>% 
  mutate(biggest = max(noncombust)) %>% 
  mutate(combustion = noncombust / biggest) %>% 
  select(StateFull, combustion)

# get renewables ratio
renewables.total <- power %>% 
  mutate(total.renewables = as.numeric(gsub(",","",power$State.annual.total.renewables.net.generation..MWh.))
         + as.numeric(gsub(",","",power$State.annual.total.nonrenewables.net.generation..MWh.))) %>% 
  mutate(renewables = as.numeric(gsub(",","",power$State.annual.total.renewables.net.generation..MWh.)) / total.renewables) %>% 
  select(StateFull, renewables) 
# get renewables score 
renewables.score <- renewables.total %>% 
  mutate(biggest = max(renewables)) %>% 
  mutate(renews = renewables / biggest) %>% 
  select(StateFull, renews)

# get total emissions/net generation
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
  mutate(emissions.total.gen = total / State.annual.net.generation..MWh.) %>% 
  select(StateFull, emissions.total.gen)
# get emissions score
emissions.score <- total.emissions %>% 
  mutate(biggest = max(emissions.total.gen)) %>% 
  mutate(emissions = 1 - (emissions.total.gen / biggest)) %>% 
  select(StateFull, emissions)

# Water Scores
# get water withdrawal/pop
water.withdrawals.by.pop <- slice(water, 1:50) %>% 
  transform( withdrawal.pop = as.numeric(Total) / as.numeric(Population.Total)) %>% 
  select(State, withdrawal.pop)
# get water scores
water.score <- water.withdrawals.by.pop %>% 
  mutate(biggest = max(withdrawal.pop)) %>% 
  mutate(withdrawals = 1 - (withdrawal.pop / biggest)) %>% 
  select(State, withdrawals)

all.data <- left_join(waste.score, lfg.score, by="state.name")
all.data <- left_join(all.data, noncombust.score, by = c("state.name" = "StateFull"))
all.data <- left_join(all.data,renewables.score, by = c("state.name" = "StateFull"))
all.data <- left_join(all.data,emissions.score, by = c("state.name" = "StateFull"))
all.data <- left_join(all.data,water.score, by = c("state.name" = "State"))
locations <- state.abb

# population 
all.data.pop <- all.data %>% 
  filter(state.name != "District of Columbia") %>% 
  mutate(totalScore = ((waste.pop * 0.16) +
           (lfg.collected * 0.13) +
           (emissions * 0.25) + 
           (renews * 0.14) +
           (combustion * 0.12) +
           (withdrawals * 0.2)))%>% 
  arrange(state.name) %>%
  mutate(loc = locations)  %>% 
  arrange(-totalScore)
all.data.pop$Rank <- seq.int(nrow(all.data.pop))
all.data.pop <- all.data.pop %>% 
  arrange(state.name)
overall.map <- choroplthFunc(all.data.pop, all.data.pop$totalScore, all.data.pop$loc, all.data.pop$totalScore, 
              "States Overall Impact Scores - Graph 1",c('red4', 'khaki1'), paste("Rank:", all.data.pop$Rank))


# ----------------------------------------------------------------------------------------------#
# summary stats
all.data.output <- all.data.pop %>% 
  mutate(waste.pop = paste0(round((waste.pop * 100), 2), "%")) %>% 
  mutate(lfg.collected = paste0(round((lfg.collected * 100), 2), "%")) %>% 
  mutate(combustion = paste0(round((combustion * 100), 2), "%")) %>% 
  mutate(renews = paste0(round((renews * 100), 2), "%")) %>% 
  mutate(emissions = paste0(round((emissions * 100), 2), "%")) %>% 
  mutate(withdrawals = paste0(round((withdrawals * 100), 2), "%")) %>% 
  arrange(-totalScore) %>% 
  mutate(totalScore = paste0(round((totalScore * 100), 2), "%"))  
all.data.output$Rank <- seq.int(nrow(all.data.output))
all.data.output <- all.data.output %>% 
  select(-loc) %>% 
  arrange(state.name)

names(all.data.output)[1]<-paste("State")
names(all.data.output)[2]<-paste("Waste")
names(all.data.output)[3]<-paste("LFG")
names(all.data.output)[4]<-paste("Noncombustables")
names(all.data.output)[5]<-paste("Renewables")
names(all.data.output)[6]<-paste("Emissions")
names(all.data.output)[7]<-paste("Withdrawals")
names(all.data.output)[8]<-paste("Score")

# get sum stats and combine
emissions.sum.stats <- summarise(total.emissions, variable = "Emissions", 
                                 mean = mean(emissions.total.gen),
                                 median = median(emissions.total.gen))
water.sum.stats <- summarise(water.withdrawals.by.pop, variable = "Water Withdrawals", 
                             mean = mean(withdrawal.pop),
                             median = median(withdrawal.pop))
waste.sum.stats <- summarise(total.waste.population, variable = "Waste",
                             mean = mean(total.waste.pop),
                             median = median(total.waste.pop))
renewables.sum.stats <- summarise(renewables.total , variable = "Renewables",
                                  mean = mean(renewables),
                                  median = median(renewables))
lfg.sum.stats <- summarise(lfg.collected.population, variable = "LFG",
                           mean = mean(lfg.collected.pop),
                           median = median(lfg.collected.pop))
noncombust.sum.stats <- summarise(noncombust.total, variable = "Noncombustible",
                                  mean = mean(noncombust),
                                  median = median(noncombust))
sum.stats <- rbind(emissions.sum.stats, water.sum.stats, waste.sum.stats, renewables.sum.stats,
                   lfg.sum.stats, noncombust.sum.stats)


# distributions
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
distribution.plot <- 
  plot_ly(all.data.pop, x = ~state.name, y = ~emissions, name = 'Emissions', type = 'scatter', mode = 'markers',
          text = ~state.name, marker = list(color = 'rgb(10, 36, 99)')) %>%
  add_trace(y = ~withdrawals, name = 'Withdrawals', mode = 'markers', marker = list(color = 'rgb(63, 136, 197)')) %>%
  add_trace(y = ~waste.pop, name = 'Waste', mode = 'markers', marker = list(color = 'rgb(232, 151, 44)')) %>%
  add_trace(y = ~lfg.collected, name = 'LFG Collection', mode = 'markers', marker = list(color = 'rgb(239, 219, 127)')) %>%
  add_trace(y = ~renews, name = 'Renewable Power', mode = 'markers', marker = list(color = 'rgb(215, 38, 56)')) %>%
  add_trace(y = ~combustion, name = 'Noncombustables', mode = 'markers', marker = list(color = 'rgb(148, 16, 32)'))  %>% 
  layout(autosize = F, width = 930, height = 500, margin = m, xaxis = list(title = "", tickfont = list(size = 10)), 
         yaxis = list(title = "score"), font = list(family = "times"), title = "Plot of Scores by State and Score Type - Graph 2")

emissions.dist <- 
  plot_ly(all.data.pop, x = ~state.name, y = ~emissions, name = 'Emissions', type = 'scatter', mode = 'lines',
          text = ~state.name)
withdrawals.dist <- 
  plot_ly(all.data.pop, y = ~withdrawals, name = 'Withdrawals', mode = 'lines', type = 'scatter',
          text = ~state.name)
waste.dist <- 
  plot_ly(all.data.pop, y = ~waste.pop, name = 'Waste', mode = 'lines', type = 'scatter',
          text = ~state.name)
lfg.dist <- 
  plot_ly(all.data.pop, y = ~lfg.collected, name = 'LFG Collection', mode = 'lines', type = 'scatter',
          text = ~state.name)
renewables.dist <- 
  plot_ly(all.data.pop, y = ~renews, name = 'Renewable Power', mode = 'lines', type = 'scatter',
          text = ~state.name)
combustion.dist <- 
  plot_ly(all.data.pop, y = ~combustion, name = 'Noncombustables', mode = 'lines', type = 'scatter',
          text = ~state.name)





# ----------------------------------------------------------------------------------------------#
# Discussion

getRanks <- function(StateName){
  all.data <- arrange(all.data.pop, waste.pop)
  waste.pop <- which(grepl(StateName, all.data$state.name))
  all.data <- arrange(all.data.pop, lfg.collected)
  lfg.collected <- which(grepl(StateName, all.data$state.name))
  all.data <- arrange(all.data.pop, emissions)
  emissions <- which(grepl(StateName, all.data$state.name))
  all.data <- arrange(all.data.pop, renews)
  renews <- which(grepl(StateName, all.data$state.name))
  all.data <- arrange(all.data.pop, combustion)
  combustion <- which(grepl(StateName, all.data$state.name))
  all.data <- arrange(all.data.pop, withdrawals)
  withdrawals <-  which(grepl(StateName, all.data$state.name))
  
  ranks <- list("Total Waste" = 51 - waste.pop, "LFG Collection" = 51 - lfg.collected,
                   "Emissions" = 51 - emissions, "Renewable Energy" = 51 - renews,
                   "Combustables" = 51 - combustion, "Water Withdrawals" = 51 - withdrawals)
  return(ranks)
}

wy.ranks <- as.data.frame(getRanks("Wyoming"))
wa.ranks <- as.data.frame(getRanks("Washington"))
ca.ranks <- as.data.frame(getRanks("California"))


wy.landfill <- landfill_by_state %>% 
  mutate(num.without = num.landfills.x - total.with.lfg) %>% 
  mutate(percent.without = paste0(round(((num.without / num.landfills.x) * 100), 2), "%")) %>% 
  select(State, num.without, percent.without) %>% 
  arrange(num.without)


#new waste
total.waste.population2 <- landfill_by_state %>% 
  mutate(total.waste.pop = total.waste / X2017.Population) %>% 
  select(state.name, total.waste.pop) 
# get waste scores
waste.score2 <- total.waste.population2 %>% 
  mutate(biggest = max(total.waste.pop)) %>% 
  mutate(waste.pop = 1 - (total.waste.pop / biggest))%>% 
  select(state.name, waste.pop)

all.data2 <- left_join(waste.score2, lfg.score, by="state.name")
all.data2 <- left_join(all.data2, noncombust.score, by = c("state.name" = "StateFull"))
all.data2 <- left_join(all.data2,renewables.score, by = c("state.name" = "StateFull"))
all.data2 <- left_join(all.data2,emissions.score, by = c("state.name" = "StateFull"))
all.data2 <- left_join(all.data2,water.score, by = c("state.name" = "State"))
locations <- state.abb

# population 
all.data.pop2 <- all.data2 %>% 
  filter(state.name != "District of Columbia") %>% 
  mutate(totalScore = ((waste.pop * 0.16) +
                         (lfg.collected * 0.13) +
                         (emissions * 0.25) + 
                         (renews * 0.14) +
                         (combustion * 0.12) +
                         (withdrawals * 0.2))) %>% 
  arrange(state.name) %>% 
  mutate(loc = locations)
overall.map2 <- choroplthFunc(all.data.pop2, all.data.pop2$totalScore, all.data.pop2$loc, all.data.pop2$totalScore, 
                             "States Overall Impact Scores", c('red4', 'khaki1'), paste("Rank"))

getRanks2 <- function(StateName){
  all.data2 <- arrange(all.data2, waste.pop)
  waste.pop <- which(grepl(StateName, all.data2$state.name))
  all.data2 <- arrange(all.data2, lfg.collected)
  lfg.collected <- which(grepl(StateName, all.data2$state.name))
  all.data2 <- arrange(all.data2, emissions)
  emissions <- which(grepl(StateName, all.data2$state.name))
  all.data2 <- arrange(all.data2, renews)
  renews <- which(grepl(StateName, all.data2$state.name))
  all.data2 <- arrange(all.data2, combustion)
  combustion <- which(grepl(StateName, all.data2$state.name))
  all.data2 <- arrange(all.data2, withdrawals)
  withdrawals <-  which(grepl(StateName, all.data2$state.name))
  
  ranks <- list("Total Waste" = 51 - waste.pop, "LFG Collection" = 51 - lfg.collected,
                "Emissions" = 51 - emissions, "Renewable Energy" = 51 - renews,
                "Combustables" = 51 - combustion, "Water Withdrawals" = 51 - withdrawals)
  return(ranks)
}
ca.ranks2 <- as.data.frame(getRanks2("California"))

